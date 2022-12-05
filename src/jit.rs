use std::mem::MaybeUninit;

use crate::arm_asm::{Add, Immediate, OpCode, Register, Ret, Udf};
use region::{alloc, page, protect, Allocation, Protection};

use libc::{sigaction, SIGILL};

#[derive(Clone)]
pub struct Marker {
    index: i64,
}

pub struct OpCodeStream<'a> {
    data: &'a mut [MaybeUninit<OpCode>],
    index: usize,
}

impl<'a> OpCodeStream<'a> {
    pub fn new(data: &'a mut [MaybeUninit<OpCode>]) -> Self {
        for op in data.iter_mut() {
            op.write(Udf::new().generate());
        }
        Self { data, index: 0 }
    }

    pub fn push_opcode(&mut self, opcode: OpCode) -> Marker {
        let index = self.index;
        self.data[index].write(opcode);
        self.index = index + 1;
        Marker {
            index: index as i64,
        }
    }

    pub fn add_marker(&mut self) -> Marker {
        let index = self.index;
        self.push_opcode(Udf::new().generate());
        Marker {
            index: index as i64,
        }
    }

    pub fn relative_distance(&self, marker_a: &Marker, marker_b: &Marker) -> i64 {
        marker_a.index - marker_b.index
    }

    pub fn marker_address(&self, marker: &Marker) -> *const () {
        &self.data[marker.index as usize] as *const _ as *const _
    }

    pub fn patch_opcode(&mut self, marker: &Marker, opcode: OpCode) {
        self.data[marker.index as usize].write(opcode);
    }
}

pub struct JitPage {
    allocation: Allocation,
}

impl JitPage {
    pub fn allocate(size: usize) -> Result<Self, region::Error> {
        let allocation = alloc(size, Protection::READ_WRITE)?;
        Ok(Self { allocation })
    }

    pub fn populate<U, T: FnMut(&mut OpCodeStream) -> U>(&mut self, mut callable: T) -> U {
        let ptr = self.allocation.as_mut_ptr::<MaybeUninit<OpCode>>();
        let size = self.allocation.len() / std::mem::size_of::<MaybeUninit<OpCode>>();
        let slice = unsafe { std::slice::from_raw_parts_mut(ptr, size) };
        let mut opcode_stream = OpCodeStream::new(slice);
        let val = callable(&mut opcode_stream);

        // Invalidate the instruction cache at this virtual address (for this page) because the
        // code in this page has changed
        unsafe { core::arch::asm!("ic ivau, {va}", va = in(reg) self.allocation.as_ptr::<()>()) };
        // Ensure the instruction finishes
        unsafe { core::arch::asm!("dsb ish") };
        // Then flush the instruction stream
        unsafe { core::arch::asm!("isb") };

        val
    }

    /// Maps the pages as READ_EXECUTE and then runs the callable, passing a pointer to the
    /// beginning of the page.
    ///
    /// # Safety
    /// The user must guarantee that the region contains valid code at this point
    pub unsafe fn run<U, T: FnMut(*const ()) -> U>(&mut self, mut callable: T) -> U {
        protect(
            self.allocation.as_mut_ptr::<()>(),
            self.allocation.len(),
            Protection::READ_EXECUTE,
        )
        .unwrap();

        let ptr: *const () = self.allocation.as_ptr();
        let result = callable(ptr);

        protect(
            self.allocation.as_mut_ptr::<()>(),
            self.allocation.len(),
            Protection::READ_WRITE,
        )
        .unwrap();

        result
    }
}

macro_rules! invoke {
    ($jit:expr, $fn_type:ty $(, $args:expr)*) => {
        $jit.run(|ptr: *const _| {
            let func: $fn_type = std::mem::transmute_copy(&ptr);
            func($($args),*)
        })
    };
}

unsafe fn handler(signal: libc::c_int, siginfo: *const libc::siginfo_t, _: *const libc::c_void) {
    let siginfo = &*siginfo;
    println!("Handled signal: {}!", signal);
    println!("Address: {:?}", siginfo.si_addr());
    libc::exit(libc::EXIT_FAILURE);
}

unsafe fn register_sigill_handler() {
    let fn_ptr: unsafe fn(libc::c_int, *const libc::siginfo_t, *const libc::c_void) = handler;
    let action: libc::sighandler_t = std::mem::transmute_copy(&fn_ptr);
    let act = sigaction {
        sa_mask: 0,
        sa_flags: 0,
        sa_sigaction: action,
    };

    let result = sigaction(SIGILL, &act as *const _, std::ptr::null_mut());
    if result < 0 {
        panic!("Error registering hander: {}", result);
    }
}

pub fn run_demo() {
    unsafe { register_sigill_handler() };

    let mut jit_page = JitPage::allocate(page::size()).unwrap();

    jit_page.populate(|opcode_stream| {
        opcode_stream.push_opcode(
            Add::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(24))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
    });

    let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 10) };
    println!("Value {}", val);
}

#[cfg(all(test, target_arch = "aarch64"))]
mod test {
    use super::*;
    use crate::arm_asm::{
        Adc, And, Branch, Condition, Ldrd, MemoryAccessMode, Mov, MovShift, Movk, Movn, Movz, Mrs,
        Msr, OpSize, Or, RegShift, Sbc, SetF8, SignedImmediate, Strd, Sub, Sxtb, Xor, NZCV,
    };
    use region::page;

    #[test]
    fn test_add_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Add::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(24))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 10) };
        assert_eq!(val, 34);
    }

    #[test]
    fn test_add_shifted_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Add::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(0x12))
                    .shifted()
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 0x123) };
        assert_eq!(val, 0x12123);
    }

    #[test]
    fn test_sub_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Sub::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(24))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 34) };
        assert_eq!(val, 10);
    }

    #[test]
    fn test_sub_shifted_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Sub::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(0x1))
                    .shifted()
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 0x1234) };
        assert_eq!(val, 0x234);
    }

    #[test]
    fn test_add_register_not_shifted() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Add::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 0x1234, 0x10001) };
        assert_eq!(val, 0x11235);
    }

    #[test]
    fn test_sub_register_not_shifted() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Sub::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 1234, 1230) };
        assert_eq!(val, 4);
    }

    #[test]
    fn test_add_register_shifted_left() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Add::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .with_shift(RegShift::Lsl(8))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 0x12, 0x23) };
        assert_eq!(val, 0x2312);
    }

    #[test]
    fn test_sub_register_shifted_left() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Sub::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .with_shift(RegShift::Lsl(8))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 0x2333, 0x23) };
        assert_eq!(val, 0x33);
    }

    #[test]
    fn test_or_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Or::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(0x1111_1111_1111_1111u64))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 0xAAAA_0000_FFFF_5555) };
        assert_eq!(val, 0xBBBB_1111_FFFF_5555);
    }

    #[test]
    fn test_xor_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Xor::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(0xdddd_dddd_dddd_ddddu64))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 0xAAAA_0000_FFFF_5555) };
        assert_eq!(val, 0x7777_dddd_2222_8888);
    }

    #[test]
    fn test_and_immediate() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                And::new(Register::X0, Register::X0)
                    .with_immediate(Immediate::new(0xff00_ff00_ff00_ff00u64))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 0xAAAA_0000_FFFF_5555) };
        assert_eq!(val, 0xaa00_0000_ff00_5500);
    }

    #[test]
    fn test_or_register() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Or::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .with_shift(RegShift::Lsl(8))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(u64, u64) -> u64,
                0xAAAA_0000_FFFF_5555,
                0x5555_0000_AAAA_FFFF
            )
        };
        assert_eq!(val, 0xFFAA_00AA_FFFF_FF55);
    }

    #[test]
    fn test_and_register() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                And::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .with_shift(RegShift::Lsl(8))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(u64, u64) -> u64,
                0xAAAA_0000_FFFF_5555,
                0x5555_0000_AAAA_FFFF
            )
        };
        assert_eq!(val, 0x0000_0000_AAFF_5500);
    }

    #[test]
    fn test_xor_register() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Xor::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X1)
                    .with_shift(RegShift::Lsl(8))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(u64, u64) -> u64,
                0xAAAA_0000_FFFF_5555,
                0x5555_0000_AAAA_FFFF
            )
        };
        assert_eq!(val, 0xFFAA_00AA_5500_AA55);
    }

    #[test]
    fn test_movz() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Movz::new(Register::X0)
                    .with_immediate(Immediate::new(0x1234))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn() -> u64) };
        assert_eq!(val, 0x1234);
    }

    #[test]
    fn test_movk() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Movz::new(Register::X0)
                    .with_immediate(Immediate::new(0x1234))
                    .generate(),
            );
            opcode_stream.push_opcode(
                Movk::new(Register::X0)
                    .with_immediate(Immediate::new(0x5678))
                    .with_shift(MovShift::Bits16)
                    .generate(),
            );
            opcode_stream.push_opcode(
                Movk::new(Register::X0)
                    .with_immediate(Immediate::new(0xFFFF))
                    .with_shift(MovShift::Bits32)
                    .generate(),
            );
            opcode_stream.push_opcode(
                Movk::new(Register::X0)
                    .with_immediate(Immediate::new(0x55AA))
                    .with_shift(MovShift::Bits48)
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn() -> u64) };
        assert_eq!(val, 0x55AAFFFF56781234);
    }

    #[test]
    fn test_movn() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Movn::new(Register::X0)
                    .with_immediate(Immediate::new(0xAA55))
                    .with_shift(MovShift::Bits0)
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn() -> u64) };
        assert_eq!(val, 0xFFFFFFFFFFFF55AA);
    }

    #[test]
    fn test_mov_register() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(Mov::new(Register::X0, Register::X1).generate());
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 123, 234) };
        assert_eq!(val, 234);
    }

    #[test]
    fn test_branch() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Movz::new(Register::X0)
                    .with_immediate(Immediate::new(0xFFFF))
                    .with_shift(MovShift::Bits0)
                    .generate(),
            );

            let source_marker = opcode_stream.add_marker();
            opcode_stream.push_opcode(
                Movz::new(Register::X0)
                    .with_immediate(Immediate::new(0xAA55))
                    .with_shift(MovShift::Bits0)
                    .generate(),
            );
            let target_marker = opcode_stream.push_opcode(Ret::new().generate());

            opcode_stream.patch_opcode(
                &source_marker,
                Branch::new()
                    .with_immediate(SignedImmediate::new(
                        opcode_stream.relative_distance(&target_marker, &source_marker),
                    ))
                    .generate(),
            );
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn() -> u64) };
        assert_eq!(val, 0xFFFF);
    }

    #[test]
    fn test_conditional_branch() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Add::new(Register::X2, Register::X0)
                    .with_immediate(Immediate::new(0))
                    .generate(),
            );
            opcode_stream.push_opcode(
                Movz::new(Register::X0)
                    .with_immediate(Immediate::new(0))
                    .generate(),
            );

            // Check condition
            opcode_stream.push_opcode(
                Add::new(Register::X1, Register::X1)
                    .with_immediate(Immediate::new(0))
                    .update_flags()
                    .generate(),
            );
            let branch_forward_marker = opcode_stream.add_marker();

            opcode_stream.push_opcode(
                Add::new(Register::X0, Register::X0)
                    .with_shifted_reg(Register::X2)
                    .generate(),
            );
            opcode_stream.push_opcode(
                Sub::new(Register::X1, Register::X1)
                    .with_immediate(Immediate::new(1))
                    .update_flags()
                    .generate(),
            );
            let branch_back_marker = opcode_stream.add_marker();
            let offset =
                opcode_stream.relative_distance(&branch_forward_marker, &branch_back_marker);
            opcode_stream.patch_opcode(
                &branch_back_marker,
                Branch::new()
                    .with_immediate(SignedImmediate::new(offset))
                    .generate(),
            );

            // Patching opcode
            let target_jump = opcode_stream.push_opcode(Ret::new().generate());
            let offset = opcode_stream.relative_distance(&target_jump, &branch_forward_marker);
            opcode_stream.patch_opcode(
                &branch_forward_marker,
                Branch::new()
                    .with_immediate(SignedImmediate::new(offset))
                    .iff(Condition::Eq)
                    .generate(),
            );
        });

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 4, 4) };
        assert_eq!(val, 16);

        let val = unsafe { invoke!(jit_page, extern "C" fn(u64, u64) -> u64, 128, 128) };
        assert_eq!(val, 16384);
    }

    #[test]
    fn test_str_instruction() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Store the value in X0 in [X1]
            opcode_stream.push_opcode(
                Strd::new(Register::X0, Register::X1)
                    .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(0)))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let input = 0x1234_5678_9012_3456u64;
        let mut output = 0u64;

        unsafe {
            invoke!(
                jit_page,
                extern "C" fn(u64, *mut u64),
                input,
                &mut output as *mut u64
            )
        };

        assert_eq!(output, input);
    }

    #[test]
    fn test_ldr_instruction() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Store the address in X0 in [X0]
            opcode_stream.push_opcode(
                Ldrd::new(Register::X0, Register::X0)
                    .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(0)))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let input = 0x1234_5678_9012_3456u64;
        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(*const u64) -> u64,
                &input as *const u64
            )
        };
        assert_eq!(val, 0x1234_5678_9012_3456u64);
    }

    #[test]
    fn test_str_with_imm_offset_instruction() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Store the value in X0 in [X1]
            opcode_stream.push_opcode(
                Strd::new(Register::X0, Register::X1)
                    .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(1)))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let input = 0x1234_5678_9012_3456u64;
        let mut output = [0u64; 2];

        unsafe {
            invoke!(
                jit_page,
                extern "C" fn(u64, *mut u64),
                input,
                &mut output[0] as *mut u64
            )
        };

        assert_eq!(output[0], 0);
        assert_eq!(output[1], input);
    }

    #[test]
    fn test_ldr_with_imm_offset_instruction() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Store the value in X0 in [X1]
            opcode_stream.push_opcode(
                Ldrd::new(Register::X0, Register::X0)
                    .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(1)))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let input = [0u64, 0x1234_5678_9012_3456u64];

        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(*const u64) -> u64,
                &input[0] as *const u64
            )
        };

        assert_eq!(val, 0x1234_5678_9012_3456u64);
    }

    #[test]
    fn test_ldr_with_register_offset_instr() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Store the value in X0 in [X1]
            opcode_stream.push_opcode(
                Ldrd::new(Register::X0, Register::X0)
                    .with_mode(MemoryAccessMode::ShiftedRegister(Register::X1))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let input = [123u64, 0x1234_5678_9012_3456u64];

        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(*const u64, u64) -> u64,
                &input[0] as *const u64,
                0
            )
        };

        assert_eq!(val, 123);

        let val = unsafe {
            invoke!(
                jit_page,
                extern "C" fn(*const u64, u64) -> u64,
                &input[0] as *const u64,
                8
            )
        };

        assert_eq!(val, 0x1234_5678_9012_3456u64);
    }

    #[test]
    fn test_str_with_register_offset_instr() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Store the value in X0 in [X1]
            opcode_stream.push_opcode(
                Strd::new(Register::X2, Register::X0)
                    .with_mode(MemoryAccessMode::ShiftedRegister(Register::X1))
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let mut output = [0u64, 0u64];

        unsafe {
            invoke!(
                jit_page,
                extern "C" fn(*mut u64, usize, u64),
                &mut output[0] as *mut u64,
                0,
                0x1234_5678_9012_3456u64
            )
        };

        assert_eq!(output[0], 0x1234_5678_9012_3456);
        assert_eq!(output[1], 0);

        unsafe {
            invoke!(
                jit_page,
                extern "C" fn(*mut u64, usize, u64),
                &mut output[0] as *mut u64,
                8,
                0x1234_5678_9012_3456u64
            )
        };
        assert_eq!(output[0], 0x1234_5678_9012_3456);
        assert_eq!(output[1], 0x1234_5678_9012_3456);
    }

    #[test]
    fn test_sign_extend() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            // Sign extend byte on x0
            opcode_stream.push_opcode(Sxtb::new(Register::X0, Register::X0).generate());
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let value = unsafe { invoke!(jit_page, extern "C" fn(u8) -> u32, 0x84) };
        assert_eq!(value, 0xffffff84);
        let value = unsafe { invoke!(jit_page, extern "C" fn(u8) -> u32, 0x73) };
        assert_eq!(value, 0x73);
    }

    #[test]
    fn test_set_get_flags() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(Msr::new(NZCV, Register::X0).generate());
            opcode_stream.push_opcode(Mrs::new(Register::X0, NZCV).generate());
            opcode_stream.push_opcode(Ret::new().generate());
        });

        let value = unsafe { invoke!(jit_page, extern "C" fn(u64) -> u64, 0xFFFFFFFFFFFFFFF) };
        assert_eq!(value, 0xF0000000);

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(
                Movz::new(Register::X1)
                    .with_immediate(Immediate::new(0x00))
                    .generate(),
            );
            opcode_stream.push_opcode(Msr::new(NZCV, Register::X1).generate());
            opcode_stream.push_opcode(SetF8::new(Register::X0).generate());
            opcode_stream.push_opcode(Mrs::new(Register::X0, NZCV).generate());
            opcode_stream.push_opcode(Ret::new().generate());
        });

        const N: u64 = 0x80000000;
        const Z: u64 = 0x40000000;
        const V: u64 = 0x10000000;
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u32) -> u64, 0) },
            Z
        );
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u32) -> u64, 0x80) },
            V | N
        );
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u32) -> u64, 0xffffff80) },
            N
        );
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u32) -> u64, 0xffffff7f) },
            V
        );
    }

    #[test]
    fn test_add_with_carry() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
            opcode_stream
                .push_opcode(Adc::new(Register::X0, Register::X0, Register::X1).generate());
            opcode_stream.push_opcode(Ret::new().generate());
        });

        const C: u64 = 0x20000000;
        const NO_FLAGS: u64 = 0x00000000;
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u64, u64, u64) -> u64, 12, 4, C) },
            17
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    12,
                    4,
                    NO_FLAGS
                )
            },
            16
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0x7FFF_FFFF_FFFF_FFFF,
                    4,
                    C
                )
            },
            0x8000_0000_0000_0004
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0xFFFF_FFFF_FFFF_FFFF,
                    4,
                    C
                )
            },
            0x0000_0000_0000_0004
        );
    }

    #[test]
    fn test_add_with_carry_32_bit() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
            opcode_stream.push_opcode(
                Adc::new(Register::X0, Register::X0, Register::X1)
                    .with_op_size(OpSize::Size32)
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        const C: u64 = 0x20000000;
        const NO_FLAGS: u64 = 0x00000000;
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u64, u64, u64) -> u64, 12, 4, C) },
            17
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    12,
                    4,
                    NO_FLAGS
                )
            },
            16
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0x7FFF_FFFF,
                    4,
                    C
                )
            },
            0x8000_0004
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0xFFFF_FFFF,
                    4,
                    C
                )
            },
            0x0000_0004
        );
    }

    #[test]
    fn test_sub_with_borrow() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
            opcode_stream
                .push_opcode(Sbc::new(Register::X0, Register::X0, Register::X1).generate());
            opcode_stream.push_opcode(Ret::new().generate());
        });

        const BORROW: u64 = 0x00000000;
        const NO_BORROW: u64 = 0x20000000;
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u64, u64, u64) -> u64, 12, 4, BORROW) },
            7
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    12,
                    4,
                    NO_BORROW
                )
            },
            8
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    12,
                    12,
                    BORROW
                )
            },
            0xFFFF_FFFF_FFFF_FFFF
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    11,
                    12,
                    NO_BORROW
                )
            },
            0xFFFF_FFFF_FFFF_FFFF
        );
    }

    #[test]
    fn test_sub_with_borrow_32_bit() {
        let mut jit_page = JitPage::allocate(page::size()).unwrap();

        jit_page.populate(|opcode_stream| {
            opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
            opcode_stream.push_opcode(
                Sbc::new(Register::X0, Register::X0, Register::X1)
                    .with_op_size(OpSize::Size32)
                    .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
        });

        const BORROW: u64 = 0x00000000;
        const NO_BORROW: u64 = 0x20000000;
        assert_eq!(
            unsafe { invoke!(jit_page, extern "C" fn(u64, u64, u64) -> u64, 12, 4, BORROW) },
            7
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    12,
                    4,
                    NO_BORROW
                )
            },
            8
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    12,
                    12,
                    BORROW
                )
            },
            0xFFFF_FFFF
        );

        assert_eq!(
            unsafe {
                invoke!(
                    jit_page,
                    extern "C" fn(u64, u64, u64) -> u64,
                    11,
                    12,
                    NO_BORROW
                )
            },
            0xFFFF_FFFF
        );
    }
}
