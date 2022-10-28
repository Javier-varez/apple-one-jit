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

pub struct JitPage<'a> {
    allocation: Allocation,
    opcode_stream: OpCodeStream<'a>,
}

impl<'a> JitPage<'a> {
    pub fn allocate(size: usize) -> Result<Self, region::Error> {
        let mut allocation = alloc(size, Protection::READ_WRITE)?;

        let ptr = allocation.as_mut_ptr::<MaybeUninit<OpCode>>();
        let size = allocation.len() / std::mem::size_of::<MaybeUninit<OpCode>>();
        let slice = unsafe { std::slice::from_raw_parts_mut(ptr, size) };

        Ok(Self {
            allocation,
            opcode_stream: OpCodeStream::new(slice),
        })
    }

    pub fn populate<T: Fn(&mut OpCodeStream)>(&mut self, callable: T) {
        callable(&mut self.opcode_stream);
    }

    /// Maps the pages as READ_EXECUTE and then runs the callable, passing a pointer to the
    /// beginning of the page.
    ///
    /// # Safety
    /// The user must guarantee that the region contains valid code at this point
    pub unsafe fn run<U, T: Fn(*const ()) -> U>(&mut self, callable: T) -> U {
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
        And, Branch, Condition, MovShift, Movk, Movn, Movz, Or, RegShift, SignedImmediate, Sub, Xor,
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
}
