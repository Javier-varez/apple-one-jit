use std::mem::MaybeUninit;

use crate::arm_asm::{Add, Immediate, OpCode, Register, Ret, Udf};
use region::{alloc, page, protect, Allocation, Protection};

use libc::{sigaction, SIGILL};

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

    pub fn push_opcode(&mut self, opcode: OpCode) -> usize {
        let idx = self.index;
        self.data[idx].write(opcode);
        self.index = idx + 1;
        idx
    }

    pub fn add_marker(&mut self) -> usize {
        let marker = self.index;
        self.push_opcode(Udf::new().generate());
        marker
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
    use crate::arm_asm::{RegShift, Sub};
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
}
