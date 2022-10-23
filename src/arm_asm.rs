/// This module provides functions to dynamically assemble armv8-a machine code.

pub struct OpCode(u32);
impl OpCode {
    pub fn value(&self) -> u32 {
        self.0
    }
}

pub struct Immediate(u32);

impl Immediate {
    pub fn new(val: u32) -> Self {
        Self(val)
    }
}

pub enum Register {
    X0 = 0,
    X1 = 1,
    X2 = 2,
    X3 = 3,
    X4 = 4,
    X5 = 5,
    X6 = 6,
    X7 = 7,
    X8 = 8,
    X9 = 9,
    X10 = 10,
    X11 = 11,
    X12 = 12,
    X13 = 13,
    X14 = 14,
    X15 = 15,
    X16 = 16,
    X17 = 17,
    X18 = 18,
    X19 = 19,
    X20 = 20,
    X21 = 21,
    X22 = 22,
    X23 = 23,
    X24 = 24,
    X25 = 25,
    X26 = 26,
    X27 = 27,
    X28 = 28,
    X29 = 29,
    X30 = 30,
    SP = 31,
}

mod udf {
    use super::*;

    pub struct Udf();

    impl Udf {
        pub fn new() -> Self {
            Self()
        }

        pub fn generate(self) -> OpCode {
            OpCode(0)
        }
    }
}

pub use udf::Udf;

mod ret {
    use super::*;
    pub struct Ret(Register);

    impl Ret {
        pub fn new() -> Self {
            Self(Register::X30)
        }

        pub fn set_register(mut self, register: Register) -> Self {
            self.0 = register;
            self
        }

        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0xD65F0000;
            const REG_OFFSET: usize = 5;
            let reg = self.0 as u32;
            OpCode(OPCODE_BASE | (reg << REG_OFFSET))
        }
    }
}

pub use ret::Ret;

mod branch {
    use super::*;

    enum BranchTarget {
        Register(Register),
        Immediate(Immediate),
    }

    pub struct Branch {
        branch_target: Option<BranchTarget>,
        link: bool,
    }

    impl Branch {
        pub fn new() -> Self {
            Self {
                branch_target: None,
                link: false,
            }
        }

        pub fn register(mut self, reg: Register) -> Self {
            self.branch_target = Some(BranchTarget::Register(reg));
            self
        }

        pub fn pc_relative(mut self, imm: Immediate) -> Self {
            self.branch_target = Some(BranchTarget::Immediate(imm));
            self
        }

        pub fn link(mut self) -> Self {
            self.link = true;
            self
        }

        pub fn generate(self) -> OpCode {
            unimplemented!();
        }
    }
}

pub use branch::Branch;

pub mod add_sub {
    use super::*;

    pub(super) enum AddOrSub {
        Add,
        Sub,
    }

    pub struct AddSubImmediate {
        pub(super) source_reg: Register,
        pub(super) dest_reg: Register,
        pub(super) immediate: Immediate,
        pub(super) shift: bool,
        pub(super) update_flags: bool,
        pub(super) add_or_sub: AddOrSub,
    }

    impl AddSubImmediate {
        pub(super) fn new(
            source_reg: Register,
            dest_reg: Register,
            imm: Immediate,
            add_or_sub: AddOrSub,
        ) -> Self {
            if imm.0 > 1u32 << 12 {
                panic!("Immediate is out of range");
            }
            Self {
                source_reg,
                dest_reg,
                immediate: imm,
                shift: false,
                update_flags: false,
                add_or_sub,
            }
        }
        /// Shifts the immediate another 12 bits.
        pub fn shifted(mut self) -> Self {
            self.shift = true;
            self
        }

        /// Shifts the immediate another 12 bits.
        pub fn update_flags(mut self) -> Self {
            self.update_flags = true;
            self
        }

        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0x91000000;
            const IMM_OFFSET: usize = 10;
            const SRC_REG_OFFSET: usize = 5;
            const DST_REG_OFFSET: usize = 0;
            let source_reg = (self.source_reg as u32) << SRC_REG_OFFSET;
            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let shift = if self.shift { 1 << 22 } else { 0 };
            let update_flags = if self.update_flags { 1 << 29 } else { 0 };
            let imm = (self.immediate.0 as u32) << IMM_OFFSET;
            let sub = match self.add_or_sub {
                AddOrSub::Add => 0,
                AddOrSub::Sub => 1 << 30,
            };
            OpCode(OPCODE_BASE | source_reg | dest_reg | shift | imm | update_flags | sub)
        }
    }
}

mod add {
    use super::*;
    pub struct Add {
        source_reg: Register,
        dest_reg: Register,
    }

    impl Add {
        pub fn new(dest_reg: Register, source_reg: Register) -> Self {
            Self {
                source_reg,
                dest_reg,
            }
        }

        pub fn with_immediate(self, imm: Immediate) -> super::add_sub::AddSubImmediate {
            super::add_sub::AddSubImmediate::new(
                self.source_reg,
                self.dest_reg,
                imm,
                super::add_sub::AddOrSub::Add,
            )
        }
    }
}

pub use add::Add;

mod sub {
    use super::*;
    pub struct Sub {
        source_reg: Register,
        dest_reg: Register,
        immediate: Option<Immediate>,
        shift: bool,
        update_flags: bool,
    }

    impl Sub {
        pub fn new(dest_reg: Register, source_reg: Register) -> Self {
            Self {
                source_reg,
                dest_reg,
                immediate: None,
                shift: false,
                update_flags: false,
            }
        }

        pub fn with_immediate(self, imm: Immediate) -> super::add_sub::AddSubImmediate {
            super::add_sub::AddSubImmediate::new(
                self.source_reg,
                self.dest_reg,
                imm,
                super::add_sub::AddOrSub::Sub,
            )
        }
    }
}

pub use sub::Sub;
