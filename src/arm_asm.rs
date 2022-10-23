/// This module provides functions to dynamically assemble armv8-a machine code.

pub struct OpCode(u32);
impl OpCode {
    pub fn value(&self) -> u32 {
        self.0
    }
}

pub trait OperandType {}

pub struct UnknownOperand {}
pub struct ImmediateOperand {}
pub struct RegisterOperand {}

impl OperandType for UnknownOperand {}
impl OperandType for ImmediateOperand {}
impl OperandType for RegisterOperand {}

pub struct Immediate(u64);

impl Immediate {
    pub fn new(val: u64) -> Self {
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
        source_reg: Register,
        dest_reg: Register,
        immediate: Immediate,
        shift: bool,
        update_flags: bool,
        add_or_sub: AddOrSub,
    }

    impl AddSubImmediate {
        pub(super) fn new(
            source_reg: Register,
            dest_reg: Register,
            imm: Immediate,
            add_or_sub: AddOrSub,
        ) -> Self {
            if imm.0 > 1u64 << 12 {
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
            let ty = match self.add_or_sub {
                AddOrSub::Add => 0,
                AddOrSub::Sub => 1 << 30,
            };
            OpCode(OPCODE_BASE | source_reg | dest_reg | shift | imm | update_flags | ty)
        }
    }

    pub enum RegShift {
        Lsl(u32),
        Lsr(u32),
        Asr(u32),
    }

    impl RegShift {
        fn validate(&self) -> u32 {
            let value = match self {
                RegShift::Lsl(val) => val,
                RegShift::Lsr(val) => val,
                RegShift::Asr(val) => val,
            };
            const MASK: u32 = 0x3f;
            assert_eq!(value & !MASK, 0);
            value & MASK
        }

        fn value(&self) -> u32 {
            const SHIFT_TYPE_OFFSET: usize = 22;
            const SHIFT_VALUE_OFFSET: usize = 10;
            let shift_type = match self {
                RegShift::Lsl(_) => (0 << SHIFT_TYPE_OFFSET),
                RegShift::Lsr(_) => (1 << SHIFT_TYPE_OFFSET),
                RegShift::Asr(_) => (2 << SHIFT_TYPE_OFFSET),
            };
            let shift_value = self.validate();

            (shift_value << SHIFT_VALUE_OFFSET) | (shift_type << SHIFT_TYPE_OFFSET)
        }
    }

    pub struct AddSubShiftedRegister {
        source_reg: Register,
        second_source_reg: Register,
        dest_reg: Register,
        update_flags: bool,
        reg_shift: Option<RegShift>,
        add_or_sub: AddOrSub,
    }

    impl AddSubShiftedRegister {
        pub(super) fn new(
            source_reg: Register,
            dest_reg: Register,
            second_source_reg: Register,
            add_or_sub: AddOrSub,
        ) -> Self {
            Self {
                source_reg,
                dest_reg,
                second_source_reg,
                add_or_sub,
                reg_shift: None,
                update_flags: false,
            }
        }

        /// Shifts the immediate another 12 bits.
        pub fn update_flags(mut self) -> Self {
            self.update_flags = true;
            self
        }

        pub fn with_shift(mut self, reg_shift: RegShift) -> Self {
            self.reg_shift = Some(reg_shift);
            self
        }

        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0x0B000000;
            const SRC_REG2_OFFSET: usize = 16;
            const SRC_REG_OFFSET: usize = 5;
            const DST_REG_OFFSET: usize = 0;
            let source_reg = (self.source_reg as u32) << SRC_REG_OFFSET;
            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let second_source_reg = (self.second_source_reg as u32) << SRC_REG2_OFFSET;
            let shift = self.reg_shift.map(|shift| shift.value()).unwrap_or(0);
            let update_flags = if self.update_flags { 1 << 29 } else { 0 };
            let ty = match self.add_or_sub {
                AddOrSub::Add => 0,
                AddOrSub::Sub => 1 << 30,
            };
            OpCode(
                OPCODE_BASE | source_reg | dest_reg | second_source_reg | shift | update_flags | ty,
            )
        }
    }
}

pub use add_sub::RegShift;

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

        pub fn with_shifted_reg(self, reg: Register) -> super::add_sub::AddSubShiftedRegister {
            super::add_sub::AddSubShiftedRegister::new(
                self.source_reg,
                self.dest_reg,
                reg,
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
    }

    impl Sub {
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
                super::add_sub::AddOrSub::Sub,
            )
        }

        pub fn with_shifted_reg(self, reg: Register) -> super::add_sub::AddSubShiftedRegister {
            super::add_sub::AddSubShiftedRegister::new(
                self.source_reg,
                self.dest_reg,
                reg,
                super::add_sub::AddOrSub::Sub,
            )
        }
    }
}

pub use sub::Sub;

mod logical_op {
    use super::*;
    use std::marker::PhantomData;

    #[repr(u8)]
    enum Operation {
        And = 0,
        Or = 1,
        Xor = 2,
    }

    pub struct LogicalOperation<const OP: u8, ArgumentType: OperandType> {
        source_reg: Register,
        dest_reg: Register,
        immediate: Option<Immediate>,
        _pd: PhantomData<ArgumentType>,
    }

    /// Creates a logical operation with the given intermediate as the second argument
    impl<const OP: u8> LogicalOperation<OP, UnknownOperand> {
        pub fn new(dest_reg: Register, source_reg: Register) -> Self {
            Self {
                dest_reg,
                source_reg,
                immediate: None,
                _pd: PhantomData {},
            }
        }

        /// Creates a logical operation with the given intermediate as the second argument
        pub fn with_immediate(self, imm: Immediate) -> LogicalOperation<OP, ImmediateOperand> {
            LogicalOperation::<OP, ImmediateOperand> {
                source_reg: self.source_reg,
                dest_reg: self.dest_reg,
                immediate: Some(imm),
                _pd: PhantomData {},
            }
        }
    }

    fn is_mask(value: u64) -> bool {
        (value & (value + 1)) == 0
    }

    fn is_shifted_mask(value: u64) -> bool {
        (value != 0) && (is_mask(value | (value - 1)))
    }

    impl<const OP: u8> LogicalOperation<OP, ImmediateOperand> {
        fn encode_immediate(&self) -> (u32, u32, u32) {
            let immediate: u64 = self
                .immediate
                .as_ref()
                .map(|imm| imm.0)
                .expect("LogicalOperation should contain an immediate value to encode");

            if (immediate == 0) || (immediate == 0xffff_ffff_ffff_ffffu64) {
                panic!("Immediate must not be all 0's or all 1's");
            }

            // Calculate the size of the immediate
            let mut size = 64;
            while size > 2 {
                size = size / 2;
                let mask = (1 << size) - 1;

                if (immediate & mask) != ((immediate >> size) & mask) {
                    // The pattern does not work anymore
                    size *= 2;
                    break;
                }
            }

            let mask = 0xffff_ffff_ffff_ffffu64 >> (64 - size);
            let mut sized_immediate = immediate & mask;

            let (rol, num_bits) = if is_shifted_mask(sized_immediate) {
                let rol = u64::trailing_zeros(sized_immediate);
                let num_bits = u64::trailing_zeros(!(sized_immediate >> rol));
                (rol, num_bits)
            } else {
                sized_immediate |= !mask;
                assert!(is_shifted_mask(!sized_immediate));
                let leading_ones = u64::leading_zeros(!sized_immediate);
                let trailing_ones = u64::trailing_zeros(!(sized_immediate));
                let num_bits = leading_ones + trailing_ones + size - 64;
                let rol = 64 - leading_ones;
                (rol, num_bits)
            };

            let nimms = !(size - 1) << 1;
            let nimms = nimms | (num_bits - 1);
            let n = ((nimms >> 6) & 1) ^ 1;
            let imms = nimms & 0x3f;
            let immr = (size - rol) & (size - 1);

            (n, immr, imms)
        }

        pub fn generate(self) -> OpCode {
            let (n, immr, imms) = self.encode_immediate();
            const OPCODE_BASE: u32 = 0x92000000;
            const SRC_REG_OFFSET: usize = 5;
            const DST_REG_OFFSET: usize = 0;
            const IMMS_OFFSET: usize = 10;
            const IMMR_OFFSET: usize = 16;
            const N_OFFSET: usize = 22;
            const TYPE_OFFSET: usize = 29;
            let source_reg = (self.source_reg as u32) << SRC_REG_OFFSET;
            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let n = n << N_OFFSET;
            let immr = immr << IMMR_OFFSET;
            let imms = imms << IMMS_OFFSET;
            let ty = (OP as u32) << TYPE_OFFSET;
            OpCode(OPCODE_BASE | source_reg | dest_reg | n | immr | imms | ty)
        }
    }

    /// A logical `OR` operation
    /// Example:
    /// ```
    ///     let opcode = Or::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_immediate(Immediate::new(0xaaaaaaaaaaaaaaaa)).generate();
    /// ```
    pub type Or = LogicalOperation<{ Operation::Or as u8 }, UnknownOperand>;

    /// A logical `AND` operation
    /// Example:
    /// ```
    ///     let opcode = And::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_immediate(Immediate::new(0xaaaaaaaaaaaaaaaa)).generate();
    /// ```
    pub type And = LogicalOperation<{ Operation::And as u8 }, UnknownOperand>;

    /// A logical `XOR` operation
    /// Example:
    /// ```
    ///     let opcode = Xor::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_immediate(Immediate::new(0xaaaaaaaaaaaaaaaa)).generate();
    /// ```
    pub type Xor = LogicalOperation<{ Operation::Xor as u8 }, UnknownOperand>;
}

pub use logical_op::And;
pub use logical_op::Or;
pub use logical_op::Xor;
