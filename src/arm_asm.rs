//! This module provides functions to dynamically assemble armv8-a machine code.

/// Represents an opcode for a single Aarch64 machine instruction
pub struct OpCode(u32);

impl OpCode {
    /// Obtains the underlying value of the opcode
    pub fn value(&self) -> u32 {
        self.0
    }
}

/// Immediate argument for a machine code instruction
pub struct Immediate(u64);

/// Signed immediate argument for a machine code instruction
pub struct SignedImmediate(i64);

impl Immediate {
    /// Constructs a new immediate from the given value
    pub fn new(val: u64) -> Self {
        Self(val)
    }
}

impl SignedImmediate {
    /// Constructs a new immediate from the given value
    pub fn new(val: i64) -> Self {
        Self(val)
    }
}

/// Aarch64 Machine registers
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

mod operand {
    pub trait OperandType {}
    pub struct UnknownOperand {}
    pub struct ImmediateOperand {}
    pub struct RegisterOperand {}

    impl OperandType for UnknownOperand {}
    impl OperandType for ImmediateOperand {}
    impl OperandType for RegisterOperand {}
}

mod udf {
    use super::*;

    /// Aarch64 `udf` asm instruction.
    ///
    /// Causes an exception if executed
    /// ```
    ///     use apple_one_jit::arm_asm::Udf;
    ///     let opcode = Udf::new().generate();
    /// ```
    pub struct Udf();

    impl Udf {
        /// Constructs a new Udf instruction
        pub fn new() -> Self {
            Self()
        }

        /// Generates the opcode for the instruction
        pub fn generate(self) -> OpCode {
            OpCode(0)
        }
    }
}

pub use udf::Udf;

mod ret {
    use super::*;

    /// Aarch64 `ret` asm instruction.
    ///
    /// Returns execution to the caller, replacing the program counter by the value in the
    /// selected register (X30, also known as LR, by default)
    pub struct Ret(Register);

    impl Ret {
        /// Constructs a new Ret instruction
        /// ```
        ///     use apple_one_jit::arm_asm::{Ret, Register};
        ///     let opcode = Ret::new().with_register(Register::X30).generate();
        /// ```
        pub fn new() -> Self {
            Self(Register::X30)
        }

        /// Sets the register to use for the ret instruction
        pub fn with_register(mut self, register: Register) -> Self {
            self.0 = register;
            self
        }

        /// Generates the opcode for the instruction
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
    use std::marker::PhantomData;

    /// A `branch` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Branch, SignedImmediate};
    ///     // Jump 5 instructions before and save PC+4 in R30 (LR)
    ///     let opcode = Branch::new()
    ///         .with_immediate(SignedImmediate::new(-5)).link().generate();
    /// ```
    pub struct Branch<T: operand::OperandType> {
        register: Option<Register>,
        immediate: Option<SignedImmediate>,
        link: bool,
        _pd: PhantomData<T>,
    }

    impl Branch<operand::UnknownOperand> {
        pub fn new() -> Self {
            Self {
                register: None,
                immediate: None,
                link: false,
                _pd: PhantomData,
            }
        }

        pub fn with_register(self, reg: Register) -> Branch<operand::RegisterOperand> {
            Branch::<operand::RegisterOperand> {
                register: Some(reg),
                immediate: None,
                link: self.link,
                _pd: PhantomData,
            }
        }

        pub fn with_immediate(self, imm: SignedImmediate) -> Branch<operand::ImmediateOperand> {
            Branch::<operand::ImmediateOperand> {
                register: None,
                immediate: Some(imm),
                link: self.link,
                _pd: PhantomData,
            }
        }
    }

    impl<T: operand::OperandType> Branch<T> {
        /// If called, stores the return address in X30 when branching
        pub fn link(mut self) -> Self {
            self.link = true;
            self
        }
    }

    impl Branch<operand::ImmediateOperand> {
        pub fn generate(self) -> OpCode {
            const LINK_OFFSET: usize = 31;
            const BASE: u32 = 0x14000000;
            const IMM_MASK: i64 = (1 << 26) - 1;
            let link = if self.link { 1 << LINK_OFFSET } else { 0 };

            let immediate = self.immediate.unwrap().0;
            let extra_bits = immediate & !IMM_MASK;
            assert!((extra_bits == 0) || (extra_bits == !IMM_MASK));
            let immediate = (immediate & IMM_MASK) as u32;

            OpCode(BASE | link | immediate)
        }
    }

    impl Branch<operand::RegisterOperand> {
        pub fn generate(self) -> OpCode {
            unimplemented!()
        }
    }
}

pub use branch::Branch;

/// Register shift type
pub enum RegShift {
    /// Logical shift left
    Lsl(u32),
    /// Logical shift right (no sign extension)
    Lsr(u32),
    /// Arithmetic shift right (sign-extends)
    Asr(u32),
    /// Rotate right
    Ror(u32),
}

impl RegShift {
    fn validate(&self) -> u32 {
        let value = match self {
            RegShift::Lsl(val) => val,
            RegShift::Lsr(val) => val,
            RegShift::Asr(val) => val,
            RegShift::Ror(val) => val,
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
            RegShift::Ror(_) => (3 << SHIFT_TYPE_OFFSET),
        };
        let shift_value = self.validate();

        (shift_value << SHIFT_VALUE_OFFSET) | (shift_type << SHIFT_TYPE_OFFSET)
    }
}

mod arithmetic {
    use super::*;
    use std::marker::PhantomData;

    #[repr(u8)]
    enum Operation {
        Add = 0,
        Sub = 1,
    }

    struct ImmediateArgs {
        immediate: Immediate,
        shift: bool,
    }

    struct RegisterArgs {
        second_source_reg: Register,
        reg_shift: Option<RegShift>,
    }

    /// Aarch64 asm arithmetic instructions.
    pub struct ArithmeticOp<const OP: u8, T: operand::OperandType> {
        source_reg: Register,
        dest_reg: Register,
        update_flags: bool,
        immediate: Option<ImmediateArgs>,
        register: Option<RegisterArgs>,
        _pd: PhantomData<T>,
    }

    impl<const OP: u8, T: operand::OperandType> ArithmeticOp<OP, T> {
        /// Updates the flags after the operation
        pub fn update_flags(mut self) -> Self {
            self.update_flags = true;
            self
        }
    }

    impl<const OP: u8> ArithmeticOp<OP, operand::UnknownOperand> {
        /// Constructs the arithmetic operation
        pub fn new(dest_reg: Register, source_reg: Register) -> Self {
            Self {
                source_reg,
                dest_reg,
                update_flags: false,
                immediate: None,
                register: None,
                _pd: PhantomData {},
            }
        }

        /// Selects the immediate operation type
        pub fn with_immediate(self, imm: Immediate) -> ArithmeticOp<OP, operand::ImmediateOperand> {
            ArithmeticOp::<OP, operand::ImmediateOperand> {
                source_reg: self.source_reg,
                dest_reg: self.dest_reg,
                update_flags: self.update_flags,
                immediate: Some(ImmediateArgs {
                    immediate: imm,
                    shift: false,
                }),
                register: None,
                _pd: PhantomData {},
            }
        }

        /// Selects the shifted register operation type
        pub fn with_shifted_reg(self, reg: Register) -> ArithmeticOp<OP, operand::RegisterOperand> {
            ArithmeticOp::<OP, operand::RegisterOperand> {
                source_reg: self.source_reg,
                dest_reg: self.dest_reg,
                update_flags: self.update_flags,
                immediate: None,
                register: Some(RegisterArgs {
                    second_source_reg: reg,
                    reg_shift: None,
                }),
                _pd: PhantomData {},
            }
        }
    }

    impl<const OP: u8> ArithmeticOp<OP, operand::ImmediateOperand> {
        /// Shifts the immediate another 12 bits.
        pub fn shifted(mut self) -> Self {
            self.immediate.as_mut().unwrap().shift = true;
            self
        }

        /// Generates the opcode for this instruction
        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0x91000000;
            const IMM_OFFSET: usize = 10;
            const SRC_REG_OFFSET: usize = 5;
            const DST_REG_OFFSET: usize = 0;
            let ImmediateArgs { immediate, shift } = self.immediate.unwrap();

            let source_reg = (self.source_reg as u32) << SRC_REG_OFFSET;
            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let shift = if shift { 1 << 22 } else { 0 };
            let update_flags = if self.update_flags { 1 << 29 } else { 0 };
            let imm = (immediate.0 as u32) << IMM_OFFSET;
            let ty = (OP as u32) << 30;
            OpCode(OPCODE_BASE | source_reg | dest_reg | shift | imm | update_flags | ty)
        }
    }

    impl<const OP: u8> ArithmeticOp<OP, operand::RegisterOperand> {
        /// Shifts the second register operand before performing the arithmetic operation
        pub fn with_shift(mut self, reg_shift: RegShift) -> Self {
            self.register.as_mut().unwrap().reg_shift.replace(reg_shift);
            self
        }

        /// Generates the opcode for this instruction
        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0x8B000000;
            const SRC_REG2_OFFSET: usize = 16;
            const SRC_REG_OFFSET: usize = 5;
            const DST_REG_OFFSET: usize = 0;
            let RegisterArgs {
                reg_shift,
                second_source_reg,
            } = self.register.unwrap();

            let source_reg = (self.source_reg as u32) << SRC_REG_OFFSET;
            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let second_source_reg = (second_source_reg as u32) << SRC_REG2_OFFSET;
            let shift = reg_shift.map(|shift| shift.value()).unwrap_or(0);
            let update_flags = if self.update_flags { 1 << 29 } else { 0 };
            let ty = (OP as u32) << 30;
            OpCode(
                OPCODE_BASE | source_reg | dest_reg | second_source_reg | shift | update_flags | ty,
            )
        }
    }

    /// An arithmetic `add` instruction for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Add, RegShift};
    ///     let opcode = Add::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_shifted_reg(Register::X30).with_shift(RegShift::Lsl(2)).generate();
    /// ```
    pub type Add = ArithmeticOp<{ Operation::Add as u8 }, operand::UnknownOperand>;

    /// An arithmetic `sub` instruction for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Sub, RegShift};
    ///     let opcode = Sub::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_shifted_reg(Register::X30).with_shift(RegShift::Asr(3)).generate();
    /// ```
    pub type Sub = ArithmeticOp<{ Operation::Sub as u8 }, operand::UnknownOperand>;
}

pub use arithmetic::Add;
pub use arithmetic::Sub;

mod logical_op {
    use super::*;
    use std::marker::PhantomData;

    #[repr(u8)]
    enum Operation {
        And = 0,
        Or = 1,
        Xor = 2,
    }

    struct RegisterArgs {
        register: Register,
        reg_shift: Option<RegShift>,
    }

    pub struct LogicalOperation<const OP: u8, ArgumentType: operand::OperandType> {
        source_reg: Register,
        dest_reg: Register,
        immediate: Option<Immediate>,
        register: Option<RegisterArgs>,
        _pd: PhantomData<ArgumentType>,
    }

    /// Creates a logical operation with the given intermediate as the second argument
    impl<const OP: u8> LogicalOperation<OP, operand::UnknownOperand> {
        pub fn new(dest_reg: Register, source_reg: Register) -> Self {
            Self {
                dest_reg,
                source_reg,
                immediate: None,
                register: None,
                _pd: PhantomData {},
            }
        }

        /// Creates a logical operation with the given intermediate as the second operand
        pub fn with_immediate(
            self,
            imm: Immediate,
        ) -> LogicalOperation<OP, operand::ImmediateOperand> {
            LogicalOperation::<OP, operand::ImmediateOperand> {
                source_reg: self.source_reg,
                dest_reg: self.dest_reg,
                immediate: Some(imm),
                register: None,
                _pd: PhantomData {},
            }
        }

        /// Creates a logical operation with the given register as the second operand
        pub fn with_shifted_reg(
            self,
            register: Register,
        ) -> LogicalOperation<OP, operand::RegisterOperand> {
            LogicalOperation::<OP, operand::RegisterOperand> {
                source_reg: self.source_reg,
                dest_reg: self.dest_reg,
                immediate: None,
                register: Some(RegisterArgs {
                    register,
                    reg_shift: None,
                }),
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

    impl<const OP: u8> LogicalOperation<OP, operand::ImmediateOperand> {
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

    impl<const OP: u8> LogicalOperation<OP, operand::RegisterOperand> {
        /// Shifts the second register operand before performing the arithmetic operation
        pub fn with_shift(mut self, reg_shift: RegShift) -> Self {
            self.register.as_mut().unwrap().reg_shift.replace(reg_shift);
            self
        }

        /// Generates the opcode for this instruction
        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0x8A000000;
            const SRC_REG2_OFFSET: usize = 16;
            const SRC_REG_OFFSET: usize = 5;
            const DST_REG_OFFSET: usize = 0;
            const TYPE_OFFSET: usize = 29;
            let RegisterArgs {
                reg_shift,
                register: second_source_reg,
            } = self.register.unwrap();

            let source_reg = (self.source_reg as u32) << SRC_REG_OFFSET;
            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let second_source_reg = (second_source_reg as u32) << SRC_REG2_OFFSET;
            let shift = reg_shift.map(|shift| shift.value()).unwrap_or(0);
            let ty = (OP as u32) << TYPE_OFFSET;
            OpCode(OPCODE_BASE | source_reg | dest_reg | second_source_reg | shift | ty)
        }
    }

    /// A logical `or` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Or, Immediate};
    ///     let opcode = Or::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_immediate(Immediate::new(0xaaaaaaaaaaaaaaaa)).generate();
    /// ```
    pub type Or = LogicalOperation<{ Operation::Or as u8 }, operand::UnknownOperand>;

    /// A logical `and` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, And, Immediate};
    ///     let opcode = And::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_immediate(Immediate::new(0xaaaaaaaaaaaaaaaa)).generate();
    /// ```
    pub type And = LogicalOperation<{ Operation::And as u8 }, operand::UnknownOperand>;

    /// A logical `xor` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Xor, Immediate};
    ///     let opcode = Xor::new(
    ///             Register::X0,
    ///             Register::X1
    ///         )
    ///         .with_immediate(Immediate::new(0xaaaaaaaaaaaaaaaa)).generate();
    /// ```
    pub type Xor = LogicalOperation<{ Operation::Xor as u8 }, operand::UnknownOperand>;
}

pub use logical_op::And;
pub use logical_op::Or;
pub use logical_op::Xor;

mod mov {
    use super::*;
    use std::marker::PhantomData;

    #[repr(u8)]
    enum Operation {
        MOVN = 0,
        MOVZ = 2,
        MOVK = 3,
    }

    #[repr(u32)]
    pub enum MovShift {
        Bits0 = 0,
        Bits16 = 1,
        Bits32 = 2,
        Bits48 = 3,
    }

    pub struct MovOperation<const OP: u8, ArgumentType: operand::OperandType> {
        dest_reg: Register,
        immediate: Option<Immediate>,
        shift: MovShift,
        _pd: PhantomData<ArgumentType>,
    }

    impl<const OP: u8> MovOperation<OP, operand::UnknownOperand> {
        /// Creates a mov operation targeting the given register
        pub fn new(dest_reg: Register) -> Self {
            Self {
                dest_reg,
                immediate: None,
                shift: MovShift::Bits0,
                _pd: PhantomData {},
            }
        }

        /// Creates a mov operation with the given intermediate as the second operand
        pub fn with_immediate(self, imm: Immediate) -> MovOperation<OP, operand::ImmediateOperand> {
            MovOperation::<OP, operand::ImmediateOperand> {
                dest_reg: self.dest_reg,
                immediate: Some(imm),
                shift: MovShift::Bits0,
                _pd: PhantomData {},
            }
        }
    }

    impl<const OP: u8> MovOperation<OP, operand::ImmediateOperand> {
        pub fn with_shift(mut self, shift: MovShift) -> Self {
            self.shift = shift;
            self
        }
        pub fn generate(self) -> OpCode {
            const OPCODE_BASE: u32 = 0x92800000;
            const IMM_OFFSET: usize = 5;
            const IMM_MASK: u32 = 0xFFFF;
            const DST_REG_OFFSET: usize = 0;

            if (self.immediate.as_ref().unwrap().0 & !(IMM_MASK as u64)) != 0 {
                panic!("Invalid immediate operand for mov operation. Must be 16-bit maximum");
            }

            let dest_reg = (self.dest_reg as u32) << DST_REG_OFFSET;
            let shift = (self.shift as u32) << 21;
            let imm = ((self.immediate.unwrap().0 as u32) & IMM_MASK) << IMM_OFFSET;
            let ty = (OP as u32) << 29;
            OpCode(OPCODE_BASE | dest_reg | shift | imm | ty)
        }
    }

    /// A `movz` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Movz, MovShift, Immediate};
    ///     let opcode = Movz::new(
    ///             Register::X0,
    ///         )
    ///         .with_immediate(Immediate::new(0xAAAA)).with_shift(MovShift::Bits16).generate();
    /// ```
    pub type Movz = MovOperation<{ Operation::MOVZ as u8 }, operand::UnknownOperand>;

    /// A `movk` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Movk, MovShift, Immediate};
    ///     let opcode = Movk::new(
    ///             Register::X0,
    ///         )
    ///         .with_immediate(Immediate::new(0xAAAA)).with_shift(MovShift::Bits16).generate();
    /// ```
    pub type Movk = MovOperation<{ Operation::MOVK as u8 }, operand::UnknownOperand>;

    /// A `movn` operation for Aarch64
    /// ```
    ///     use apple_one_jit::arm_asm::{Register, Movn, MovShift, Immediate};
    ///     let opcode = Movn::new(
    ///             Register::X0,
    ///         )
    ///         .with_immediate(Immediate::new(0xAAAA)).with_shift(MovShift::Bits16).generate();
    /// ```
    pub type Movn = MovOperation<{ Operation::MOVN as u8 }, operand::UnknownOperand>;
}

pub use mov::MovShift;
pub use mov::Movk;
pub use mov::Movn;
pub use mov::Movz;
