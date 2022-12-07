#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AddressingMode {
    Accumulator = 0,
    Absolute = 1,
    AbsoluteXIndexed = 2,
    AbsoluteYIndexed = 3,
    Immediate = 4,
    Implied = 5,
    Indirect = 6,
    XIndexedIndirect = 7,
    IndirectYIndexed = 8,
    Relative = 9,
    Zeropage = 10,
    ZeropageXIndexed = 11,
    ZeropageYIndexed = 12,
}

const INST_SIZE_MAP: [usize; 13] = [
    0_usize, // AddressingMode::Accumulator
    2_usize, // AddressingMode::Absolute
    2_usize, // AddressingMode::AbsoluteXIndexed,
    2_usize, // AddressingMode::AbsoluteYIndexed
    1_usize, // AddressingMode::Immediate
    0_usize, // AddressingMode::Implied
    1_usize, // AddressingMode::Indirect
    1_usize, // AddressingMode::XIndexedIndirect
    1_usize, // AddressingMode::IndirectYIndexed
    1_usize, // AddressingMode::Relative
    1_usize, // AddressingMode::Zeropage
    1_usize, // AddressingMode::ZeropageXIndexed
    1_usize, // AddressingMode::ZeropageYIndexed
];

impl AddressingMode {
    pub fn operand_size(&self) -> usize {
        INST_SIZE_MAP[*self as usize]
    }
    pub fn operand_type(&self) -> OperandType {
        match self {
            AddressingMode::Accumulator => OperandType::Memory,
            AddressingMode::Absolute => OperandType::Memory,
            AddressingMode::AbsoluteXIndexed => OperandType::Memory,
            AddressingMode::AbsoluteYIndexed => OperandType::Memory,
            AddressingMode::Immediate => OperandType::Value,
            AddressingMode::Implied => OperandType::None,
            AddressingMode::Indirect => OperandType::Memory,
            AddressingMode::IndirectYIndexed => OperandType::Memory,
            AddressingMode::XIndexedIndirect => OperandType::Memory,
            AddressingMode::Relative => OperandType::Memory,
            AddressingMode::Zeropage => OperandType::Memory,
            AddressingMode::ZeropageXIndexed => OperandType::Memory,
            AddressingMode::ZeropageYIndexed => OperandType::Memory,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OperandType {
    Value,
    Memory,
    None,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Operand {
    U16(u16),
    U8(u8),
    None,
}
