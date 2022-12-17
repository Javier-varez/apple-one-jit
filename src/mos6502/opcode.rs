use super::addressing_modes::AddressingMode;
use super::instructions::BaseInstruction;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct OpCode {
    pub base_instr: BaseInstruction,
    pub addressing_mode: AddressingMode,
}

const OPCODES: [Option<OpCode>; 256] = [
    Some(OpCode::new(BaseInstruction::Brk, AddressingMode::Implied)), // 0x00
    Some(OpCode::new(
        BaseInstruction::Ora,
        AddressingMode::XIndexedIndirect,
    )), // 0x01
    None,                                                             // 0x02
    None,                                                             // 0x03
    None,                                                             // 0x04
    Some(OpCode::new(BaseInstruction::Ora, AddressingMode::Zeropage)), // 0x05
    Some(OpCode::new(BaseInstruction::Asl, AddressingMode::Zeropage)), // 0x06
    None,                                                             // 0x07
    Some(OpCode::new(BaseInstruction::Php, AddressingMode::Implied)), // 0x08
    Some(OpCode::new(BaseInstruction::Ora, AddressingMode::Immediate)), // 0x09
    Some(OpCode::new(
        BaseInstruction::Asl,
        AddressingMode::Accumulator,
    )), // 0x0A
    None,                                                             // 0x0B
    None,                                                             // 0x0C
    Some(OpCode::new(BaseInstruction::Ora, AddressingMode::Absolute)), // 0x0D
    Some(OpCode::new(BaseInstruction::Asl, AddressingMode::Absolute)), // 0x0E
    None,                                                             // 0x0F
    Some(OpCode::new(BaseInstruction::Bpl, AddressingMode::Relative)), // 0x10
    Some(OpCode::new(
        BaseInstruction::Ora,
        AddressingMode::AbsoluteYIndexed,
    )), // 0x11
    None,                                                             // 0x12
    None,                                                             // 0x13
    None,                                                             // 0x14
    Some(OpCode::new(
        BaseInstruction::Ora,
        AddressingMode::ZeropageXIndexed,
    )), // 0x15
    Some(OpCode::new(
        BaseInstruction::Asl,
        AddressingMode::ZeropageXIndexed,
    )), // 0x16
    None,                                                             // 0x17
    Some(OpCode::new(BaseInstruction::Clc, AddressingMode::Implied)), // 0x18
    Some(OpCode::new(
        BaseInstruction::Ora,
        AddressingMode::AbsoluteYIndexed,
    )), // 0x19
    None,                                                             // 0x1A
    None,                                                             // 0x1B
    None,                                                             // 0x1C
    Some(OpCode::new(
        BaseInstruction::Ora,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x1D
    Some(OpCode::new(
        BaseInstruction::Asl,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x1E
    None,                                                             // 0x1F
    Some(OpCode::new(BaseInstruction::Jsr, AddressingMode::Absolute)), // 0x20
    Some(OpCode::new(
        BaseInstruction::And,
        AddressingMode::XIndexedIndirect,
    )), // 0x21
    None,                                                             // 0x22
    None,                                                             // 0x23
    Some(OpCode::new(BaseInstruction::Bit, AddressingMode::Zeropage)), // 0x24
    Some(OpCode::new(BaseInstruction::And, AddressingMode::Zeropage)), // 0x25
    Some(OpCode::new(BaseInstruction::Rol, AddressingMode::Zeropage)), // 0x26
    None,                                                             // 0x27
    Some(OpCode::new(BaseInstruction::Plp, AddressingMode::Implied)), // 0x28
    Some(OpCode::new(BaseInstruction::And, AddressingMode::Immediate)), // 0x29
    Some(OpCode::new(
        BaseInstruction::Rol,
        AddressingMode::Accumulator,
    )), // 0x2A
    None,                                                             // 0x2B
    Some(OpCode::new(BaseInstruction::Bit, AddressingMode::Absolute)), // 0x2C
    Some(OpCode::new(BaseInstruction::And, AddressingMode::Absolute)), // 0x2D
    Some(OpCode::new(BaseInstruction::Rol, AddressingMode::Absolute)), // 0x2E
    None,                                                             // 0x2F
    Some(OpCode::new(BaseInstruction::Bmi, AddressingMode::Relative)), // 0x30
    Some(OpCode::new(
        BaseInstruction::And,
        AddressingMode::IndirectYIndexed,
    )), // 0x31
    None,                                                             // 0x32
    None,                                                             // 0x33
    None,                                                             // 0x34
    Some(OpCode::new(
        BaseInstruction::And,
        AddressingMode::ZeropageXIndexed,
    )), // 0x35
    Some(OpCode::new(
        BaseInstruction::Rol,
        AddressingMode::ZeropageXIndexed,
    )), // 0x36
    None,                                                             // 0x37
    Some(OpCode::new(BaseInstruction::Sec, AddressingMode::Implied)), // 0x38
    Some(OpCode::new(
        BaseInstruction::And,
        AddressingMode::AbsoluteYIndexed,
    )), // 0x39
    None,                                                             // 0x3A
    None,                                                             // 0x3B
    None,                                                             // 0x3C
    Some(OpCode::new(
        BaseInstruction::And,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x3D
    Some(OpCode::new(
        BaseInstruction::Rol,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x3E
    None,                                                             // 0x3F
    Some(OpCode::new(BaseInstruction::Rti, AddressingMode::Implied)), // 0x40
    Some(OpCode::new(
        BaseInstruction::Eor,
        AddressingMode::XIndexedIndirect,
    )), // 0x41
    None,                                                             // 0x42
    None,                                                             // 0x43
    None,                                                             // 0x44
    Some(OpCode::new(BaseInstruction::Eor, AddressingMode::Zeropage)), // 0x45
    Some(OpCode::new(BaseInstruction::Lsr, AddressingMode::Zeropage)), // 0x46
    None,                                                             // 0x47
    Some(OpCode::new(BaseInstruction::Pha, AddressingMode::Implied)), // 0x48
    Some(OpCode::new(BaseInstruction::Eor, AddressingMode::Immediate)), // 0x49
    Some(OpCode::new(
        BaseInstruction::Lsr,
        AddressingMode::Accumulator,
    )), // 0x4A
    None,                                                             // 0x4B
    Some(OpCode::new(BaseInstruction::Jmp, AddressingMode::Absolute)), // 0x4C
    Some(OpCode::new(BaseInstruction::Eor, AddressingMode::Absolute)), // 0x4D
    Some(OpCode::new(BaseInstruction::Lsr, AddressingMode::Absolute)), // 0x4E
    None,                                                             // 0x4F
    Some(OpCode::new(BaseInstruction::Bvc, AddressingMode::Relative)), // 0x50
    Some(OpCode::new(
        BaseInstruction::Eor,
        AddressingMode::IndirectYIndexed,
    )), // 0x51
    None,                                                             // 0x52
    None,                                                             // 0x53
    None,                                                             // 0x54
    Some(OpCode::new(
        BaseInstruction::Eor,
        AddressingMode::ZeropageXIndexed,
    )), // 0x55
    Some(OpCode::new(
        BaseInstruction::Lsr,
        AddressingMode::ZeropageXIndexed,
    )), // 0x56
    None,                                                             // 0x57
    Some(OpCode::new(BaseInstruction::Cli, AddressingMode::Implied)), // 0x58
    Some(OpCode::new(
        BaseInstruction::Eor,
        AddressingMode::AbsoluteYIndexed,
    )), // 0x59
    None,                                                             // 0x5A
    None,                                                             // 0x5B
    None,                                                             // 0x5C
    Some(OpCode::new(
        BaseInstruction::Eor,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x5D
    Some(OpCode::new(
        BaseInstruction::Lsr,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x5E
    None,                                                             // 0x5F
    Some(OpCode::new(BaseInstruction::Rts, AddressingMode::Implied)), // 0x60
    Some(OpCode::new(
        BaseInstruction::Adc,
        AddressingMode::XIndexedIndirect,
    )), // 0x61
    None,                                                             // 0x62
    None,                                                             // 0x63
    None,                                                             // 0x64
    Some(OpCode::new(BaseInstruction::Adc, AddressingMode::Zeropage)), // 0x65
    Some(OpCode::new(BaseInstruction::Ror, AddressingMode::Zeropage)), // 0x66
    None,                                                             // 0x67
    Some(OpCode::new(BaseInstruction::Pla, AddressingMode::Implied)), // 0x68
    Some(OpCode::new(BaseInstruction::Adc, AddressingMode::Immediate)), // 0x69
    Some(OpCode::new(
        BaseInstruction::Ror,
        AddressingMode::Accumulator,
    )), // 0x6A
    None,                                                             // 0x6B
    Some(OpCode::new(BaseInstruction::Jmp, AddressingMode::Indirect)), // 0x6C
    Some(OpCode::new(BaseInstruction::Adc, AddressingMode::Absolute)), // 0x6D
    Some(OpCode::new(BaseInstruction::Ror, AddressingMode::Absolute)), // 0x6E
    None,                                                             // 0x6F
    Some(OpCode::new(BaseInstruction::Bvs, AddressingMode::Relative)), // 0x70
    Some(OpCode::new(
        BaseInstruction::Adc,
        AddressingMode::IndirectYIndexed,
    )), // 0x71
    None,                                                             // 0x72
    None,                                                             // 0x73
    None,                                                             // 0x74
    Some(OpCode::new(
        BaseInstruction::Adc,
        AddressingMode::ZeropageXIndexed,
    )), // 0x75
    Some(OpCode::new(
        BaseInstruction::Ror,
        AddressingMode::ZeropageXIndexed,
    )), // 0x76
    None,                                                             // 0x77
    Some(OpCode::new(BaseInstruction::Sei, AddressingMode::Implied)), // 0x78
    Some(OpCode::new(
        BaseInstruction::Adc,
        AddressingMode::AbsoluteYIndexed,
    )), // 0x79
    None,                                                             // 0x7A
    None,                                                             // 0x7B
    None,                                                             // 0x7C
    Some(OpCode::new(
        BaseInstruction::Adc,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x7D
    Some(OpCode::new(
        BaseInstruction::Ror,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x7E
    None,                                                             // 0x7F
    None,                                                             // 0x80
    Some(OpCode::new(
        BaseInstruction::Sta,
        AddressingMode::XIndexedIndirect,
    )), // 0x81
    None,                                                             // 0x82
    None,                                                             // 0x83
    Some(OpCode::new(BaseInstruction::Sty, AddressingMode::Zeropage)), // 0x84
    Some(OpCode::new(BaseInstruction::Sta, AddressingMode::Zeropage)), // 0x85
    Some(OpCode::new(BaseInstruction::Stx, AddressingMode::Zeropage)), // 0x86
    None,                                                             // 0x87
    Some(OpCode::new(BaseInstruction::Dey, AddressingMode::Implied)), // 0x88
    None,                                                             // 0x89
    Some(OpCode::new(BaseInstruction::Txa, AddressingMode::Implied)), // 0x8A
    None,                                                             // 0x8B
    Some(OpCode::new(BaseInstruction::Sty, AddressingMode::Absolute)), // 0x8C
    Some(OpCode::new(BaseInstruction::Sta, AddressingMode::Absolute)), // 0x8D
    Some(OpCode::new(BaseInstruction::Stx, AddressingMode::Absolute)), // 0x8E
    None,                                                             // 0x8F
    Some(OpCode::new(BaseInstruction::Bcc, AddressingMode::Relative)), // 0x90
    Some(OpCode::new(
        BaseInstruction::Sta,
        AddressingMode::IndirectYIndexed,
    )), // 0x91
    None,                                                             // 0x92
    None,                                                             // 0x93
    Some(OpCode::new(
        BaseInstruction::Sty,
        AddressingMode::ZeropageXIndexed,
    )), // 0x94
    Some(OpCode::new(
        BaseInstruction::Sta,
        AddressingMode::ZeropageXIndexed,
    )), // 0x95
    Some(OpCode::new(
        BaseInstruction::Stx,
        AddressingMode::ZeropageYIndexed,
    )), // 0x96
    None,                                                             // 0x97
    Some(OpCode::new(BaseInstruction::Tya, AddressingMode::Implied)), // 0x98
    Some(OpCode::new(
        BaseInstruction::Sta,
        AddressingMode::AbsoluteYIndexed,
    )), // 0x99
    Some(OpCode::new(BaseInstruction::Txs, AddressingMode::Implied)), // 0x9A
    None,                                                             // 0x9B
    None,                                                             // 0x9C
    Some(OpCode::new(
        BaseInstruction::Sta,
        AddressingMode::AbsoluteXIndexed,
    )), // 0x9D
    None,                                                             // 0x9E
    None,                                                             // 0x9F
    Some(OpCode::new(BaseInstruction::Ldy, AddressingMode::Immediate)), // 0xA0
    Some(OpCode::new(
        BaseInstruction::Lda,
        AddressingMode::XIndexedIndirect,
    )), // 0xA1
    Some(OpCode::new(BaseInstruction::Ldx, AddressingMode::Immediate)), // 0xA2
    None,                                                             // 0xA3
    Some(OpCode::new(BaseInstruction::Ldy, AddressingMode::Zeropage)), // 0xA4
    Some(OpCode::new(BaseInstruction::Lda, AddressingMode::Zeropage)), // 0xA5
    Some(OpCode::new(BaseInstruction::Ldx, AddressingMode::Zeropage)), // 0xA6
    None,                                                             // 0xA7
    Some(OpCode::new(BaseInstruction::Tay, AddressingMode::Implied)), // 0xA8
    Some(OpCode::new(BaseInstruction::Lda, AddressingMode::Immediate)), // 0xA9
    Some(OpCode::new(BaseInstruction::Tax, AddressingMode::Implied)), // 0xAA
    None,                                                             // 0xAB
    Some(OpCode::new(BaseInstruction::Ldy, AddressingMode::Absolute)), // 0xAC
    Some(OpCode::new(BaseInstruction::Lda, AddressingMode::Absolute)), // 0xAD
    Some(OpCode::new(BaseInstruction::Ldx, AddressingMode::Absolute)), // 0xAE
    None,                                                             // 0xAF
    Some(OpCode::new(BaseInstruction::Bcs, AddressingMode::Relative)), // 0xB0
    Some(OpCode::new(
        BaseInstruction::Lda,
        AddressingMode::IndirectYIndexed,
    )), // 0xB1
    None,                                                             // 0xB2
    None,                                                             // 0xB3
    Some(OpCode::new(
        BaseInstruction::Ldy,
        AddressingMode::ZeropageXIndexed,
    )), // 0xB4
    Some(OpCode::new(
        BaseInstruction::Lda,
        AddressingMode::ZeropageXIndexed,
    )), // 0xB5
    Some(OpCode::new(
        BaseInstruction::Ldx,
        AddressingMode::ZeropageYIndexed,
    )), // 0xB6
    None,                                                             // 0xB7
    Some(OpCode::new(BaseInstruction::Clv, AddressingMode::Implied)), // 0xB8
    Some(OpCode::new(
        BaseInstruction::Lda,
        AddressingMode::AbsoluteYIndexed,
    )), // 0xB9
    Some(OpCode::new(BaseInstruction::Tsx, AddressingMode::Implied)), // 0xBA
    None,                                                             // 0xBB
    Some(OpCode::new(
        BaseInstruction::Ldy,
        AddressingMode::AbsoluteXIndexed,
    )), // 0xBC
    Some(OpCode::new(
        BaseInstruction::Lda,
        AddressingMode::AbsoluteXIndexed,
    )), // 0xBD
    Some(OpCode::new(
        BaseInstruction::Ldx,
        AddressingMode::AbsoluteYIndexed,
    )), // 0xBE
    None,                                                             // 0xBF
    Some(OpCode::new(BaseInstruction::Cpy, AddressingMode::Immediate)), // 0xC0
    Some(OpCode::new(
        BaseInstruction::Cmp,
        AddressingMode::XIndexedIndirect,
    )), // 0xC1
    None,                                                             // 0xC2
    None,                                                             // 0xC3
    Some(OpCode::new(BaseInstruction::Cpy, AddressingMode::Zeropage)), // 0xC4
    Some(OpCode::new(BaseInstruction::Cmp, AddressingMode::Zeropage)), // 0xC5
    Some(OpCode::new(BaseInstruction::Dec, AddressingMode::Zeropage)), // 0xC6
    None,                                                             // 0xC7
    Some(OpCode::new(BaseInstruction::Iny, AddressingMode::Implied)), // 0xC8
    Some(OpCode::new(BaseInstruction::Cmp, AddressingMode::Immediate)), // 0xC9
    Some(OpCode::new(BaseInstruction::Dex, AddressingMode::Implied)), // 0xCA
    None,                                                             // 0xCB
    Some(OpCode::new(BaseInstruction::Cpy, AddressingMode::Absolute)), // 0xCC
    Some(OpCode::new(BaseInstruction::Cmp, AddressingMode::Absolute)), // 0xCD
    Some(OpCode::new(BaseInstruction::Dec, AddressingMode::Absolute)), // 0xCE
    None,                                                             // 0xCF
    Some(OpCode::new(BaseInstruction::Bne, AddressingMode::Relative)), // 0xD0
    Some(OpCode::new(
        BaseInstruction::Cmp,
        AddressingMode::IndirectYIndexed,
    )), // 0xD1
    None,                                                             // 0xD2
    None,                                                             // 0xD3
    None,                                                             // 0xD4
    Some(OpCode::new(
        BaseInstruction::Cmp,
        AddressingMode::ZeropageXIndexed,
    )), // 0xD5
    Some(OpCode::new(
        BaseInstruction::Dec,
        AddressingMode::ZeropageXIndexed,
    )), // 0xD6
    None,                                                             // 0xD7
    Some(OpCode::new(BaseInstruction::Cld, AddressingMode::Implied)), // 0xD8
    Some(OpCode::new(
        BaseInstruction::Cmp,
        AddressingMode::AbsoluteYIndexed,
    )), // 0xD9
    None,                                                             // 0xDA
    None,                                                             // 0xDB
    None,                                                             // 0xDC
    Some(OpCode::new(
        BaseInstruction::Cmp,
        AddressingMode::AbsoluteXIndexed,
    )), // 0xDD
    Some(OpCode::new(
        BaseInstruction::Dec,
        AddressingMode::AbsoluteXIndexed,
    )), // 0xDE
    None,                                                             // 0xDF
    Some(OpCode::new(BaseInstruction::Cpx, AddressingMode::Immediate)), // 0xE0
    Some(OpCode::new(
        BaseInstruction::Sbc,
        AddressingMode::XIndexedIndirect,
    )), // 0xE1
    None,                                                             // 0xE2
    None,                                                             // 0xE3
    Some(OpCode::new(BaseInstruction::Cpx, AddressingMode::Zeropage)), // 0xE4
    Some(OpCode::new(BaseInstruction::Sbc, AddressingMode::Zeropage)), // 0xE5
    Some(OpCode::new(BaseInstruction::Inc, AddressingMode::Zeropage)), // 0xE6
    None,                                                             // 0xE7
    Some(OpCode::new(BaseInstruction::Inx, AddressingMode::Implied)), // 0xE8
    Some(OpCode::new(BaseInstruction::Sbc, AddressingMode::Immediate)), // 0xE9
    Some(OpCode::new(BaseInstruction::Nop, AddressingMode::Implied)), // 0xEA
    None,                                                             // 0xEB
    Some(OpCode::new(BaseInstruction::Cpx, AddressingMode::Absolute)), // 0xEC
    Some(OpCode::new(BaseInstruction::Sbc, AddressingMode::Absolute)), // 0xED
    Some(OpCode::new(BaseInstruction::Inc, AddressingMode::Absolute)), // 0xEE
    None,                                                             // 0xEF
    Some(OpCode::new(BaseInstruction::Beq, AddressingMode::Relative)), // 0xF0
    Some(OpCode::new(
        BaseInstruction::Sbc,
        AddressingMode::IndirectYIndexed,
    )), // 0xF1
    None,                                                             // 0xF2
    None,                                                             // 0xF3
    None,                                                             // 0xF4
    Some(OpCode::new(
        BaseInstruction::Sbc,
        AddressingMode::ZeropageXIndexed,
    )), // 0xF5
    Some(OpCode::new(
        BaseInstruction::Inc,
        AddressingMode::ZeropageXIndexed,
    )), // 0xF6
    None,                                                             // 0xF7
    Some(OpCode::new(BaseInstruction::Sed, AddressingMode::Implied)), // 0xF8
    Some(OpCode::new(
        BaseInstruction::Sbc,
        AddressingMode::AbsoluteYIndexed,
    )), // 0xF9
    None,                                                             // 0xFA
    None,                                                             // 0xFB
    None,                                                             // 0xFC
    Some(OpCode::new(
        BaseInstruction::Sbc,
        AddressingMode::AbsoluteXIndexed,
    )), // 0xFD
    Some(OpCode::new(
        BaseInstruction::Inc,
        AddressingMode::AbsoluteXIndexed,
    )), // 0xFE
    None,                                                             // 0xFF
];

pub fn translate(byte: u8) -> Option<OpCode> {
    OPCODES[byte as usize]
}

impl OpCode {
    pub const fn new(base_instr: BaseInstruction, addressing_mode: AddressingMode) -> Self {
        Self {
            base_instr,
            addressing_mode,
        }
    }

    pub fn addressing_mode(&self) -> AddressingMode {
        self.addressing_mode
    }

    pub fn base_instruction(&self) -> BaseInstruction {
        self.base_instr
    }
}
