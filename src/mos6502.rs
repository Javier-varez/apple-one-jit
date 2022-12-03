pub mod addressing_modes;
pub mod instructions;
pub mod opcode;

#[derive(Debug)]
pub enum Error {
    UnknownOpCode,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Instruction {
    pub opcode: opcode::OpCode,
    pub operand: addressing_modes::Operand,
}

#[derive(Clone, Copy)]
enum State {
    DecodeOpCode,
    DecodeOperand(opcode::OpCode, Option<u8>),
}

pub struct InstrDecoder {
    state: State,
}

impl InstrDecoder {
    pub fn new() -> Self {
        Self {
            state: State::DecodeOpCode,
        }
    }

    pub fn feed(&mut self, byte: u8) -> Result<Option<Instruction>, Error> {
        match self.state.clone() {
            State::DecodeOpCode => {
                if let Some(opcode) = opcode::translate(byte) {
                    if opcode.addressing_mode().operand_size() == 0 {
                        self.state = State::DecodeOpCode;
                        Ok(Some(Instruction {
                            opcode,
                            operand: addressing_modes::Operand::None,
                        }))
                    } else {
                        self.state = State::DecodeOperand(opcode, None);
                        Ok(None)
                    }
                } else {
                    Err(Error::UnknownOpCode)
                }
            }
            State::DecodeOperand(opcode, Some(first_byte)) => {
                self.state = State::DecodeOpCode;
                Ok(Some(Instruction {
                    opcode,
                    operand: addressing_modes::Operand::U16(
                        ((byte as u16) << 8) | (first_byte as u16),
                    ),
                }))
            }
            State::DecodeOperand(opcode, None) => {
                if opcode.addressing_mode().operand_size() == 1 {
                    self.state = State::DecodeOpCode;
                    Ok(Some(Instruction {
                        opcode,
                        operand: addressing_modes::Operand::U8(byte),
                    }))
                } else {
                    self.state = State::DecodeOperand(opcode, Some(byte));
                    Ok(None)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::mos6502::addressing_modes::{AddressingMode, Operand};
    use crate::mos6502::instructions::BaseInstruction;
    use crate::mos6502::opcode::OpCode;
    use crate::mos6502::{InstrDecoder, Instruction};

    #[test]
    fn decode_jsr_absolute_instruction() {
        let mut decoder = InstrDecoder::new();
        assert_eq!(decoder.feed(0x20).unwrap(), None);
        assert_eq!(decoder.feed(0xaf).unwrap(), None);
        assert_eq!(
            decoder.feed(0x5d).unwrap(),
            Some(Instruction {
                opcode: OpCode::new(BaseInstruction::Jsr, AddressingMode::Absolute),
                operand: Operand::U16(0x5daf)
            })
        );
    }

    #[test]
    fn decode_brk_instruction() {
        let mut decoder = InstrDecoder::new();
        assert_eq!(
            decoder.feed(0x00).unwrap(),
            Some(Instruction {
                opcode: OpCode::new(BaseInstruction::Brk, AddressingMode::Implied),
                operand: Operand::None
            })
        );
    }

    #[test]
    fn decode_bpl_relative_instruction() {
        let mut decoder = InstrDecoder::new();
        assert_eq!(decoder.feed(0x10).unwrap(), None);
        assert_eq!(
            decoder.feed(0x35).unwrap(),
            Some(Instruction {
                opcode: OpCode::new(BaseInstruction::Bpl, AddressingMode::Relative),
                operand: Operand::U8(0x35)
            })
        );
    }
}
