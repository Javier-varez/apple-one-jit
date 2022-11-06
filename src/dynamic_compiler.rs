use crate::arm_asm;
use crate::arm_asm::Immediate;
use crate::dynamic_compiler::Error::Unknown6502OpCode;
use crate::jit::{JitPage, OpCodeStream};
use crate::mos6502;

pub enum Error {
    // TODO(ja): Add variants
    Unknown6502OpCode,
}

impl From<mos6502::Error> for Error {
    fn from(mos_error: mos6502::Error) -> Self {
        match mos_error {
            mos6502::Error::UnknownOpCode => Unknown6502OpCode,
        }
    }
}

pub struct Compiler {
    page: JitPage,
    decoder: mos6502::InstrDecoder,
}

const ACCUMULATOR_REGISTER: arm_asm::Register = arm_asm::Register::X0;
const X_REGISTER: arm_asm::Register = arm_asm::Register::X1;
const Y_REGISTER: arm_asm::Register = arm_asm::Register::X2;
const SP_REGISTER: arm_asm::Register = arm_asm::Register::X3;
const DECODED_OP_REGISTER: arm_asm::Register = arm_asm::Register::X4;
const MEMORY_MAP_BASE_ADDR_REGISTER: arm_asm::Register = arm_asm::Register::X5;
const SCRATCH_REGISTER: arm_asm::Register = arm_asm::Register::X6;

impl Compiler {
    /// Constructs a new dynamic re-compiler, allocating a JIT page to store the generated code
    pub fn new() -> Result<Self, region::Error> {
        Ok(Self {
            page: JitPage::allocate(region::page::size())?,
            decoder: mos6502::InstrDecoder::new(),
        })
    }

    /// Translates the given mos6502 machine code into native arm64 machine code.
    pub fn translate_code(&mut self, buffer: &[u8]) -> Result<(), Error> {
        self.page.populate(|opcode_stream| {
            Self::translate_code_impl(&mut self.decoder, opcode_stream, buffer)
        })
    }

    // Register allocation:
    // R0(8-bit) -> Accumulator
    // R1(8-bit) -> X Register
    // R2(8-bit) -> Y Register
    // R3(8-bit) -> SP Register
    // R4(8/16-bit) -> Decoded operand (if any)
    // R5(64-bit) -> Base address of the 6502 memory map.
    // R6(8/16-bit) -> Scratch register
    fn translate_code_impl(
        decoder: &mut mos6502::InstrDecoder,
        opcode_stream: &mut OpCodeStream,
        buffer: &[u8],
    ) -> Result<(), Error> {
        for byte in buffer.iter() {
            if let Some(instr) = decoder.feed(*byte)? {
                // Handle instruction
                Self::emit_instruction_address_mode(opcode_stream, &instr);
                Self::emit_instruction(opcode_stream, &instr);
            }
        }
        Ok(())
    }

    /// Takes the operand and makes sure it ends up in register R4 of the host processor
    fn emit_instruction_address_mode(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
    ) {
        let operand = instruction.operand.clone();
        match &instruction.opcode.addressing_mode() {
            mos6502::addressing_modes::AddressingMode::Accumulator => {
                opcode_stream.push_opcode(
                    arm_asm::Mov::new(DECODED_OP_REGISTER, ACCUMULATOR_REGISTER).generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::Absolute => {
                let mos6502::addressing_modes::Operand::U16(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Absolute", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Movz::new(DECODED_OP_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteXIndexed => {
                let mos6502::addressing_modes::Operand::U16(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::AbsoluteXIndexed", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Add::new(DECODED_OP_REGISTER, X_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteYIndexed => {
                let mos6502::addressing_modes::Operand::U16(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::AbsoluteYIndexed", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Add::new(DECODED_OP_REGISTER, Y_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::Immediate => {
                let mos6502::addressing_modes::Operand::U8(value) = operand else {
                    panic!(
                        "Unexpected operand {:?} for AddressingMode::Immediate",
                        operand
                    )
                };

                opcode_stream.push_opcode(
                    arm_asm::Movz::new(DECODED_OP_REGISTER)
                        .with_immediate(arm_asm::Immediate::new(value as u64))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::Implied => {
                // Nothing to do here! :)
            }
            mos6502::addressing_modes::AddressingMode::Indirect => {
                let mos6502::addressing_modes::Operand::U16(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Indirect", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Ldrb::new(DECODED_OP_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                        .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                            Immediate::new(value as u64),
                        ))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::XIndexedIndirect => {
                let mos6502::addressing_modes::Operand::U8(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::XIndexedIndirect", operand);
                };

                // (u8 operand + x reg) => addr in zero page => contains the address we want
                // building the address here is kinda painful because the 6502 wraps around on the
                // zero page. so if (x + imm) == 0xff, the first byte of the resulting addr is at
                // memory location 0xff, while the second is at memory location 0x00 (not 0x100!).

                // Since we don't know the value of X beforehand, we cannot tell if it will
                // overflow at the boundary or not, so special handling is always needed (or we add
                // conditionally executed code, which is not so nice to do at the moment, but could
                // be a future optimization).
                //
                // THIS CODE IS WRONG!!!!
                // Therefore the pseudocode for the asm below does the following:
                //  - x + operand => scratch_register
                //  - And 0xff with scratch_register => scratch_register
                //  - ldrb(scratch_register) => decoded_op_register
                //  - x + operand + 1 => scratch_register
                //  - And 0xff with scratch_register => scratch_register
                //  - ldrb(scratch_register) => scratch_register
                //  - Add decoded_op_register + (scratch_register << 8) => decoded_op_register

                opcode_stream.push_opcode(
                    arm_asm::Add::new(SCRATCH_REGISTER, X_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                        .with_immediate(Immediate::new(0xff))
                        .generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::Ldrb::new(DECODED_OP_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                        .with_mode(arm_asm::MemoryAccessMode::ShiftedRegister(SCRATCH_REGISTER))
                        .generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::Add::new(SCRATCH_REGISTER, X_REGISTER)
                        .with_immediate(Immediate::new(value.wrapping_add(1) as u64))
                        .generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                        .with_immediate(Immediate::new(0xff))
                        .generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::Ldrb::new(SCRATCH_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                        .with_mode(arm_asm::MemoryAccessMode::ShiftedRegister(SCRATCH_REGISTER))
                        .generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                        .with_shifted_reg(SCRATCH_REGISTER)
                        .with_shift(arm_asm::RegShift::Lsl(8))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::IndirectYIndexed => {
                let mos6502::addressing_modes::Operand::U8(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::IndirectYIndexed", operand);
                };

                // (u8 operand) => addr in zero page + y reg => the address we want

                if value == 0xff {
                    // We have wraparound and needs special handling
                    opcode_stream.push_opcode(
                        arm_asm::Ldrb::new(DECODED_OP_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                            .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                                Immediate::new(0xff),
                            ))
                            .generate(),
                    );
                    opcode_stream.push_opcode(
                        arm_asm::Ldrb::new(SCRATCH_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                            .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                                Immediate::new(0x00),
                            ))
                            .generate(),
                    );
                    opcode_stream.push_opcode(
                        arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                            .with_shifted_reg(SCRATCH_REGISTER)
                            .with_shift(arm_asm::RegShift::Lsl(8))
                            .generate(),
                    );
                } else {
                    opcode_stream.push_opcode(
                        arm_asm::Ldrh::new(DECODED_OP_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                            .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                                Immediate::new(value as u64),
                            ))
                            .generate(),
                    );
                    opcode_stream.push_opcode(
                        arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                            .with_shifted_reg(Y_REGISTER)
                            .generate(),
                    );
                }
            }
            mos6502::addressing_modes::AddressingMode::Relative => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::Zeropage => {
                let mos6502::addressing_modes::Operand::U8(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Zeropage", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Movz::new(DECODED_OP_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::ZeropageXIndexed => {
                let mos6502::addressing_modes::Operand::U8(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::ZeropageXIndexed", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Add::new(DECODED_OP_REGISTER, X_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
            }
            mos6502::addressing_modes::AddressingMode::ZeropageYIndexed => {
                let mos6502::addressing_modes::Operand::U8(value) = operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::ZeropageYIndexed", operand);
                };

                opcode_stream.push_opcode(
                    arm_asm::Add::new(DECODED_OP_REGISTER, Y_REGISTER)
                        .with_immediate(Immediate::new(value as u64))
                        .generate(),
                );
            }
        }
    }

    /// Handles the actual instruction, assuming that the decoded operand is available in DECODED_OP_REGISTER
    fn emit_instruction(_opcode_stream: &mut OpCodeStream, instruction: &mos6502::Instruction) {
        match instruction {
            mos6502::Instruction {
                opcode:
                    mos6502::opcode::OpCode {
                        base_instr: mos6502::instructions::BaseInstruction::Adc,
                        ..
                    },
                ..
            } => {}
            _ => {}
        }
        unimplemented!()
    }

    /// Runs the dynamically-reassembled code, protecting temporarily the inner page as RX.
    /// # Safety
    /// Must guarantee that the compiled code is valid and will naturally complete execution
    pub unsafe fn run<U, T: Fn(*const ()) -> U>(&mut self, callable: T) -> U {
        self.page.run(callable)
    }
}
