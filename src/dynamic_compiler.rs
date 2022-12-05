use crate::arm_asm;
use crate::arm_asm::Immediate;
use crate::dynamic_compiler::Error::Unknown6502OpCode;
use crate::jit::{JitPage, OpCodeStream};
use crate::mos6502::{self, opcode};

core::arch::global_asm!(include_str!("dynamic_compiler.S"));

extern "C" {
    fn jumpToEmulatedCode(ptr: *const (), state: *mut CpuState, memory: *mut u8);
}

#[derive(Debug)]
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

#[repr(C)]
#[derive(Default, PartialEq, Debug)]
pub struct CpuState {
    pub a: u64,
    pub x: u64,
    pub y: u64,
    pub sp: u64,
    pub pc: u64,
    pub flags: u64,
}

pub struct Compiler {
    page: JitPage,
    decoder: mos6502::InstrDecoder,
}

const ACCUMULATOR_REGISTER: arm_asm::Register = arm_asm::Register::X0;
const X_REGISTER: arm_asm::Register = arm_asm::Register::X1;
const Y_REGISTER: arm_asm::Register = arm_asm::Register::X2;
const SP_REGISTER: arm_asm::Register = arm_asm::Register::X3;
const PC_REGISTER: arm_asm::Register = arm_asm::Register::X4;
const MEMORY_MAP_BASE_ADDR_REGISTER: arm_asm::Register = arm_asm::Register::X5;
const DECODED_OP_REGISTER: arm_asm::Register = arm_asm::Register::X6;
const SCRATCH_REGISTER: arm_asm::Register = arm_asm::Register::X7;

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

    fn emit_increment_pc(opcode_stream: &mut OpCodeStream, instruction: &mos6502::Instruction) {
        opcode_stream.push_opcode(
            arm_asm::Add::new(PC_REGISTER, PC_REGISTER)
                .with_immediate(arm_asm::Immediate::new(
                    instruction.instruction_size() as u64
                ))
                .generate(),
        );
    }

    // Register allocation:
    // R0(8-bit) -> Accumulator
    // R1(8-bit) -> X Register
    // R2(8-bit) -> Y Register
    // R3(8-bit) -> SP Register
    // R4(16-bit) -> PC
    // R5(8/16-bit) -> Decoded operand (if any)
    // R6(64-bit) -> Base address of the 6502 memory map.
    // R7(8/16-bit) -> Scratch register
    fn translate_code_impl(
        decoder: &mut mos6502::InstrDecoder,
        opcode_stream: &mut OpCodeStream,
        buffer: &[u8],
    ) -> Result<(), Error> {
        for byte in buffer.iter() {
            if let Some(instr) = decoder.feed(*byte)? {
                println!("Decoded instr {:?}", instr);
                Self::emit_instruction_address_mode(opcode_stream, &instr);
                Self::emit_instruction(opcode_stream, &instr);
                if !instr.opcode.base_instruction().is_branching_op() {
                    // Conditional branchs need to handle this internally. Jumps simply don't need
                    // this because they always set the PC
                    Self::emit_increment_pc(opcode_stream, &instr);
                }
            }
        }

        // Just in case the instruction decoder did not find any isntr
        opcode_stream.push_opcode(arm_asm::Ret::new().generate());

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

                if value & 0xff == 0xff {
                    // This is annoying because a bug in the 6502 would cause a read of 0x33ff
                    // to read the low byte from 0x33ff and the high byte from 0x3300
                    opcode_stream.push_opcode(
                        arm_asm::Ldrb::new(DECODED_OP_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                            .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                                Immediate::new(value as u64),
                            ))
                            .generate(),
                    );
                    opcode_stream.push_opcode(
                        arm_asm::Ldrb::new(SCRATCH_REGISTER, MEMORY_MAP_BASE_ADDR_REGISTER)
                            .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                                Immediate::new(value as u64),
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
                }
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

    fn emit_deref_if_needed(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        dest_register: arm_asm::Register,
    ) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::Memory => {
                opcode_stream.push_opcode(
                    arm_asm::Ldrb::new(dest_register, MEMORY_MAP_BASE_ADDR_REGISTER)
                        .with_mode(arm_asm::MemoryAccessMode::ShiftedRegister(
                            DECODED_OP_REGISTER,
                        ))
                        .generate(),
                );
            }
            _ => {}
        }
    }

    fn emit_load_instruction(opcode_stream: &mut OpCodeStream, instruction: &mos6502::Instruction) {
        let dest_reg = match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => ACCUMULATOR_REGISTER,
            mos6502::instructions::BaseInstruction::Ldx => X_REGISTER,
            mos6502::instructions::BaseInstruction::Ldy => Y_REGISTER,
            _ => {
                unreachable!()
            }
        };

        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::Memory => {
                opcode_stream.push_opcode(
                    arm_asm::Ldrb::new(dest_reg, MEMORY_MAP_BASE_ADDR_REGISTER)
                        .with_mode(arm_asm::MemoryAccessMode::ShiftedRegister(
                            DECODED_OP_REGISTER,
                        ))
                        .generate(),
                );
            }
            mos6502::addressing_modes::OperandType::Value => {
                opcode_stream
                    .push_opcode(arm_asm::Mov::new(dest_reg, DECODED_OP_REGISTER).generate());
            }
            mos6502::addressing_modes::OperandType::None => {
                unreachable!()
            }
        }

        opcode_stream.push_opcode(arm_asm::SetF8::new(dest_reg).generate());
    }

    fn emit_store_instruction(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
    ) {
        let source_reg = match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => ACCUMULATOR_REGISTER,
            mos6502::instructions::BaseInstruction::Ldx => X_REGISTER,
            mos6502::instructions::BaseInstruction::Ldy => Y_REGISTER,
            _ => {
                unreachable!()
            }
        };

        assert!(
            instruction.opcode.addressing_mode().operand_type()
                == mos6502::addressing_modes::OperandType::Memory
        );
        opcode_stream.push_opcode(
            arm_asm::Strb::new(source_reg, MEMORY_MAP_BASE_ADDR_REGISTER)
                .with_mode(arm_asm::MemoryAccessMode::ShiftedRegister(
                    DECODED_OP_REGISTER,
                ))
                .generate(),
        );
    }

    /// Handles the actual instruction, assuming that the decoded operand is available in DECODED_OP_REGISTER
    fn emit_instruction(opcode_stream: &mut OpCodeStream, instruction: &mos6502::Instruction) {
        match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => {
                Self::emit_load_instruction(opcode_stream, instruction);
            }
            mos6502::instructions::BaseInstruction::Ldx => {
                Self::emit_load_instruction(opcode_stream, instruction);
            }
            mos6502::instructions::BaseInstruction::Ldy => {
                Self::emit_load_instruction(opcode_stream, instruction);
            }
            mos6502::instructions::BaseInstruction::Sta => {
                Self::emit_store_instruction(opcode_stream, instruction);
            }
            mos6502::instructions::BaseInstruction::Stx => {
                Self::emit_store_instruction(opcode_stream, instruction);
            }
            mos6502::instructions::BaseInstruction::Sty => {
                Self::emit_store_instruction(opcode_stream, instruction);
            }
            mos6502::instructions::BaseInstruction::Adc => {
                // TODO(javier-varez): handle carry as well... (both setting it after and using it
                // if set before)
                Self::emit_deref_if_needed(opcode_stream, instruction, DECODED_OP_REGISTER);
                opcode_stream.push_opcode(
                    arm_asm::Add::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::And => {
                Self::emit_deref_if_needed(opcode_stream, instruction, DECODED_OP_REGISTER);
                opcode_stream.push_opcode(
                    arm_asm::And::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Ora => {
                Self::emit_deref_if_needed(opcode_stream, instruction, DECODED_OP_REGISTER);
                opcode_stream.push_opcode(
                    arm_asm::Or::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Eor => {
                Self::emit_deref_if_needed(opcode_stream, instruction, DECODED_OP_REGISTER);
                opcode_stream.push_opcode(
                    arm_asm::Xor::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Tax => {
                // TODO(javier-varez): Update flags
                opcode_stream
                    .push_opcode(arm_asm::Mov::new(X_REGISTER, ACCUMULATOR_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Tay => {
                // TODO(javier-varez): Update flags
                opcode_stream
                    .push_opcode(arm_asm::Mov::new(Y_REGISTER, ACCUMULATOR_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Tsx => {
                // TODO(javier-varez): Update flags
                opcode_stream.push_opcode(arm_asm::Mov::new(X_REGISTER, SP_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Txa => {
                // TODO(javier-varez): Update flags
                opcode_stream
                    .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, X_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Tya => {
                // TODO(javier-varez): Update flags
                opcode_stream
                    .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, Y_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Txs => {
                // TODO(javier-varez): Update flags
                opcode_stream.push_opcode(arm_asm::Mov::new(SP_REGISTER, X_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Rts => {
                // TODO(javier-varez): Pop pc from stack and set it before actually returning
                opcode_stream.push_opcode(arm_asm::Ret::new().generate());
            }
            _ => {
                unimplemented!();
            }
        }
    }

    /// Runs the dynamically-reassembled code, protecting temporarily the inner page as RX.
    /// # Safety
    /// Must guarantee that the compiled code is valid and will naturally complete execution
    pub unsafe fn run(&mut self, state: &mut CpuState, memory: &mut [u8]) {
        self.page.run(|ptr| {
            jumpToEmulatedCode(ptr, state as *mut CpuState, memory.as_mut_ptr());
        })
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn empty_test() {
        use super::*;

        let mut state = CpuState::default();
        let mut memory = [0u8; 256];

        let mut compiler = Compiler::new().unwrap();
        compiler.translate_code(&[]).unwrap();
        unsafe { compiler.run(&mut state, &mut memory) };
    }

    #[test]
    fn add_test() {
        use super::*;

        let mut state = CpuState::default();
        let mut memory = [0u8; 256];

        let mut compiler = Compiler::new().unwrap();
        compiler
            .translate_code(&[0xA9, 0x0A, 0x69, 0x14, 0x60])
            .unwrap();
        unsafe { compiler.run(&mut state, &mut memory) };

        assert_eq!(state.a, 30);
    }

    #[test]
    fn load_acc_immediate_test() {
        use super::*;

        let mut state = CpuState::default();
        let mut memory = [0u8; 256];

        let mut compiler = Compiler::new().unwrap();
        compiler.translate_code(&[0xA9, 0x0A, 0x60]).unwrap();
        unsafe { compiler.run(&mut state, &mut memory) };

        assert_eq!(state.a, 10);
    }

    #[test]
    fn load_acc_memory_test() {
        use super::*;

        let mut state = CpuState::default();
        let mut memory = [0u8; 256];
        memory[0xa] = 0x34;

        let mut compiler = Compiler::new().unwrap();
        compiler.translate_code(&[0xA5, 0x0A, 0x60]).unwrap();
        unsafe { compiler.run(&mut state, &mut memory) };

        assert_eq!(state.a, 0x34);
    }

    #[test]
    fn add_immediate_test() {
        use super::*;

        let mut state = CpuState::default();
        let mut memory = [0u8; 256];
        memory[0xa] = 0x34;

        let mut compiler = Compiler::new().unwrap();
        compiler.translate_code(&[0xA5, 0x0A, 0x69, 0x10]).unwrap();
        unsafe { compiler.run(&mut state, &mut memory) };

        assert_eq!(state.a, 0x44);
    }

    #[test]
    fn add_memory_test() {
        use super::*;

        let mut state = CpuState::default();
        let mut memory = [0u8; 256];
        memory[0xa] = 0x34;
        memory[0x10] = 0x44;

        let mut compiler = Compiler::new().unwrap();
        compiler.translate_code(&[0xA5, 0x0A, 0x65, 0x10]).unwrap();
        unsafe { compiler.run(&mut state, &mut memory) };

        assert_eq!(state.a, 0x78);
    }
}
