use std::marker::PhantomData;

use crate::arm_asm::{self, Immediate};
use crate::block::{LocationRange, Marker, OpCodeStream};
use crate::dynamic_compiler::Error::Unknown6502OpCode;
use crate::memory::{Address, MemoryInterface};
use crate::mos6502;

#[derive(Debug)]
pub enum Error {
    Unknown6502OpCode,
}

impl From<mos6502::Error> for Error {
    fn from(mos_error: mos6502::Error) -> Self {
        match mos_error {
            mos6502::Error::UnknownOpCode => Unknown6502OpCode,
        }
    }
}

pub struct Compiler<'a, 'b: 'a, T: MemoryInterface> {
    decoder: mos6502::InstrDecoder,
    memory_interface: &'a mut T,
    trampolines: Trampolines,
    opcode_stream: &'a mut OpCodeStream<'b>,
    _pd: PhantomData<T>,
}

// These are callee-saved registers, which simplifies our life significantly when calling
// procedures since we don't need to bother about saving the state of the VM anywhere
const ACCUMULATOR_REGISTER: arm_asm::Register = arm_asm::Register::X19;
const X_REGISTER: arm_asm::Register = arm_asm::Register::X20;
const Y_REGISTER: arm_asm::Register = arm_asm::Register::X21;
const SP_REGISTER: arm_asm::Register = arm_asm::Register::X22;
const PC_REGISTER: arm_asm::Register = arm_asm::Register::X23;
const MEMORY_INTERFACE_REG: arm_asm::Register = arm_asm::Register::X24;
const DECODED_OP_REGISTER: arm_asm::Register = arm_asm::Register::X25;
const SCRATCH_REGISTER: arm_asm::Register = arm_asm::Register::X26;
const SCRATCH_REGISTER_2: arm_asm::Register = arm_asm::Register::X27;

// These are registers used for procedure calls, as defined by the AAPCS
const CALL_ARG0: arm_asm::Register = arm_asm::Register::X0;
const CALL_ARG1: arm_asm::Register = arm_asm::Register::X1;
const CALL_ARG2: arm_asm::Register = arm_asm::Register::X2;
const CALL_RESULT0: arm_asm::Register = arm_asm::Register::X0;

// This is an intra-procedure register, used for the trampoline.
const TRAMPOLIN_TARGET: arm_asm::Register = arm_asm::Register::X17;

type Trampoline = Marker;

struct Trampolines {
    write_8_bit: Trampoline,
    read_8_bit: Trampoline,
    read_16_bit: Trampoline,
}

impl<'a, 'b: 'a, T: MemoryInterface + 'a> Compiler<'a, 'b, T> {
    /// Constructs a new dynamic re-compiler
    pub fn new(opcode_stream: &'a mut OpCodeStream<'b>, memory_interface: &'a mut T) -> Self {
        let trampolines = Self::emit_trampolines(opcode_stream);
        Self {
            decoder: mos6502::InstrDecoder::new(),
            memory_interface,
            trampolines,
            opcode_stream,
            _pd: PhantomData {},
        }
    }

    /// Translates the given mos6502 machine code into native arm64 machine code.
    pub fn translate_code(&mut self, start_address: Address) -> Result<LocationRange, Error> {
        let mut address = start_address;
        let mut should_continue = true;
        while should_continue {
            if let Some(instr) = self
                .decoder
                .feed(self.memory_interface.read_8_bits(address))?
            {
                println!("Decoded instr {:?}", instr);
                self.emit_instruction_address_mode(&instr);
                self.emit_instruction(&instr);
                if !instr.opcode.base_instruction().is_branching_op() {
                    // Conditional branches need to handle this internally. Jumps simply don't need
                    // this because they always set the PC
                    self.emit_increment_pc(&instr);
                }
                should_continue = self.should_continue(&instr);
            }
            address = address.wrapping_add(1);
        }

        // Just in case the instruction decoder did not find any isntr
        self.opcode_stream
            .push_opcode(arm_asm::Ret::new().generate());

        Ok(LocationRange::new(start_address, address))
    }

    fn should_continue(&self, instruction: &mos6502::Instruction) -> bool {
        use mos6502::instructions::BaseInstruction;
        match instruction.opcode.base_instruction() {
            // TODO(javier-varez): Handle other instructions that can cause an exit
            BaseInstruction::Jsr
            | BaseInstruction::Jmp
            | BaseInstruction::Bcc
            | BaseInstruction::Bcs
            | BaseInstruction::Beq
            | BaseInstruction::Bne
            | BaseInstruction::Bmi
            | BaseInstruction::Bpl
            | BaseInstruction::Bvs
            | BaseInstruction::Bvc
            | BaseInstruction::Rts => false,
            _ => true,
        }
    }

    fn emit_trampolines(opcode_stream: &mut OpCodeStream) -> Trampolines {
        let forward_jump_marker = opcode_stream.add_marker();

        let read_8_bit_func: *const () = <T as MemoryInterface>::read_8_bits as *const _;
        let read_16_bit_func: *const () = <T as MemoryInterface>::read_16_bits as *const _;
        let write_8_bit_func: *const () = <T as MemoryInterface>::write_8_bits as *const _;
        let read_8_bit_marker = opcode_stream.push_pointer(read_8_bit_func);
        let read_16_bit_marker = opcode_stream.push_pointer(read_16_bit_func);
        let write_8_bit_marker = opcode_stream.push_pointer(write_8_bit_func);

        let jump_target =
            opcode_stream.relative_distance(&write_8_bit_marker.next(), &forward_jump_marker);

        opcode_stream.patch_opcode(
            &forward_jump_marker,
            arm_asm::Branch::new()
                .with_immediate(arm_asm::SignedImmediate::new(jump_target))
                .generate(),
        );

        // Save pointers as data here.
        Trampolines {
            read_8_bit: read_8_bit_marker,
            read_16_bit: read_16_bit_marker,
            write_8_bit: write_8_bit_marker,
        }
    }

    fn emit_context_save(&mut self) {
        self.opcode_stream.push_opcode(
            arm_asm::Sub::new(arm_asm::Register::SpZr, arm_asm::Register::SpZr)
                .with_immediate(Immediate::new(0x10))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            // link register
            arm_asm::Strd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(0),
                ))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            // x30 = NZCV
            arm_asm::Mrs::new(arm_asm::Register::X30, arm_asm::NZCV).generate(),
        );
        self.opcode_stream.push_opcode(
            // link register
            arm_asm::Strd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(1),
                ))
                .generate(),
        );
    }

    fn emit_context_restore(&mut self) {
        self.opcode_stream.push_opcode(
            // link register
            arm_asm::Ldrd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(1),
                ))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            // NZCV = x30
            arm_asm::Msr::new(arm_asm::NZCV, arm_asm::Register::X30).generate(),
        );
        self.opcode_stream.push_opcode(
            // link register
            arm_asm::Ldrd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(0),
                ))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(arm_asm::Register::SpZr, arm_asm::Register::SpZr)
                .with_immediate(Immediate::new(0x10))
                .generate(),
        );
    }

    fn emit_function_call(&mut self, target: Trampoline) {
        self.emit_context_save();
        // Load address of the target trampoline
        let current_location = self.opcode_stream.add_marker();
        let target_relative_distance = self
            .opcode_stream
            .relative_distance(&target, &current_location);
        self.opcode_stream.patch_opcode(
            &current_location,
            arm_asm::LdrLit::new(
                TRAMPOLIN_TARGET,
                arm_asm::SignedImmediate::new(target_relative_distance),
            )
            .generate(),
        );
        // Jump to it
        self.opcode_stream.push_opcode(
            arm_asm::Branch::new()
                .with_register(TRAMPOLIN_TARGET)
                .link()
                .generate(),
        );
        self.emit_context_restore();
    }

    fn emit_8_byte_load(&mut self, target_reg: arm_asm::Register, addr_reg: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG1, addr_reg).generate());
        self.emit_function_call(self.trampolines.read_8_bit.clone());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
    }

    fn emit_8_byte_load_immediate_addr(&mut self, target_reg: arm_asm::Register, address: u16) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(CALL_ARG1)
                .with_immediate(arm_asm::Immediate::new(address as u64))
                .generate(),
        );
        self.emit_function_call(self.trampolines.read_8_bit.clone());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
    }

    fn emit_16_byte_load_immediate_addr(&mut self, target_reg: arm_asm::Register, address: u16) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(CALL_ARG1)
                .with_immediate(arm_asm::Immediate::new(address as u64))
                .generate(),
        );
        self.emit_function_call(self.trampolines.read_16_bit.clone());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
    }

    fn emit_8_byte_store(&mut self, address_reg: arm_asm::Register, data_reg: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG1, address_reg).generate());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG2, data_reg).generate());
        self.emit_function_call(self.trampolines.write_8_bit.clone());
    }

    fn emit_increment_pc(&mut self, instruction: &mos6502::Instruction) {
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(PC_REGISTER, PC_REGISTER)
                .with_immediate(arm_asm::Immediate::new(
                    instruction.instruction_size() as u64
                ))
                .generate(),
        );
    }

    fn emit_addr_mode_accumulator(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(DECODED_OP_REGISTER, ACCUMULATOR_REGISTER).generate());
    }

    fn emit_addr_mode_absolute(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U16(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Absolute", instruction.operand);
                };

        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    fn emit_addr_mode_absolute_indexed(
        &mut self,
        instruction: &mos6502::Instruction,
        index_reg: arm_asm::Register,
    ) {
        let mos6502::addressing_modes::Operand::U16(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::AbsoluteIndexed", instruction.operand);
                };

        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, index_reg)
                .with_shifted_reg(SCRATCH_REGISTER)
                .generate(),
        );
    }

    fn emit_addr_mode_immediate(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!(
                        "Unexpected operand {:?} for AddressingMode::Immediate",
                        instruction.operand
                    )
                };

        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    fn emit_addr_mode_indirect(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U16(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Indirect", instruction.operand);
                };

        self.emit_16_byte_load_immediate_addr(DECODED_OP_REGISTER, value);
    }

    fn emit_addr_mode_x_indexed_indirect(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::XIndexedIndirect", instruction.operand);
                };

        // (u8 operand + x reg) => addr in zero page => contains the address we want
        // building the address here is kinda painful because the 6502 wraps around on the
        // zero page. so if (x + imm) == 0xff, the first byte of the resulting addr is at
        // memory location 0xff, while the second is at memory location 0x00 (not 0x100!).

        // Since we don't know the value of X beforehand, we cannot tell if it will
        // overflow at the boundary or not, so special handling is always needed (or we add
        // conditionally executed code, which is not so nice to do at the moment, but could
        // be a future optimization).

        self.opcode_stream.push_opcode(
            arm_asm::Add::new(SCRATCH_REGISTER, X_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
        self.emit_8_byte_load(DECODED_OP_REGISTER, SCRATCH_REGISTER);
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(SCRATCH_REGISTER, X_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value.wrapping_add(1) as u64))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
        self.emit_8_byte_load(SCRATCH_REGISTER, SCRATCH_REGISTER);
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                .with_shifted_reg(SCRATCH_REGISTER)
                .with_shift(arm_asm::RegShift::Lsl(8))
                .generate(),
        );
    }

    fn emit_addr_mode_indirect_y_indexed(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::IndirectYIndexed", instruction.operand);
                };

        // (u8 operand) => addr in zero page + y reg => the address we want
        if value == 0xff {
            // We have wraparound and needs special handling
            self.emit_8_byte_load_immediate_addr(DECODED_OP_REGISTER, 0xff);
            self.emit_8_byte_load_immediate_addr(SCRATCH_REGISTER, 0x00);
            self.opcode_stream.push_opcode(
                arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                    .with_shifted_reg(SCRATCH_REGISTER)
                    .with_shift(arm_asm::RegShift::Lsl(8))
                    .generate(),
            );
        } else {
            self.emit_16_byte_load_immediate_addr(DECODED_OP_REGISTER, value as u16);
            self.opcode_stream.push_opcode(
                arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                    .with_shifted_reg(Y_REGISTER)
                    .generate(),
            );
        }
    }

    fn emit_addr_mode_relative(&mut self, _instruction: &mos6502::Instruction) {
        // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
        unimplemented!();
    }

    fn emit_addr_mode_zeropage(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Zeropage", instruction.operand);
                };

        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    fn emit_addr_mode_zeropage_indexed(
        &mut self,
        instruction: &mos6502::Instruction,
        index_reg: arm_asm::Register,
    ) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::ZeropageIndexed", instruction.operand);
                };

        self.opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, index_reg)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    /// Takes the operand and makes sure it ends up in register R4 of the host processor
    fn emit_instruction_address_mode(&mut self, instruction: &mos6502::Instruction) {
        match &instruction.opcode.addressing_mode() {
            mos6502::addressing_modes::AddressingMode::Accumulator => {
                self.emit_addr_mode_accumulator();
            }
            mos6502::addressing_modes::AddressingMode::Absolute => {
                self.emit_addr_mode_absolute(instruction);
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteXIndexed => {
                self.emit_addr_mode_absolute_indexed(instruction, X_REGISTER);
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteYIndexed => {
                self.emit_addr_mode_absolute_indexed(instruction, Y_REGISTER);
            }
            mos6502::addressing_modes::AddressingMode::Immediate => {
                self.emit_addr_mode_immediate(instruction);
            }
            mos6502::addressing_modes::AddressingMode::Implied => {
                // Nothing to do here! :)
            }
            mos6502::addressing_modes::AddressingMode::Indirect => {
                self.emit_addr_mode_indirect(instruction);
            }
            mos6502::addressing_modes::AddressingMode::XIndexedIndirect => {
                self.emit_addr_mode_x_indexed_indirect(instruction);
            }
            mos6502::addressing_modes::AddressingMode::IndirectYIndexed => {
                self.emit_addr_mode_indirect_y_indexed(instruction);
            }
            mos6502::addressing_modes::AddressingMode::Relative => {
                self.emit_addr_mode_relative(instruction);
            }
            mos6502::addressing_modes::AddressingMode::Zeropage => {
                self.emit_addr_mode_zeropage(instruction);
            }
            mos6502::addressing_modes::AddressingMode::ZeropageXIndexed => {
                self.emit_addr_mode_zeropage_indexed(instruction, X_REGISTER);
            }
            mos6502::addressing_modes::AddressingMode::ZeropageYIndexed => {
                self.emit_addr_mode_zeropage_indexed(instruction, Y_REGISTER);
            }
        }
    }

    fn emit_deref_if_needed(
        &mut self,
        instruction: &mos6502::Instruction,
        dest_register: arm_asm::Register,
    ) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::Memory => {
                self.emit_8_byte_load(dest_register, DECODED_OP_REGISTER);
            }
            _ => {}
        }
    }

    fn flags_mask(flags: &[mos6502::Flags]) -> u64 {
        flags.iter().fold(0u64, |folded, flag| {
            // This translates to the NZCV aarch64 register flags
            folded
                | match flag {
                    mos6502::Flags::N => 0x8000_0000,
                    mos6502::Flags::Z => 0x4000_0000,
                    mos6502::Flags::C => 0x2000_0000,
                    mos6502::Flags::V => 0x1000_0000,
                }
        })
    }

    fn inverted_flags_mask(flags: &[mos6502::Flags]) -> u64 {
        Self::flags_mask(flags) ^ 0xF000_0000
    }

    // NOTE: this function uses the SCRATCH_REGISTER and SCRATCH_REGISTER_2, must not be overwritten
    // by the callable
    fn emit_save_flags(
        &mut self,
        saved_flags: &[mos6502::Flags], // the flags to keep
    ) {
        // TODO(javier-varez): I'm sure this could be optimized, but for now it shall work!
        // Compute saved mask
        if !saved_flags.is_empty() {
            self.opcode_stream
                .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
            self.opcode_stream.push_opcode(
                arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                    .with_immediate(arm_asm::Immediate::new(Self::flags_mask(saved_flags)))
                    .generate(),
            );
        }
    }

    // NOTE: this function uses the SCRATCH_REGISTER and SCRATCH_REGISTER_2, must not be overwritten
    // by the callable
    fn emit_restore_flags(
        &mut self,
        saved_flags: &[mos6502::Flags], // the flags to keep
    ) {
        if !saved_flags.is_empty() {
            self.opcode_stream
                .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER_2, arm_asm::NZCV).generate());
            self.opcode_stream.push_opcode(
                arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                    .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(
                        saved_flags,
                    )))
                    .generate(),
            );
            self.opcode_stream.push_opcode(
                arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                    .with_shifted_reg(SCRATCH_REGISTER_2)
                    .generate(),
            );
            self.opcode_stream
                .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());
        }
    }

    fn emit_load_instruction(&mut self, instruction: &mos6502::Instruction) {
        let dest_reg = match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => ACCUMULATOR_REGISTER,
            mos6502::instructions::BaseInstruction::Ldx => X_REGISTER,
            mos6502::instructions::BaseInstruction::Ldy => Y_REGISTER,
            _ => {
                unreachable!()
            }
        };

        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::Memory => {
                self.emit_8_byte_load(dest_reg, DECODED_OP_REGISTER);
            }
            mos6502::addressing_modes::OperandType::Value => {
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(dest_reg, DECODED_OP_REGISTER).generate());
            }
            mos6502::addressing_modes::OperandType::None => {
                unreachable!()
            }
        }

        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(dest_reg).generate());
        self.emit_restore_flags(&saved_flags);
    }

    fn emit_store_instruction(&mut self, instruction: &mos6502::Instruction) {
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
        self.emit_8_byte_store(source_reg, DECODED_OP_REGISTER);
    }

    /// Handles the actual instruction, assuming that the decoded operand is available in DECODED_OP_REGISTER
    fn emit_instruction(&mut self, instruction: &mos6502::Instruction) {
        match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => {
                self.emit_load_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Ldx => {
                self.emit_load_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Ldy => {
                self.emit_load_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Sta => {
                self.emit_store_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Stx => {
                self.emit_store_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Sty => {
                self.emit_store_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Adc => {
                self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
                self.opcode_stream.push_opcode(
                    arm_asm::Sxtb::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER).generate(),
                );
                self.opcode_stream.push_opcode(
                    arm_asm::Sxtb::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER).generate(),
                );
                self.opcode_stream.push_opcode(
                    arm_asm::Adc::new(
                        ACCUMULATOR_REGISTER,
                        ACCUMULATOR_REGISTER,
                        DECODED_OP_REGISTER,
                    )
                    .update_flags()
                    .with_op_size(arm_asm::OpSize::Size32)
                    .generate(),
                );
                self.opcode_stream
                    .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::And => {
                self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
                self.opcode_stream.push_opcode(
                    arm_asm::And::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Ora => {
                self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
                self.opcode_stream.push_opcode(
                    arm_asm::Or::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Eor => {
                self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
                self.opcode_stream.push_opcode(
                    arm_asm::Xor::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Tax => {
                // TODO(javier-varez): Update flags
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(X_REGISTER, ACCUMULATOR_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Tay => {
                // TODO(javier-varez): Update flags
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(Y_REGISTER, ACCUMULATOR_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Tsx => {
                // TODO(javier-varez): Update flags
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(X_REGISTER, SP_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Txa => {
                // TODO(javier-varez): Update flags
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, X_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Tya => {
                // TODO(javier-varez): Update flags
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, Y_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Txs => {
                // TODO(javier-varez): Update flags
                self.opcode_stream
                    .push_opcode(arm_asm::Mov::new(SP_REGISTER, X_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::Rts => {
                // TODO(javier-varez): Pop pc from stack and set it before actually returning
                self.opcode_stream
                    .push_opcode(arm_asm::Ret::new().generate());
            }
            _ => {
                unimplemented!();
            }
        }
    }
}
