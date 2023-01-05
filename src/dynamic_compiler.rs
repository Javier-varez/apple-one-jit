use crate::arm_asm;
use crate::block::{Block, Marker, OpCodeStream};
use crate::compiled_block::{CompiledBlock, LocationRange};
use crate::memory::{MemoryInterface, TargetAddress, VirtualAddress};
use crate::mos6502;
use crate::virtual_machine::ExitReason;
use std::collections::HashMap;

#[derive(Debug)]
pub enum Error {
    DecodingError(mos6502::Error),
}

impl From<mos6502::Error> for Error {
    fn from(mos_error: mos6502::Error) -> Self {
        Error::DecodingError(mos_error)
    }
}

pub struct Compiler<'a, T: MemoryInterface> {
    decoder: mos6502::InstrDecoder,
    memory_interface: &'a mut T,
    trampolines: Trampolines,
    opcode_stream: OpCodeStream,
    instr_map: HashMap<u16, VirtualAddress>,
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

// Temporary registers. Wiped out by function calls
const TEMP_REG0: arm_asm::Register = arm_asm::Register::X3;

// This is an intra-procedure register, used for the trampoline.
const TRAMPOLINE_TARGET: arm_asm::Register = arm_asm::Register::X17;

// VM Exit reason
const EXIT_REASON_REG: arm_asm::Register = arm_asm::Register::X0;

type Trampoline = Marker;

struct Trampolines {
    write_8_bit: Trampoline,
    read_8_bit: Trampoline,
    read_16_bit: Trampoline,
    cond_branch: Trampoline,
}

impl<'a, T: MemoryInterface + 'a> Compiler<'a, T> {
    /// Constructs a new dynamic re-compiler
    pub fn new(block: Block, memory_interface: &'a mut T) -> Self {
        let mut opcode_stream = block.into_stream();
        let trampolines = Self::emit_trampolines(&mut opcode_stream);
        Self {
            decoder: mos6502::InstrDecoder::new(),
            memory_interface,
            trampolines,
            opcode_stream,
            instr_map: HashMap::new(),
        }
    }

    /// Translates the given mos6502 machine code into native arm64 machine code.
    pub fn translate_code(mut self, start_address: TargetAddress) -> Result<CompiledBlock, Error> {
        let mut address = start_address;
        let mut current_instr_address = start_address;
        let mut should_continue = true;
        while should_continue {
            let instr_or_error = self
                .decoder
                .feed(self.memory_interface.read_8_bits(address));

            const TEST_END_OPCODE: u8 = 0x02;
            // Jam opcode 0x02 is treated differently so that tests can stop without returning or
            // jumping
            if matches!(
                instr_or_error,
                Err(mos6502::Error::JamOpCode(TEST_END_OPCODE))
            ) {
                let marker = self.opcode_stream.get_current_marker();
                self.instr_map.insert(
                    current_instr_address,
                    self.opcode_stream.marker_address(&marker),
                );
                self.emit_vm_exit(ExitReason::TestEnd);
                should_continue = false;
            } else if let Some(instr) = instr_or_error? {
                println!("Decoded instr {:?}", instr);
                let marker = self.opcode_stream.get_current_marker();
                self.instr_map.insert(
                    current_instr_address,
                    self.opcode_stream.marker_address(&marker),
                );
                self.emit_instruction_address_mode(&instr);
                self.emit_instruction(&instr);
                if !instr.opcode.base_instruction().is_branching_op() {
                    // Conditional branches need to handle this internally. Jumps simply don't need
                    // this because they always set the PC
                    self.emit_increment_pc(&instr);
                }
                should_continue = self.should_continue(&instr);
                current_instr_address = address.wrapping_add(1);
            }
            address = address.wrapping_add(1);
        }

        Ok(CompiledBlock::new(
            self.opcode_stream.into_executable_code(),
            LocationRange::new(start_address, address),
            self.instr_map,
        ))
    }

    fn should_continue(&self, instruction: &mos6502::Instruction) -> bool {
        use mos6502::instructions::BaseInstruction;
        match instruction.opcode.base_instruction() {
            BaseInstruction::Jsr
            | BaseInstruction::Jmp
            | BaseInstruction::Brk
            | BaseInstruction::Rti
            | BaseInstruction::Rts => false,
            _ => true,
        }
    }

    fn emit_trampolines(opcode_stream: &mut OpCodeStream) -> Trampolines {
        let forward_jump_marker = opcode_stream.add_undefined_instruction();

        let read_8_bit_func: *const () = <T as MemoryInterface>::read_8_bits as *const _;
        let read_16_bit_func: *const () = <T as MemoryInterface>::read_16_bits as *const _;
        let write_8_bit_func: *const () = <T as MemoryInterface>::write_8_bits as *const _;
        let read_8_bit_marker = opcode_stream.push_pointer(read_8_bit_func);
        let read_16_bit_marker = opcode_stream.push_pointer(read_16_bit_func);
        let write_8_bit_marker = opcode_stream.push_pointer(write_8_bit_func);

        let conditional_branch_marker = opcode_stream
            .push_opcode(arm_asm::Mov::new(PC_REGISTER, DECODED_OP_REGISTER).generate());
        opcode_stream.push_opcode(
            arm_asm::Movz::new(EXIT_REASON_REG)
                .with_immediate(arm_asm::Immediate::new(
                    ExitReason::BranchInstruction as u64,
                ))
                .generate(),
        );
        opcode_stream.push_opcode(arm_asm::Ret::new().generate());

        let current_marker = opcode_stream.get_current_marker();
        let jump_target = opcode_stream.relative_distance(&current_marker, &forward_jump_marker);

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
            cond_branch: conditional_branch_marker,
        }
    }

    fn emit_context_save(&mut self) {
        self.opcode_stream.push_opcode(
            arm_asm::Sub::new(arm_asm::Register::SpZr, arm_asm::Register::SpZr)
                .with_immediate(arm_asm::Immediate::new(0x10))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            // link register
            arm_asm::Strd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    arm_asm::Immediate::new(0),
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
                    arm_asm::Immediate::new(1),
                ))
                .generate(),
        );
    }

    fn emit_context_restore(&mut self) {
        self.opcode_stream.push_opcode(
            // link register
            arm_asm::Ldrd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    arm_asm::Immediate::new(1),
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
                    arm_asm::Immediate::new(0),
                ))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(arm_asm::Register::SpZr, arm_asm::Register::SpZr)
                .with_immediate(arm_asm::Immediate::new(0x10))
                .generate(),
        );
    }

    fn emit_function_call(&mut self, target: Trampoline) {
        self.emit_context_save();
        // Load address of the target trampoline
        let current_location = self.opcode_stream.add_undefined_instruction();
        let target_relative_distance = self
            .opcode_stream
            .relative_distance(&target, &current_location);
        self.opcode_stream.patch_opcode(
            &current_location,
            arm_asm::LdrLit::new(
                TRAMPOLINE_TARGET,
                arm_asm::SignedImmediate::new(target_relative_distance),
            )
            .generate(),
        );
        // Jump to it
        self.opcode_stream.push_opcode(
            arm_asm::Branch::new()
                .with_register(TRAMPOLINE_TARGET)
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
        if target_reg != CALL_RESULT0 {
            self.opcode_stream
                .push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
        }
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
                .with_immediate(arm_asm::Immediate::new(u8::MAX.into()))
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
                .with_immediate(arm_asm::Immediate::new(u8::MAX.into()))
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
        if value == u8::MAX {
            // We have wraparound and needs special handling
            self.emit_8_byte_load_immediate_addr(DECODED_OP_REGISTER, u8::MAX.into());
            self.emit_8_byte_load_immediate_addr(SCRATCH_REGISTER, u8::MIN.into());
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

    fn emit_addr_mode_relative(&mut self, instruction: &mos6502::Instruction) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Relative", instruction.operand);
                };

        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                .with_shifted_reg(PC_REGISTER)
                .generate(),
        );
        // Should take into account the next instruction size too
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(
                    instruction.instruction_size() as u64
                ))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xFFFF))
                .generate(),
        );
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

        // Make sure it is actually zero-page
        self.opcode_stream.push_opcode(
            arm_asm::And::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(u8::MAX.into()))
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

    const fn flag_offset(flag: mos6502::Flags) -> usize {
        match flag {
            mos6502::Flags::N => 31,
            mos6502::Flags::Z => 30,
            mos6502::Flags::C => 29,
            mos6502::Flags::V => 28,
        }
    }

    fn inverted_flags_mask(flags: &[mos6502::Flags]) -> u64 {
        Self::flags_mask(flags) ^ 0xFFFF_FFFF_FFFF_FFFF
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
            mos6502::instructions::BaseInstruction::Sta => ACCUMULATOR_REGISTER,
            mos6502::instructions::BaseInstruction::Stx => X_REGISTER,
            mos6502::instructions::BaseInstruction::Sty => Y_REGISTER,
            _ => {
                unreachable!()
            }
        };

        assert_eq!(
            instruction.opcode.addressing_mode().operand_type(),
            mos6502::addressing_modes::OperandType::Memory
        );
        self.emit_8_byte_store(DECODED_OP_REGISTER, source_reg);
    }

    fn emit_adc_instruction(&mut self, instruction: &mos6502::Instruction) {
        self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER).generate());
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
        self.opcode_stream.push_opcode(
            arm_asm::And::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xFF))
                .generate(),
        );
    }

    fn emit_sbc_instruction(&mut self, instruction: &mos6502::Instruction) {
        self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Sbc::new(
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
        self.opcode_stream.push_opcode(
            arm_asm::And::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xFF))
                .generate(),
        );
    }

    fn emit_and_instruction(&mut self, instruction: &mos6502::Instruction) {
        self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);

        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.opcode_stream.push_opcode(
            arm_asm::And::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                .with_shifted_reg(DECODED_OP_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_ora_instruction(&mut self, instruction: &mos6502::Instruction) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                .with_shifted_reg(DECODED_OP_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_eor_instruction(&mut self, instruction: &mos6502::Instruction) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);
        self.opcode_stream.push_opcode(
            arm_asm::Xor::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                .with_shifted_reg(DECODED_OP_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_tax_instruction(&mut self) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(X_REGISTER, ACCUMULATOR_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(X_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_tay_instruction(&mut self) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(Y_REGISTER, ACCUMULATOR_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(Y_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_tsx_instruction(&mut self) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(X_REGISTER, SP_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(X_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_txa_instruction(&mut self) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, X_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_txs_instruction(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(SP_REGISTER, X_REGISTER).generate());
    }

    fn emit_tya_instruction(&mut self) {
        let saved_flags = [mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(&saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, Y_REGISTER).generate());
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());

        self.emit_restore_flags(&saved_flags);
    }

    fn emit_clc_instruction(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(&[
                    mos6502::Flags::C,
                ])))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());
    }

    fn emit_clv_instruction(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(&[
                    mos6502::Flags::V,
                ])))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());
    }

    fn emit_sec_instruction(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::C,
                ])))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());
    }

    fn emit_bit_instruction(&mut self) {
        self.emit_8_byte_load(DECODED_OP_REGISTER, DECODED_OP_REGISTER);

        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());

        // Keep only carry
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::C,
                ])))
                .generate(),
        );

        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(1 << 7))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(SCRATCH_REGISTER_2).generate());

        let branch_to_bit6_marker = self.opcode_stream.add_undefined_instruction();

        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::N,
                ])))
                .generate(),
        );

        let bit6_marker = self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(1 << 6))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(SCRATCH_REGISTER_2).generate());

        self.opcode_stream.patch_opcode(
            &branch_to_bit6_marker,
            arm_asm::Branch::new()
                .with_immediate(arm_asm::SignedImmediate::new(
                    self.opcode_stream
                        .relative_distance(&bit6_marker, &branch_to_bit6_marker),
                ))
                .iff(arm_asm::Condition::Eq)
                .generate(),
        );

        let branch_to_zero_marker = self.opcode_stream.add_undefined_instruction();

        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::V,
                ])))
                .generate(),
        );

        let zero_marker = self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, DECODED_OP_REGISTER)
                .with_shifted_reg(ACCUMULATOR_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(SCRATCH_REGISTER_2).generate());

        self.opcode_stream.patch_opcode(
            &branch_to_zero_marker,
            arm_asm::Branch::new()
                .with_immediate(arm_asm::SignedImmediate::new(
                    self.opcode_stream
                        .relative_distance(&zero_marker, &branch_to_zero_marker),
                ))
                .iff(arm_asm::Condition::Eq)
                .generate(),
        );

        let branch_to_set_nzcv = self.opcode_stream.add_undefined_instruction();

        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::Z,
                ])))
                .generate(),
        );

        let set_nzcv_marker = self
            .opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());

        self.opcode_stream.patch_opcode(
            &branch_to_set_nzcv,
            arm_asm::Branch::new()
                .with_immediate(arm_asm::SignedImmediate::new(
                    self.opcode_stream
                        .relative_distance(&set_nzcv_marker, &branch_to_set_nzcv),
                ))
                .iff(arm_asm::Condition::Ne)
                .generate(),
        );
    }

    const SR_N_OFFSET: usize = 7;
    const SR_V_OFFSET: usize = 6;
    const SR_B_OFFSET: usize = 4;
    const SR_D_OFFSET: usize = 3;
    const SR_I_OFFSET: usize = 2;
    const SR_Z_OFFSET: usize = 1;
    const SR_C_OFFSET: usize = 0;

    const FLAG_TRANSLATIONS: [(usize, usize); 4] = [
        (Self::flag_offset(mos6502::Flags::N), Self::SR_N_OFFSET),
        (Self::flag_offset(mos6502::Flags::V), Self::SR_V_OFFSET),
        (Self::flag_offset(mos6502::Flags::Z), Self::SR_Z_OFFSET),
        (Self::flag_offset(mos6502::Flags::C), Self::SR_C_OFFSET),
    ];

    fn emit_build_status_register(&mut self, dest_register: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());

        // Set initial value
        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(dest_register)
                .with_immediate(arm_asm::Immediate::new(
                    // FIXME(javier-varez): Interrupt flag should not be hardcoded to 0
                    (1 << Self::SR_B_OFFSET) | (0 << Self::SR_D_OFFSET) | (0 << Self::SR_I_OFFSET),
                ))
                .generate(),
        );

        for (source_flag, dest_flag) in Self::FLAG_TRANSLATIONS {
            self.opcode_stream.push_opcode(
                arm_asm::Lsr::new(
                    SCRATCH_REGISTER_2,
                    SCRATCH_REGISTER,
                    arm_asm::Immediate::new((source_flag - dest_flag) as u64),
                )
                .generate(),
            );

            self.opcode_stream.push_opcode(
                arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                    .with_immediate(arm_asm::Immediate::new(1 << dest_flag))
                    .generate(),
            );

            self.opcode_stream.push_opcode(
                arm_asm::Or::new(dest_register, dest_register)
                    .with_shifted_reg(SCRATCH_REGISTER_2)
                    .generate(),
            );
        }
    }

    fn emit_restore_status_reg(&mut self, src_reg: arm_asm::Register) {
        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0))
                .generate(),
        );

        for (dest_flag, source_flag) in Self::FLAG_TRANSLATIONS {
            self.opcode_stream.push_opcode(
                arm_asm::And::new(SCRATCH_REGISTER_2, src_reg)
                    .with_immediate(arm_asm::Immediate::new(1 << source_flag))
                    .generate(),
            );

            self.opcode_stream.push_opcode(
                arm_asm::Lsl::new(
                    SCRATCH_REGISTER_2,
                    SCRATCH_REGISTER_2,
                    arm_asm::Immediate::new((dest_flag - source_flag) as u64),
                )
                .generate(),
            );

            self.opcode_stream.push_opcode(
                arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                    .with_shifted_reg(SCRATCH_REGISTER_2)
                    .generate(),
            );
        }

        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());
    }

    fn emit_increment_sp(&mut self) {
        // Increment sp with wraparound
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(SP_REGISTER, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(1))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SP_REGISTER, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
    }

    fn emit_decrement_sp(&mut self) {
        // Decrement sp with wraparound
        self.opcode_stream.push_opcode(
            arm_asm::Sub::new(SP_REGISTER, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(1))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SP_REGISTER, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
    }

    fn emit_php_instruction(&mut self) {
        // Call write_8_bit function
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.emit_build_status_register(CALL_ARG2);
        self.emit_function_call(self.trampolines.write_8_bit.clone());

        self.emit_decrement_sp();
    }

    fn emit_pha_instruction(&mut self) {
        // Call write_8_bit function
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG2, ACCUMULATOR_REGISTER).generate());
        self.emit_function_call(self.trampolines.write_8_bit.clone());

        self.emit_decrement_sp();
    }

    fn emit_plp_instruction(&mut self) {
        self.emit_increment_sp();

        // Call read_8_bit function
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.emit_function_call(self.trampolines.read_8_bit.clone());
        self.emit_restore_status_reg(CALL_RESULT0);
    }

    fn emit_pla_instruction(&mut self) {
        self.emit_increment_sp();

        // Call read_8_bit function
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.emit_function_call(self.trampolines.read_8_bit.clone());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(ACCUMULATOR_REGISTER, CALL_RESULT0).generate());

        // Update flags
        let saved_flags = &[mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(saved_flags);
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());
        self.emit_restore_flags(saved_flags);
    }

    fn emit_dec_instruction(&mut self) {
        self.emit_8_byte_load(CALL_RESULT0, DECODED_OP_REGISTER);

        self.opcode_stream.push_opcode(
            arm_asm::Sub::new(CALL_RESULT0, CALL_RESULT0)
                .with_immediate(arm_asm::Immediate::new(1))
                .generate(),
        );

        let saved_flags = &[mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(saved_flags);
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(CALL_RESULT0).generate());
        self.emit_restore_flags(saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(SCRATCH_REGISTER, CALL_RESULT0).generate());
        self.emit_8_byte_store(DECODED_OP_REGISTER, SCRATCH_REGISTER);
    }

    fn emit_inc_instruction(&mut self) {
        self.emit_8_byte_load(CALL_RESULT0, DECODED_OP_REGISTER);

        self.opcode_stream.push_opcode(
            arm_asm::Add::new(CALL_RESULT0, CALL_RESULT0)
                .with_immediate(arm_asm::Immediate::new(1))
                .generate(),
        );

        let saved_flags = &[mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(saved_flags);
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(CALL_RESULT0).generate());
        self.emit_restore_flags(saved_flags);

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(SCRATCH_REGISTER, CALL_RESULT0).generate());
        self.emit_8_byte_store(DECODED_OP_REGISTER, SCRATCH_REGISTER);
    }

    fn emit_dec_index_instruction(&mut self, register: arm_asm::Register) {
        self.opcode_stream.push_opcode(
            arm_asm::Sub::new(register, register)
                .with_immediate(arm_asm::Immediate::new(1))
                .generate(),
        );

        let saved_flags = &[mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(saved_flags);
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(register).generate());
        self.emit_restore_flags(saved_flags);

        // Make sure it is still 8-bit
        self.opcode_stream.push_opcode(
            arm_asm::And::new(register, register)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
    }

    fn emit_inc_index_instruction(&mut self, register: arm_asm::Register) {
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(register, register)
                .with_immediate(arm_asm::Immediate::new(1))
                .generate(),
        );

        let saved_flags = &[mos6502::Flags::C, mos6502::Flags::V];
        self.emit_save_flags(saved_flags);
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(register).generate());
        self.emit_restore_flags(saved_flags);

        // Make sure it is still 8-bit
        self.opcode_stream.push_opcode(
            arm_asm::And::new(register, register)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
    }

    fn emit_nop_instruction(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Nop::new().generate());
    }

    fn emit_lsr_register_instruction(&mut self, register: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::V,
                ])))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Bfi::new(
                SCRATCH_REGISTER,
                register,
                arm_asm::Immediate::new(Self::flag_offset(mos6502::Flags::C) as u64),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );

        self.opcode_stream.push_opcode(
            arm_asm::Lsr::new(register, register, arm_asm::Immediate::new(1)).generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(register).generate());

        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER_2, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(&[
                    mos6502::Flags::V,
                    mos6502::Flags::C,
                ])))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_shifted_reg(SCRATCH_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER_2).generate());
    }

    fn emit_lsr_instruction(&mut self, instruction: &mos6502::Instruction) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::None => {
                // Accumulator
                self.emit_lsr_register_instruction(ACCUMULATOR_REGISTER);
            }
            mos6502::addressing_modes::OperandType::Memory => {
                // Load into register from memory
                self.emit_8_byte_load(TEMP_REG0, DECODED_OP_REGISTER);
                self.emit_lsr_register_instruction(TEMP_REG0);
                self.emit_8_byte_store(DECODED_OP_REGISTER, TEMP_REG0);
            }
            mos6502::addressing_modes::OperandType::Value => {
                unreachable!();
            }
        }
    }

    fn emit_asl_register_instruction(&mut self, register: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::V,
                ])))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Lsr::new(SCRATCH_REGISTER_2, register, arm_asm::Immediate::new(7)).generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Bfi::new(
                SCRATCH_REGISTER,
                SCRATCH_REGISTER_2,
                arm_asm::Immediate::new(Self::flag_offset(mos6502::Flags::C) as u64),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );

        self.opcode_stream.push_opcode(
            arm_asm::Lsl::new(register, register, arm_asm::Immediate::new(1)).generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(register, register)
                .with_immediate(arm_asm::Immediate::new(0xFF))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(register).generate());

        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER_2, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(&[
                    mos6502::Flags::V,
                    mos6502::Flags::C,
                ])))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_shifted_reg(SCRATCH_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER_2).generate());
    }

    fn emit_asl_instruction(&mut self, instruction: &mos6502::Instruction) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::None => {
                // Accumulator
                self.emit_asl_register_instruction(ACCUMULATOR_REGISTER);
            }
            mos6502::addressing_modes::OperandType::Memory => {
                // Load into register from memory
                self.emit_8_byte_load(TEMP_REG0, DECODED_OP_REGISTER);
                self.emit_asl_register_instruction(TEMP_REG0);
                self.emit_8_byte_store(DECODED_OP_REGISTER, TEMP_REG0);
            }
            mos6502::addressing_modes::OperandType::Value => {
                unreachable!();
            }
        }
    }

    fn emit_rol_register_instruction(&mut self, register: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::V,
                    mos6502::Flags::C,
                ])))
                .generate(),
        );

        self.opcode_stream.push_opcode(
            arm_asm::Lsl::new(register, register, arm_asm::Immediate::new(1)).generate(),
        );

        // Copy carry to bit 0
        self.opcode_stream.push_opcode(
            arm_asm::Bfxil::new(
                register,
                SCRATCH_REGISTER,
                arm_asm::Immediate::new(Self::flag_offset(mos6502::Flags::C) as u64),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );

        // Create new carry bit
        self.opcode_stream.push_opcode(
            arm_asm::Lsr::new(SCRATCH_REGISTER_2, register, arm_asm::Immediate::new(8)).generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Bfi::new(
                SCRATCH_REGISTER,
                SCRATCH_REGISTER_2,
                arm_asm::Immediate::new(Self::flag_offset(mos6502::Flags::C) as u64),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );

        self.opcode_stream.push_opcode(
            arm_asm::And::new(register, register)
                .with_immediate(arm_asm::Immediate::new(0xFF))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(register).generate());

        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER_2, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(&[
                    mos6502::Flags::V,
                    mos6502::Flags::C,
                ])))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_shifted_reg(SCRATCH_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER_2).generate());
    }

    fn emit_rol_instruction(&mut self, instruction: &mos6502::Instruction) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::None => {
                // Accumulator
                self.emit_rol_register_instruction(ACCUMULATOR_REGISTER);
            }
            mos6502::addressing_modes::OperandType::Memory => {
                // Load into register from memory
                self.emit_8_byte_load(TEMP_REG0, DECODED_OP_REGISTER);
                self.emit_rol_register_instruction(TEMP_REG0);
                self.emit_8_byte_store(DECODED_OP_REGISTER, TEMP_REG0);
            }
            mos6502::addressing_modes::OperandType::Value => {
                unreachable!();
            }
        }
    }

    fn emit_ror_register_instruction(&mut self, register: arm_asm::Register) {
        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(Self::flags_mask(&[
                    mos6502::Flags::V,
                    mos6502::Flags::C,
                ])))
                .generate(),
        );

        // Save the previous carry in bit 0
        self.opcode_stream.push_opcode(
            arm_asm::Bfxil::new(
                SCRATCH_REGISTER,
                SCRATCH_REGISTER,
                arm_asm::Immediate::new(Self::flag_offset(mos6502::Flags::C) as u64),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );

        // Set the new carry bit from bit 0 of the current register value
        self.opcode_stream.push_opcode(
            arm_asm::Bfi::new(
                SCRATCH_REGISTER,
                register,
                arm_asm::Immediate::new(Self::flag_offset(mos6502::Flags::C) as u64),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );

        self.opcode_stream.push_opcode(
            arm_asm::Lsr::new(register, register, arm_asm::Immediate::new(1)).generate(),
        );
        // Copy carry bit into bit 7 of the result
        self.opcode_stream.push_opcode(
            arm_asm::Bfi::new(
                register,
                SCRATCH_REGISTER,
                arm_asm::Immediate::new(7),
                arm_asm::Immediate::new(1),
            )
            .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(register).generate());

        self.opcode_stream
            .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER_2, arm_asm::NZCV).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(&[
                    mos6502::Flags::V,
                    mos6502::Flags::C,
                ])))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                .with_shifted_reg(SCRATCH_REGISTER)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER_2).generate());
    }

    fn emit_ror_instruction(&mut self, instruction: &mos6502::Instruction) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::None => {
                // Accumulator
                self.emit_ror_register_instruction(ACCUMULATOR_REGISTER);
            }
            mos6502::addressing_modes::OperandType::Memory => {
                // Load into register from memory
                self.emit_8_byte_load(TEMP_REG0, DECODED_OP_REGISTER);
                self.emit_ror_register_instruction(TEMP_REG0);
                self.emit_8_byte_store(DECODED_OP_REGISTER, TEMP_REG0);
            }
            mos6502::addressing_modes::OperandType::Value => {
                unreachable!();
            }
        }
    }

    fn emit_cmp_instruction(&mut self, instruction: &mos6502::Instruction) {
        let register = match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Cmp => ACCUMULATOR_REGISTER,
            mos6502::instructions::BaseInstruction::Cpx => X_REGISTER,
            mos6502::instructions::BaseInstruction::Cpy => Y_REGISTER,
            _ => unreachable!(),
        };

        self.emit_deref_if_needed(instruction, DECODED_OP_REGISTER);

        self.emit_save_flags(&[mos6502::Flags::V]);
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(register, register).generate());
        self.opcode_stream
            .push_opcode(arm_asm::Sxtb::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Sub::new(TEMP_REG0, register)
                .with_shifted_reg(DECODED_OP_REGISTER)
                .update_flags()
                .with_op_size(arm_asm::OpSize::Size32)
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::SetF8::new(TEMP_REG0).generate());
        self.opcode_stream.push_opcode(
            arm_asm::And::new(register, register)
                .with_immediate(arm_asm::Immediate::new(0xFF))
                .generate(),
        );
        self.emit_restore_flags(&[mos6502::Flags::V]);
    }

    fn emit_vm_exit(&mut self, reason: ExitReason) {
        self.opcode_stream.push_opcode(
            arm_asm::Movz::new(EXIT_REASON_REG)
                .with_immediate(arm_asm::Immediate::new(reason as u64))
                .generate(),
        );
        self.opcode_stream
            .push_opcode(arm_asm::Ret::new().generate());
    }

    fn emit_jmp_instruction(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(PC_REGISTER, DECODED_OP_REGISTER).generate());
        self.emit_vm_exit(ExitReason::BranchInstruction);
    }

    fn emit_save_pc_in_stack(&mut self) {
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Lsr::new(CALL_ARG2, PC_REGISTER, arm_asm::Immediate::new(8)).generate(),
        );
        self.emit_function_call(self.trampolines.write_8_bit.clone());

        self.emit_decrement_sp();

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::And::new(CALL_ARG2, PC_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xFF))
                .generate(),
        );
        self.emit_function_call(self.trampolines.write_8_bit.clone());

        self.emit_decrement_sp();
    }

    fn emit_restore_pc_from_stack(&mut self) {
        self.emit_increment_sp();

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.emit_function_call(self.trampolines.read_8_bit.clone());
        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(PC_REGISTER, CALL_RESULT0).generate());

        self.emit_increment_sp();

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(CALL_ARG1, SP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0x100))
                .generate(),
        );
        self.emit_function_call(self.trampolines.read_8_bit.clone());
        self.opcode_stream.push_opcode(
            arm_asm::Lsl::new(CALL_RESULT0, CALL_RESULT0, arm_asm::Immediate::new(8)).generate(),
        );
        self.opcode_stream.push_opcode(
            arm_asm::Or::new(PC_REGISTER, PC_REGISTER)
                .with_shifted_reg(CALL_RESULT0)
                .generate(),
        );
    }

    fn emit_jsr_instruction(&mut self) {
        // Increment PC by 3 (jsr uses absolute addressing)
        self.opcode_stream.push_opcode(
            arm_asm::Add::new(PC_REGISTER, PC_REGISTER)
                .with_immediate(arm_asm::Immediate::new(3))
                .generate(),
        );

        self.emit_save_pc_in_stack();

        self.opcode_stream
            .push_opcode(arm_asm::Mov::new(PC_REGISTER, DECODED_OP_REGISTER).generate());
        self.emit_vm_exit(ExitReason::BranchInstruction);
    }

    fn emit_rts_instruction(&mut self) {
        self.emit_restore_pc_from_stack();
        self.emit_vm_exit(ExitReason::ReturnInstruction);
    }

    fn emit_branch_instruction(
        &mut self,
        instruction: &mos6502::Instruction,
        condition: arm_asm::Condition,
    ) {
        let current_marker = self.opcode_stream.get_current_marker();
        let jump_target = self
            .opcode_stream
            .relative_distance(&self.trampolines.cond_branch, &current_marker);
        // Jump to some address where all branches are handled
        self.opcode_stream.push_opcode(
            arm_asm::Branch::new()
                // TODO(javier-varez): Fix this with a valid jump target
                .with_immediate(arm_asm::SignedImmediate::new(jump_target))
                .iff(condition)
                .generate(),
        );
        self.emit_increment_pc(instruction);
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
                self.emit_adc_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Sbc => {
                self.emit_sbc_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::And => {
                self.emit_and_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Ora => {
                self.emit_ora_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Eor => {
                self.emit_eor_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Tax => {
                self.emit_tax_instruction();
            }
            mos6502::instructions::BaseInstruction::Tay => {
                self.emit_tay_instruction();
            }
            mos6502::instructions::BaseInstruction::Tsx => {
                self.emit_tsx_instruction();
            }
            mos6502::instructions::BaseInstruction::Txa => {
                self.emit_txa_instruction();
            }
            mos6502::instructions::BaseInstruction::Tya => {
                self.emit_tya_instruction();
            }
            mos6502::instructions::BaseInstruction::Txs => {
                self.emit_txs_instruction();
            }
            mos6502::instructions::BaseInstruction::Clc => {
                self.emit_clc_instruction();
            }
            mos6502::instructions::BaseInstruction::Cld => {
                // Decimal mode is not supported, therefore there's nothing to do here
            }
            mos6502::instructions::BaseInstruction::Clv => {
                self.emit_clv_instruction();
            }
            mos6502::instructions::BaseInstruction::Sec => {
                self.emit_sec_instruction();
            }
            mos6502::instructions::BaseInstruction::Sed => {
                unimplemented!("Decimal mode not supported");
            }
            mos6502::instructions::BaseInstruction::Bit => {
                self.emit_bit_instruction();
            }
            mos6502::instructions::BaseInstruction::Pha => {
                self.emit_pha_instruction();
            }
            mos6502::instructions::BaseInstruction::Pla => {
                self.emit_pla_instruction();
            }
            mos6502::instructions::BaseInstruction::Php => {
                self.emit_php_instruction();
            }
            mos6502::instructions::BaseInstruction::Plp => {
                self.emit_plp_instruction();
            }
            mos6502::instructions::BaseInstruction::Dec => {
                self.emit_dec_instruction();
            }
            mos6502::instructions::BaseInstruction::Inc => {
                self.emit_inc_instruction();
            }
            mos6502::instructions::BaseInstruction::Dex => {
                self.emit_dec_index_instruction(X_REGISTER);
            }
            mos6502::instructions::BaseInstruction::Inx => {
                self.emit_inc_index_instruction(X_REGISTER);
            }
            mos6502::instructions::BaseInstruction::Dey => {
                self.emit_dec_index_instruction(Y_REGISTER);
            }
            mos6502::instructions::BaseInstruction::Iny => {
                self.emit_inc_index_instruction(Y_REGISTER);
            }
            mos6502::instructions::BaseInstruction::Nop => {
                self.emit_nop_instruction();
            }
            mos6502::instructions::BaseInstruction::Lsr => {
                self.emit_lsr_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Asl => {
                self.emit_asl_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Rol => {
                self.emit_rol_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Ror => {
                self.emit_ror_instruction(instruction);
            }

            // Comparison instructions
            mos6502::instructions::BaseInstruction::Cmp => {
                self.emit_cmp_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Cpx => {
                self.emit_cmp_instruction(instruction);
            }
            mos6502::instructions::BaseInstruction::Cpy => {
                self.emit_cmp_instruction(instruction);
            }

            // Unconditional branching
            mos6502::instructions::BaseInstruction::Jmp => {
                self.emit_jmp_instruction();
            }
            mos6502::instructions::BaseInstruction::Jsr => {
                self.emit_jsr_instruction();
            }
            mos6502::instructions::BaseInstruction::Rts => {
                self.emit_rts_instruction();
            }

            // Conditional branching
            mos6502::instructions::BaseInstruction::Bcs => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Cs);
            }
            mos6502::instructions::BaseInstruction::Bcc => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Cc);
            }
            mos6502::instructions::BaseInstruction::Beq => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Eq);
            }
            mos6502::instructions::BaseInstruction::Bne => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Ne);
            }
            mos6502::instructions::BaseInstruction::Bmi => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Mi);
            }
            mos6502::instructions::BaseInstruction::Bpl => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Pl);
            }
            mos6502::instructions::BaseInstruction::Bvs => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Vs);
            }
            mos6502::instructions::BaseInstruction::Bvc => {
                self.emit_branch_instruction(instruction, arm_asm::Condition::Vc);
            }

            // Interrupt functionality
            mos6502::instructions::BaseInstruction::Brk => todo!(),
            mos6502::instructions::BaseInstruction::Rti => todo!(),
            mos6502::instructions::BaseInstruction::Cli => todo!(),
            mos6502::instructions::BaseInstruction::Sei => todo!(),
        }
    }
}
