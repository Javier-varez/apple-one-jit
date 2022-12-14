use std::marker::PhantomData;

use crate::arm_asm::{self, Immediate};
use crate::block::{Block, LocationRange, Marker, OpCodeStream};
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

pub struct Compiler<T: MemoryInterface> {
    decoder: mos6502::InstrDecoder,
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

impl<T: MemoryInterface> Compiler<T> {
    /// Constructs a new dynamic re-compiler
    pub fn new() -> Self {
        Self {
            decoder: mos6502::InstrDecoder::new(),
            _pd: PhantomData {},
        }
    }

    /// Translates the given mos6502 machine code into native arm64 machine code.
    pub fn translate_code(
        &mut self,
        block: &mut Block,
        address: Address,
        memory_interface: &mut dyn MemoryInterface,
    ) -> Result<LocationRange, Error> {
        block.populate(|opcode_stream| {
            Self::translate_code_impl(&mut self.decoder, opcode_stream, address, memory_interface)
        })
    }

    // Register allocation:
    // R0(8-bit) -> Accumulator
    // R1(8-bit) -> X Register
    // R2(8-bit) -> Y Register
    // R3(8-bit) -> SP Register
    // R4(16-bit) -> PC Register
    //
    // These registers below are NOT saved
    // R5(8/16-bit) -> Decoded operand (if any)
    // R6(64-bit) -> Base address of the 6502 memory map.
    // R7(8/16-bit) -> Scratch register
    // R8(8/16-bit) -> Scratch register 2
    fn translate_code_impl(
        decoder: &mut mos6502::InstrDecoder,
        opcode_stream: &mut OpCodeStream,
        start_address: Address,
        memory_interface: &mut dyn MemoryInterface,
    ) -> Result<LocationRange, Error> {
        let trampolines = Self::emit_trampolines(opcode_stream);

        let mut address = start_address;
        let mut should_continue = true;
        while should_continue {
            if let Some(instr) = decoder.feed(memory_interface.read_8_bits(address))? {
                println!("Decoded instr {:?}", instr);
                Self::emit_instruction_address_mode(opcode_stream, &instr, &trampolines);
                Self::emit_instruction(opcode_stream, &instr, &trampolines);
                if !instr.opcode.base_instruction().is_branching_op() {
                    // Conditional branchs need to handle this internally. Jumps simply don't need
                    // this because they always set the PC
                    Self::emit_increment_pc(opcode_stream, &instr);
                }
                should_continue = Self::should_continue(&instr);
            }
            address = address.wrapping_add(1);
        }

        // Just in case the instruction decoder did not find any isntr
        opcode_stream.push_opcode(arm_asm::Ret::new().generate());

        Ok(LocationRange::new(start_address, address))
    }

    fn should_continue(instruction: &mos6502::Instruction) -> bool {
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

    fn emit_context_save(opcode_stream: &mut OpCodeStream) {
        opcode_stream.push_opcode(
            arm_asm::Sub::new(arm_asm::Register::SpZr, arm_asm::Register::SpZr)
                .with_immediate(Immediate::new(0x10))
                .generate(),
        );
        opcode_stream.push_opcode(
            // link register
            arm_asm::Strd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(0),
                ))
                .generate(),
        );
        opcode_stream.push_opcode(
            // x30 = NZCV
            arm_asm::Mrs::new(arm_asm::Register::X30, arm_asm::NZCV).generate(),
        );
        opcode_stream.push_opcode(
            // link register
            arm_asm::Strd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(1),
                ))
                .generate(),
        );
    }

    fn emit_context_restore(opcode_stream: &mut OpCodeStream) {
        opcode_stream.push_opcode(
            // link register
            arm_asm::Ldrd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(1),
                ))
                .generate(),
        );
        opcode_stream.push_opcode(
            // NZCV = x30
            arm_asm::Msr::new(arm_asm::NZCV, arm_asm::Register::X30).generate(),
        );
        opcode_stream.push_opcode(
            // link register
            arm_asm::Ldrd::new(arm_asm::Register::X30, arm_asm::Register::SpZr)
                .with_mode(arm_asm::MemoryAccessMode::UnsignedOffsetImmediate(
                    Immediate::new(0),
                ))
                .generate(),
        );
        opcode_stream.push_opcode(
            arm_asm::Add::new(arm_asm::Register::SpZr, arm_asm::Register::SpZr)
                .with_immediate(Immediate::new(0x10))
                .generate(),
        );
    }

    fn emit_function_call(opcode_stream: &mut OpCodeStream, target: &Trampoline) {
        Self::emit_context_save(opcode_stream);
        // Load address of the target trampoline
        let current_location = opcode_stream.add_marker();
        let target_relative_distance = opcode_stream.relative_distance(target, &current_location);
        opcode_stream.patch_opcode(
            &current_location,
            arm_asm::LdrLit::new(
                TRAMPOLIN_TARGET,
                arm_asm::SignedImmediate::new(target_relative_distance),
            )
            .generate(),
        );
        // Jump to it (saving the LR)
        opcode_stream.push_opcode(
            arm_asm::Branch::new()
                .with_register(TRAMPOLIN_TARGET)
                .link()
                .generate(),
        );
        Self::emit_context_restore(opcode_stream);
    }

    fn emit_8_byte_load(
        opcode_stream: &mut OpCodeStream,
        trampolines: &Trampolines,
        target_reg: arm_asm::Register,
        addr_reg: arm_asm::Register,
    ) {
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG1, addr_reg).generate());
        Self::emit_function_call(opcode_stream, &trampolines.read_8_bit);
        opcode_stream.push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
    }

    fn emit_8_byte_load_immediate_addr(
        opcode_stream: &mut OpCodeStream,
        trampolines: &Trampolines,
        target_reg: arm_asm::Register,
        address: u16,
    ) {
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        opcode_stream.push_opcode(
            arm_asm::Movz::new(CALL_ARG1)
                .with_immediate(arm_asm::Immediate::new(address as u64))
                .generate(),
        );
        Self::emit_function_call(opcode_stream, &trampolines.read_8_bit);
        opcode_stream.push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
    }

    fn emit_16_byte_load_immediate_addr(
        opcode_stream: &mut OpCodeStream,
        trampolines: &Trampolines,
        target_reg: arm_asm::Register,
        address: u16,
    ) {
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        opcode_stream.push_opcode(
            arm_asm::Movz::new(CALL_ARG1)
                .with_immediate(arm_asm::Immediate::new(address as u64))
                .generate(),
        );
        Self::emit_function_call(opcode_stream, &trampolines.read_16_bit);
        opcode_stream.push_opcode(arm_asm::Mov::new(target_reg, CALL_RESULT0).generate());
    }

    fn emit_8_byte_store(
        opcode_stream: &mut OpCodeStream,
        trampolines: &Trampolines,
        address_reg: arm_asm::Register,
        data_reg: arm_asm::Register,
    ) {
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG0, MEMORY_INTERFACE_REG).generate());
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG1, address_reg).generate());
        opcode_stream.push_opcode(arm_asm::Mov::new(CALL_ARG2, data_reg).generate());
        Self::emit_function_call(opcode_stream, &trampolines.write_8_bit);
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

    fn emit_addr_mode_accumulator(opcode_stream: &mut OpCodeStream) {
        opcode_stream
            .push_opcode(arm_asm::Mov::new(DECODED_OP_REGISTER, ACCUMULATOR_REGISTER).generate());
    }

    fn emit_addr_mode_absolute(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
    ) {
        let mos6502::addressing_modes::Operand::U16(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Absolute", instruction.operand);
                };

        opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    fn emit_addr_mode_absolute_indexed(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        index_reg: arm_asm::Register,
    ) {
        let mos6502::addressing_modes::Operand::U16(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::AbsoluteIndexed", instruction.operand);
                };

        opcode_stream.push_opcode(
            arm_asm::Movz::new(SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
        opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, index_reg)
                .with_shifted_reg(SCRATCH_REGISTER)
                .generate(),
        );
    }

    fn emit_addr_mode_immediate(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
    ) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!(
                        "Unexpected operand {:?} for AddressingMode::Immediate",
                        instruction.operand
                    )
                };

        opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    fn emit_addr_mode_indirect(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
    ) {
        let mos6502::addressing_modes::Operand::U16(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Indirect", instruction.operand);
                };

        Self::emit_16_byte_load_immediate_addr(
            opcode_stream,
            trampolines,
            DECODED_OP_REGISTER,
            value,
        );
    }

    fn emit_addr_mode_x_indexed_indirect(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
    ) {
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

        opcode_stream.push_opcode(
            arm_asm::Add::new(SCRATCH_REGISTER, X_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
        opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
        Self::emit_8_byte_load(
            opcode_stream,
            trampolines,
            DECODED_OP_REGISTER,
            SCRATCH_REGISTER,
        );
        opcode_stream.push_opcode(
            arm_asm::Add::new(SCRATCH_REGISTER, X_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value.wrapping_add(1) as u64))
                .generate(),
        );
        opcode_stream.push_opcode(
            arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                .with_immediate(arm_asm::Immediate::new(0xff))
                .generate(),
        );
        Self::emit_8_byte_load(
            opcode_stream,
            trampolines,
            SCRATCH_REGISTER,
            SCRATCH_REGISTER,
        );
        opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                .with_shifted_reg(SCRATCH_REGISTER)
                .with_shift(arm_asm::RegShift::Lsl(8))
                .generate(),
        );
    }

    fn emit_addr_mode_indirect_y_indexed(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
    ) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::IndirectYIndexed", instruction.operand);
                };

        // (u8 operand) => addr in zero page + y reg => the address we want
        if value == 0xff {
            // We have wraparound and needs special handling
            Self::emit_8_byte_load_immediate_addr(
                opcode_stream,
                trampolines,
                DECODED_OP_REGISTER,
                0xff,
            );
            Self::emit_8_byte_load_immediate_addr(
                opcode_stream,
                trampolines,
                SCRATCH_REGISTER,
                0x00,
            );
            opcode_stream.push_opcode(
                arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                    .with_shifted_reg(SCRATCH_REGISTER)
                    .with_shift(arm_asm::RegShift::Lsl(8))
                    .generate(),
            );
        } else {
            Self::emit_16_byte_load_immediate_addr(
                opcode_stream,
                trampolines,
                DECODED_OP_REGISTER,
                value as u16,
            );
            opcode_stream.push_opcode(
                arm_asm::Add::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER)
                    .with_shifted_reg(Y_REGISTER)
                    .generate(),
            );
        }
    }

    fn emit_addr_mode_relative(
        _opcode_stream: &mut OpCodeStream,
        _instruction: &mos6502::Instruction,
    ) {
        // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
        unimplemented!();
    }

    fn emit_addr_mode_zeropage(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
    ) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::Zeropage", instruction.operand);
                };

        opcode_stream.push_opcode(
            arm_asm::Movz::new(DECODED_OP_REGISTER)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    fn emit_addr_mode_zeropage_indexed(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        index_reg: arm_asm::Register,
    ) {
        let mos6502::addressing_modes::Operand::U8(value) = instruction.operand else {
                    panic!("Unexpected operand type {:?} for AddressingMode::ZeropageIndexed", instruction.operand);
                };

        opcode_stream.push_opcode(
            arm_asm::Add::new(DECODED_OP_REGISTER, index_reg)
                .with_immediate(arm_asm::Immediate::new(value as u64))
                .generate(),
        );
    }

    /// Takes the operand and makes sure it ends up in register R4 of the host processor
    fn emit_instruction_address_mode(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
    ) {
        match &instruction.opcode.addressing_mode() {
            mos6502::addressing_modes::AddressingMode::Accumulator => {
                Self::emit_addr_mode_accumulator(opcode_stream);
            }
            mos6502::addressing_modes::AddressingMode::Absolute => {
                Self::emit_addr_mode_absolute(opcode_stream, instruction);
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteXIndexed => {
                Self::emit_addr_mode_absolute_indexed(opcode_stream, instruction, X_REGISTER);
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteYIndexed => {
                Self::emit_addr_mode_absolute_indexed(opcode_stream, instruction, Y_REGISTER);
            }
            mos6502::addressing_modes::AddressingMode::Immediate => {
                Self::emit_addr_mode_immediate(opcode_stream, instruction);
            }
            mos6502::addressing_modes::AddressingMode::Implied => {
                // Nothing to do here! :)
            }
            mos6502::addressing_modes::AddressingMode::Indirect => {
                Self::emit_addr_mode_indirect(opcode_stream, instruction, trampolines);
            }
            mos6502::addressing_modes::AddressingMode::XIndexedIndirect => {
                Self::emit_addr_mode_x_indexed_indirect(opcode_stream, instruction, trampolines);
            }
            mos6502::addressing_modes::AddressingMode::IndirectYIndexed => {
                Self::emit_addr_mode_indirect_y_indexed(opcode_stream, instruction, trampolines);
            }
            mos6502::addressing_modes::AddressingMode::Relative => {
                Self::emit_addr_mode_relative(opcode_stream, instruction);
            }
            mos6502::addressing_modes::AddressingMode::Zeropage => {
                Self::emit_addr_mode_zeropage(opcode_stream, instruction);
            }
            mos6502::addressing_modes::AddressingMode::ZeropageXIndexed => {
                Self::emit_addr_mode_zeropage_indexed(opcode_stream, instruction, X_REGISTER);
            }
            mos6502::addressing_modes::AddressingMode::ZeropageYIndexed => {
                Self::emit_addr_mode_zeropage_indexed(opcode_stream, instruction, Y_REGISTER);
            }
        }
    }

    fn emit_deref_if_needed(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        dest_register: arm_asm::Register,
        trampolines: &Trampolines,
    ) {
        match instruction.opcode.addressing_mode().operand_type() {
            mos6502::addressing_modes::OperandType::Memory => {
                Self::emit_8_byte_load(
                    opcode_stream,
                    trampolines,
                    dest_register,
                    DECODED_OP_REGISTER,
                );
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
    fn wrap_and_keep_flags(
        opcode_stream: &mut OpCodeStream,
        saved_flags: &[mos6502::Flags], // the flags to keep
        callable: impl Fn(&mut OpCodeStream),
    ) {
        // TODO(javier-varez): I'm sure this could be optimized, but for now it shall work!

        // Compute saved mask
        if !saved_flags.is_empty() {
            opcode_stream
                .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER, arm_asm::NZCV).generate());
            opcode_stream.push_opcode(
                arm_asm::And::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                    .with_immediate(arm_asm::Immediate::new(Self::flags_mask(saved_flags)))
                    .generate(),
            );
        }

        callable(opcode_stream);

        if !saved_flags.is_empty() {
            opcode_stream
                .push_opcode(arm_asm::Mrs::new(SCRATCH_REGISTER_2, arm_asm::NZCV).generate());
            opcode_stream.push_opcode(
                arm_asm::And::new(SCRATCH_REGISTER_2, SCRATCH_REGISTER_2)
                    .with_immediate(arm_asm::Immediate::new(Self::inverted_flags_mask(
                        saved_flags,
                    )))
                    .generate(),
            );
            opcode_stream.push_opcode(
                arm_asm::Or::new(SCRATCH_REGISTER, SCRATCH_REGISTER)
                    .with_shifted_reg(SCRATCH_REGISTER_2)
                    .generate(),
            );
            opcode_stream
                .push_opcode(arm_asm::Msr::new(arm_asm::NZCV, SCRATCH_REGISTER).generate());
        }
    }

    fn emit_load_instruction(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
    ) {
        let dest_reg = match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => ACCUMULATOR_REGISTER,
            mos6502::instructions::BaseInstruction::Ldx => X_REGISTER,
            mos6502::instructions::BaseInstruction::Ldy => Y_REGISTER,
            _ => {
                unreachable!()
            }
        };

        Self::wrap_and_keep_flags(
            opcode_stream,
            &[mos6502::Flags::C, mos6502::Flags::V],
            |opcode_stream| {
                match instruction.opcode.addressing_mode().operand_type() {
                    mos6502::addressing_modes::OperandType::Memory => {
                        Self::emit_8_byte_load(
                            opcode_stream,
                            trampolines,
                            dest_reg,
                            DECODED_OP_REGISTER,
                        );
                    }
                    mos6502::addressing_modes::OperandType::Value => {
                        opcode_stream.push_opcode(
                            arm_asm::Mov::new(dest_reg, DECODED_OP_REGISTER).generate(),
                        );
                    }
                    mos6502::addressing_modes::OperandType::None => {
                        unreachable!()
                    }
                }

                opcode_stream.push_opcode(arm_asm::SetF8::new(dest_reg).generate());
            },
        )
    }

    fn emit_store_instruction(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
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
        Self::emit_8_byte_store(opcode_stream, trampolines, source_reg, DECODED_OP_REGISTER);
    }

    /// Handles the actual instruction, assuming that the decoded operand is available in DECODED_OP_REGISTER
    fn emit_instruction(
        opcode_stream: &mut OpCodeStream,
        instruction: &mos6502::Instruction,
        trampolines: &Trampolines,
    ) {
        match instruction.opcode.base_instruction() {
            mos6502::instructions::BaseInstruction::Lda => {
                Self::emit_load_instruction(opcode_stream, instruction, &trampolines);
            }
            mos6502::instructions::BaseInstruction::Ldx => {
                Self::emit_load_instruction(opcode_stream, instruction, &trampolines);
            }
            mos6502::instructions::BaseInstruction::Ldy => {
                Self::emit_load_instruction(opcode_stream, instruction, &trampolines);
            }
            mos6502::instructions::BaseInstruction::Sta => {
                Self::emit_store_instruction(opcode_stream, instruction, &trampolines);
            }
            mos6502::instructions::BaseInstruction::Stx => {
                Self::emit_store_instruction(opcode_stream, instruction, &trampolines);
            }
            mos6502::instructions::BaseInstruction::Sty => {
                Self::emit_store_instruction(opcode_stream, instruction, &trampolines);
            }
            mos6502::instructions::BaseInstruction::Adc => {
                Self::emit_deref_if_needed(
                    opcode_stream,
                    instruction,
                    DECODED_OP_REGISTER,
                    trampolines,
                );
                opcode_stream.push_opcode(
                    arm_asm::Sxtb::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER).generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::Sxtb::new(DECODED_OP_REGISTER, DECODED_OP_REGISTER).generate(),
                );
                opcode_stream.push_opcode(
                    arm_asm::Adc::new(
                        ACCUMULATOR_REGISTER,
                        ACCUMULATOR_REGISTER,
                        DECODED_OP_REGISTER,
                    )
                    .update_flags()
                    .with_op_size(arm_asm::OpSize::Size32)
                    .generate(),
                );
                opcode_stream.push_opcode(arm_asm::SetF8::new(ACCUMULATOR_REGISTER).generate());
            }
            mos6502::instructions::BaseInstruction::And => {
                Self::emit_deref_if_needed(
                    opcode_stream,
                    instruction,
                    DECODED_OP_REGISTER,
                    trampolines,
                );
                opcode_stream.push_opcode(
                    arm_asm::And::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Ora => {
                Self::emit_deref_if_needed(
                    opcode_stream,
                    instruction,
                    DECODED_OP_REGISTER,
                    trampolines,
                );
                opcode_stream.push_opcode(
                    arm_asm::Or::new(ACCUMULATOR_REGISTER, ACCUMULATOR_REGISTER)
                        .with_shifted_reg(DECODED_OP_REGISTER)
                        .generate(),
                );
            }
            mos6502::instructions::BaseInstruction::Eor => {
                Self::emit_deref_if_needed(
                    opcode_stream,
                    instruction,
                    DECODED_OP_REGISTER,
                    trampolines,
                );
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
}
