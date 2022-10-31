use crate::arm_asm;
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
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteXIndexed => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::AbsoluteYIndexed => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::Immediate => {
                let value = match operand {
                    mos6502::addressing_modes::Operand::U8(val) => val,
                    other => {
                        panic!(
                            "Unexpected operand {:?} for AddressingMode::Immediate",
                            other
                        )
                    }
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
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::XIndexedIndirect => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::IndirectYIndexed => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::Relative => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::Zeropage => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::ZeropageXIndexed => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
            mos6502::addressing_modes::AddressingMode::ZeropageYIndexed => {
                // TODO(javier-varez): This cannot be implemented until ldr and str instructions are available in arm_asm
                unimplemented!();
            }
        }
    }

    /// Handles the actual instruction, assuming that the decoded operand is available in DECODED_OP_REGISTER
    fn emit_instruction(_opcode_stream: &mut OpCodeStream, _instruction: &mos6502::Instruction) {
        unimplemented!()
    }

    /// Runs the dynamically-reassembled code, protecting temporarily the inner page as RX.
    /// # Safety
    /// Must guarantee that the compiled code is valid and will naturally complete execution
    pub unsafe fn run<U, T: Fn(*const ()) -> U>(&mut self, callable: T) -> U {
        self.page.run(callable)
    }
}
