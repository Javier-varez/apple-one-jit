use crate::compiled_block::CompiledBlock;
use crate::{
    block::{self, page_size, Block},
    compiled_block,
    dynamic_compiler::{self, Compiler},
    memory::{MemoryInterface, TargetAddress},
};

core::arch::global_asm!(include_str!("virtual_machine.S"));

extern "C" {
    #[link_name = "\x01jump_to_emulator"]
    fn jump_to_emulator(ptr: *const (), state: *mut VmState, memory: *mut ()) -> ExitReason;
}

#[repr(C)]
#[derive(Default, PartialEq, Eq, Debug)]
pub struct VmState {
    pub a: u64,
    pub x: u64,
    pub y: u64,
    pub sp: u64,
    pub pc: u64,
    pub flags: u64,
}

#[derive(Debug)]
pub enum Error {
    TranslationError(dynamic_compiler::Error),
    BlockError(block::Error),
    CompiledBlockError(compiled_block::Error),
}

impl From<block::Error> for Error {
    fn from(error: block::Error) -> Self {
        Self::BlockError(error)
    }
}
impl From<crate::dynamic_compiler::Error> for Error {
    fn from(error: crate::dynamic_compiler::Error) -> Self {
        Self::TranslationError(error)
    }
}

impl From<crate::compiled_block::Error> for Error {
    fn from(error: crate::compiled_block::Error) -> Self {
        Self::CompiledBlockError(error)
    }
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum ExitReason {
    BranchInstruction = 0,
    BreakInstruction = 1,
    ReturnInstruction = 2,
    TestEnd = 3,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SystemVector {
    Nmi = 0xFFFA,
    Reset = 0xFFFC,
    Irq = 0xFFFE,
}

pub struct VirtualMachine<'a, T: MemoryInterface> {
    memory_interface: &'a mut T,
    blocks: Vec<CompiledBlock>,
    state: VmState,
}

impl<'a, T: MemoryInterface> VirtualMachine<'a, T> {
    pub fn new(interface: &'a mut T) -> Self {
        Self {
            memory_interface: interface,
            blocks: vec![],
            state: VmState::default(),
        }
    }

    fn read_system_vector(&self, vector: SystemVector) -> TargetAddress {
        let lb = self.memory_interface.read_8_bits(vector as u16);
        let hb = self.memory_interface.read_8_bits((vector as u16) + 1);
        ((hb as TargetAddress) << 8) | (lb as TargetAddress)
    }

    pub fn reset(&mut self) {
        self.state.pc = self.read_system_vector(SystemVector::Reset) as u64;
    }

    /// Runs the dynamically-reassembled code, protecting temporarily the inner page as RX.
    pub fn run(&mut self) -> Result<ExitReason, Error> {
        let block_idx = self.ensure_block_for_address(self.state.pc as TargetAddress)?;
        let block = &mut self.blocks[block_idx];
        let memory_iface_ptr: *mut () = self.memory_interface as *mut _ as *mut _;
        let address = block.translate_address(self.state.pc as TargetAddress)?;
        Ok(unsafe { jump_to_emulator(address, &mut self.state as *mut VmState, memory_iface_ptr) })
    }

    fn ensure_block_for_address(&mut self, address: TargetAddress) -> Result<usize, Error> {
        if let Some(idx) = self
            .blocks
            .iter()
            .enumerate()
            .find(|(_idx, block)| block.matches_address(address))
            .map(|(idx, _)| idx)
        {
            return Ok(idx);
        }

        let block = Block::allocate(page_size())?;
        let compiler = Compiler::new(block, self.memory_interface);
        let block = compiler.translate_code(address)?;
        self.blocks.push(block);
        Ok(self.blocks.len() - 1)
    }

    pub fn get_state(&mut self) -> &VmState {
        &self.state
    }

    pub fn get_mut_state(&mut self) -> &mut VmState {
        &mut self.state
    }
}

#[cfg(test)]
mod test {
    use super::*;

    struct Memory {
        memory: Vec<u8>,
    }

    impl Memory {
        pub fn from(data: &[u8]) -> Self {
            let mut memory = Vec::from(data);
            memory.resize(1 << 16, 0);
            Self { memory }
        }
    }

    impl MemoryInterface for Memory {
        extern "C" fn read_8_bits(&self, addr: TargetAddress) -> u8 {
            self.memory[addr as usize]
        }
        extern "C" fn write_8_bits(&mut self, addr: TargetAddress, data: u8) {
            self.memory[addr as usize] = data;
        }
    }

    #[test]
    fn add_test() {
        let mut memory = Memory::from(&[0xA9, 0x0A, 0x69, 0x14, 0x02]);
        let mut vm = VirtualMachine::new(&mut memory);
        vm.run().unwrap();
        assert_eq!(vm.state.a, 30);
    }
}
