use crate::{
    block::{Block, LocationRange},
    dynamic_compiler::{self, Compiler},
    memory::{Address, MemoryInterface},
};

core::arch::global_asm!(include_str!("virtual_machine.S"));

extern "C" {
    #[link_name = "\x01jumpToEmulatedCode"]
    fn jumpToEmulatedCode(
        ptr: *const (),
        state: *mut VmState,
        memory: *mut &mut dyn MemoryInterface,
    );
}

#[repr(C)]
#[derive(Default, PartialEq, Debug)]
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
    BlockError(region::Error),
}

impl From<region::Error> for Error {
    fn from(error: region::Error) -> Self {
        Self::BlockError(error)
    }
}
impl From<crate::dynamic_compiler::Error> for Error {
    fn from(error: crate::dynamic_compiler::Error) -> Self {
        Self::TranslationError(error)
    }
}

pub struct VirtualMachine<'a> {
    compiler: Compiler,
    memory_interface: &'a mut dyn MemoryInterface,
    blocks: Vec<(LocationRange, Block)>,
    state: VmState,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(interface: &'a mut dyn MemoryInterface) -> Self {
        Self {
            compiler: Compiler::new(),
            memory_interface: interface,
            blocks: vec![],
            state: VmState::default(),
        }
    }

    /// Runs the dynamically-reassembled code, protecting temporarily the inner page as RX.
    pub fn run(&mut self) -> Result<(), Error> {
        let block_idx = self.ensure_block_for_address(self.state.pc as Address)?;
        let (_, block) = &mut self.blocks[block_idx];
        let memory_iface_ptr: *mut &mut dyn MemoryInterface = &mut self.memory_interface;
        unsafe {
            block.run(|ptr| {
                jumpToEmulatedCode(ptr, &mut self.state as *mut VmState, memory_iface_ptr);
            })
        };
        Ok(())
    }

    fn ensure_block_for_address(&mut self, address: Address) -> Result<usize, Error> {
        if let Some(idx) = self
            .blocks
            .iter()
            .enumerate()
            .find(|(_idx, (location, _))| location.contains(address))
            .map(|(idx, _)| idx)
        {
            return Ok(idx);
        }

        let mut block = Block::allocate(region::page::size())?;
        let location = self
            .compiler
            .translate_code(&mut block, address, self.memory_interface)?;
        self.blocks.push((location, block));
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
        fn read_8_bits(&self, addr: Address) -> u8 {
            self.memory[addr as usize]
        }
        fn read_16_bits(&self, addr: Address) -> u16 {
            (self.memory[addr as usize] as u16) | ((self.memory[addr as usize] as u16) << 8)
        }
        fn write_8_bits(&mut self, addr: Address, data: u8) {
            self.memory[addr as usize] = data;
        }
        fn write_16_bits(&mut self, addr: Address, data: u16) {
            self.memory[addr as usize] = (data & 0xff) as u8;
            self.memory[(addr + 1) as usize] = (data >> 8) as u8;
        }
    }

    #[test]
    fn add_test() {
        let mut memory = Memory::from(&[0xA9, 0x0A, 0x69, 0x14, 0x60]);
        let mut vm = VirtualMachine::new(&mut memory);
        vm.run().unwrap();
        assert_eq!(vm.state.a, 30);
    }
}
