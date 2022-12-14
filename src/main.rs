#![allow(dead_code)]

use apple_one_jit::memory::MemoryInterface;
use apple_one_jit::virtual_machine::VirtualMachine;

struct Memory {}

impl MemoryInterface for Memory {
    extern "C" fn read_8_bits(&self, _addr: apple_one_jit::memory::Address) -> u8 {
        0
    }
    extern "C" fn read_16_bits(&self, _addr: apple_one_jit::memory::Address) -> u16 {
        0
    }
    extern "C" fn write_8_bits(&mut self, _addr: apple_one_jit::memory::Address, _data: u8) {}
}

fn main() {
    let mut memory = Memory {};
    let mut vm = VirtualMachine::new(&mut memory);
    vm.run().unwrap();
}
