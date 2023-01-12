#![allow(dead_code)]

use std::fs::File;

use apple_one_jit::memory::{MemoryInterface, TargetAddress};
use apple_one_jit::pia::Pia;
use apple_one_jit::virtual_machine::VirtualMachine;
use simplelog::WriteLogger;

struct Memory {
    ram: [u8; Self::RAM_SIZE as usize],
    pia: std::cell::RefCell<Pia>,
}

impl Memory {
    const WOZ_MONITOR: &[u8; 256] = include_bytes!("../woz_monitor/build/woz_monitor.bin");

    const WOZ_MONITOR_OFFSET: TargetAddress = 0xFF00;
    const PIA_OFFSET: TargetAddress = 0xD010;
    const RAM_SIZE: TargetAddress = 4096;

    pub fn new() -> Self {
        Self {
            ram: [0; Self::RAM_SIZE as usize],
            pia: std::cell::RefCell::new(Pia::new()),
        }
    }

    fn log_write_access(&self, addr: apple_one_jit::memory::TargetAddress, data: u8) {
        let txt = match addr {
            0x00..=0x10 => format!("ram ({addr}): "),
            0x24 => "XAML (last opened location low): ".to_string(),
            0x25 => "XAMH (last opened location high): ".to_string(),
            0x26 => "STL (Store address low): ".to_string(),
            0x27 => "STH (Store address high): ".to_string(),
            0x28 => "L (Hex parsing low): ".to_string(),
            0x29 => "H (Hex parsing high): ".to_string(),
            0x2a => "YSAV (Used to see if hex value is given): ".to_string(),
            0x2b => "MODE ($00=XAM, $7F=STOR, $AE=BLOCK XAM): ".to_string(),
            _ => {
                return;
            }
        };
        log::debug!(target: "MemoryAccess", "{} {:x}", txt, data);
    }
}

impl MemoryInterface for Memory {
    extern "C" fn read_8_bits(&self, addr: apple_one_jit::memory::TargetAddress) -> u8 {
        if addr >= Self::WOZ_MONITOR_OFFSET {
            Self::WOZ_MONITOR[(addr - Self::WOZ_MONITOR_OFFSET) as usize]
        } else if addr < Self::RAM_SIZE {
            self.ram[addr as usize]
        } else if (addr >= Self::PIA_OFFSET) && (addr < (Self::PIA_OFFSET + Pia::ADDR_SPACE)) {
            self.pia.borrow_mut().handle_read(addr - Self::PIA_OFFSET)
        } else {
            0
        }
    }

    extern "C" fn write_8_bits(&mut self, addr: apple_one_jit::memory::TargetAddress, data: u8) {
        if addr < Self::RAM_SIZE {
            self.log_write_access(addr, data);
            self.ram[addr as usize] = data;
        } else if (addr >= Self::PIA_OFFSET) && (addr < (Self::PIA_OFFSET + Pia::ADDR_SPACE)) {
            self.pia
                .borrow_mut()
                .handle_write(addr - Self::PIA_OFFSET, data);
        }
    }
}

fn main() {
    WriteLogger::init(
        log::LevelFilter::Debug,
        simplelog::Config::default(),
        File::create("logfile.txt").unwrap(),
    )
    .unwrap();

    let mut memory = Memory::new();

    let mut vm = VirtualMachine::new(&mut memory);
    vm.reset();
    loop {
        let reason = vm.run().unwrap();
        log::info!(
            "VM exited with reason: {reason:?}, state: {state:?}",
            state = vm.get_state()
        );
    }
}
