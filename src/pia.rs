use crate::display::Display;
use crate::keyboard::Keyboard;
use crate::memory::TargetAddress;

struct DisplayPort {
    display: Display,
    peripheral_reg: u8,
    data_dir_reg: u8,
    control_reg: u8,
}

impl DisplayPort {
    const DDR_ACCESS_BIT: usize = 2;
    pub fn new() -> Self {
        Self {
            display: Display::new(),
            peripheral_reg: 0,
            data_dir_reg: 0,
            control_reg: 0,
        }
    }

    pub fn handle_write(&mut self, addr: TargetAddress, mut byte: u8) {
        let reg = match (addr, self.ddr_access()) {
            (0, false) => &mut self.data_dir_reg,
            (0, true) => {
                byte = byte & 0x7f;
                self.display.push_char(byte as char);
                &mut self.peripheral_reg
            }
            (1, _) => &mut self.control_reg,
            _ => unreachable!(),
        };
        *reg = byte;
    }

    pub fn ddr_access(&self) -> bool {
        ((self.control_reg >> Self::DDR_ACCESS_BIT) & 1) != 0
    }

    pub fn handle_read(&mut self, addr: TargetAddress) -> u8 {
        match (addr, self.ddr_access()) {
            (0, false) => self.data_dir_reg,
            (0, true) => self.peripheral_reg,
            (1, _) => self.control_reg,
            _ => unreachable!(),
        }
    }
}

struct KeyboardPort {
    keyboard: Keyboard,
    next_char: Option<char>,
    peripheral_reg: u8,
    data_dir_reg: u8,
    control_reg: u8,
}

impl KeyboardPort {
    const DDR_ACCESS_BIT: usize = 2;
    pub fn new() -> Self {
        Self {
            keyboard: Keyboard::new(),
            next_char: None,
            peripheral_reg: 0,
            data_dir_reg: 0,
            control_reg: 0,
        }
    }

    pub fn handle_write(&mut self, addr: TargetAddress, byte: u8) {
        let reg = match (addr, self.ddr_access()) {
            (0, false) => &mut self.data_dir_reg,
            (0, true) => &mut self.peripheral_reg,
            (1, _) => &mut self.control_reg,
            _ => unreachable!(),
        };
        *reg = byte;
    }

    pub fn ddr_access(&self) -> bool {
        ((self.control_reg >> Self::DDR_ACCESS_BIT) & 1) != 0
    }

    pub fn handle_read_control_reg(&mut self) -> u8 {
        let has_data = if let Some(_) = self.next_char {
            true
        } else {
            self.next_char = self.keyboard.get_character();
            self.next_char.is_some()
        };

        const DATA_BIT: u8 = 1 << 7;
        let data_bit = if has_data { DATA_BIT } else { 0 };

        (self.control_reg & !DATA_BIT) | data_bit
    }

    pub fn handle_read_peripheral_reg(&mut self) -> u8 {
        if let Some(c) = self.next_char {
            self.peripheral_reg = (c as u8) | 0x80;
        }
        self.next_char = None;
        self.peripheral_reg
    }

    pub fn handle_read(&mut self, addr: TargetAddress) -> u8 {
        match (addr, self.ddr_access()) {
            (0, false) => self.data_dir_reg,
            (0, true) => self.handle_read_peripheral_reg(),
            (1, _) => self.handle_read_control_reg(),
            _ => unreachable!(),
        }
    }
}

pub struct Pia {
    port_a: KeyboardPort,
    port_b: DisplayPort,
}

impl Pia {
    pub const ADDR_SPACE: u16 = 4;
    const ADDR_BIT: usize = 0;
    const PORT_BIT: usize = 1;

    pub fn new() -> Self {
        Self {
            port_a: KeyboardPort::new(),
            port_b: DisplayPort::new(),
        }
    }

    pub fn handle_write(&mut self, addr: TargetAddress, byte: u8) {
        match addr >> Self::PORT_BIT {
            0 => self.port_a.handle_write(addr & (1 << Self::ADDR_BIT), byte),
            1 => self.port_b.handle_write(addr & (1 << Self::ADDR_BIT), byte),
            _ => unreachable!(),
        }
    }

    pub fn handle_read(&mut self, addr: TargetAddress) -> u8 {
        match addr >> Self::PORT_BIT {
            0 => self.port_a.handle_read(addr & (1 << Self::ADDR_BIT)),
            1 => self.port_b.handle_read(addr & (1 << Self::ADDR_BIT)),
            _ => unreachable!(),
        }
    }
}
