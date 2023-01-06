pub type TargetAddress = u16;
pub type VirtualAddress = *const ();

pub trait MemoryInterface {
    extern "C" fn read_8_bits(&self, addr: TargetAddress) -> u8;
    extern "C" fn read_16_bits(&self, addr: TargetAddress) -> u16 {
        let lb = self.read_8_bits(addr);
        let hb = self.read_8_bits(addr + 1);
        ((hb as u16) << 8) | (lb as u16)
    }

    extern "C" fn write_8_bits(&mut self, addr: TargetAddress, data: u8);
}
