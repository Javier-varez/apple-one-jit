pub type TargetAddress = u16;
pub type VirtualAddress = *const ();

pub trait MemoryInterface {
    extern "C" fn read_8_bits(&self, addr: TargetAddress) -> u8;
    extern "C" fn read_16_bits(&self, addr: TargetAddress) -> u16;
    extern "C" fn write_8_bits(&mut self, addr: TargetAddress, data: u8);
}
