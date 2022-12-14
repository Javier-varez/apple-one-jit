pub type Address = u16;

pub trait MemoryInterface {
    extern "C" fn read_8_bits(&self, addr: Address) -> u8;
    extern "C" fn read_16_bits(&self, addr: Address) -> u16;
    extern "C" fn write_8_bits(&mut self, addr: Address, data: u8);
}
