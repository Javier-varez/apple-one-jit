pub type Address = u16;

pub trait MemoryInterface {
    fn read_8_bits(&self, addr: Address) -> u8;
    fn read_16_bits(&self, addr: Address) -> u16;
    fn write_8_bits(&mut self, addr: Address, data: u8);
    fn write_16_bits(&mut self, addr: Address, data: u16);
}
