use crate::jit::JitPage;
use crate::mos6502::{self, InstrDecoder};

pub enum Error {}

impl From<mos6502::Error> for Error {
    fn from(_: mos6502::Error) -> Self {
        todo!()
    }
}

pub struct Compiler {
    page: JitPage,
    decoder: InstrDecoder,
}

impl Compiler {
    pub fn new() -> Result<Self, region::Error> {
        Ok(Self {
            page: JitPage::allocate(region::page::size())?,
            decoder: InstrDecoder::new(),
        })
    }

    pub fn translate_code(&mut self, buffer: &[u8]) -> Result<(), Error> {
        for byte in buffer.iter() {
            if let Some(instr) = self.decoder.feed(*byte)? {
                // Handle instruction
            }
        }
        Ok(())
    }

    pub unsafe fn run<U, T: Fn(*const ()) -> U>(&mut self, callable: T) -> U {
        self.page.run(callable)
    }
}
