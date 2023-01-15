use crate::block::ExecutableBlock;
use crate::memory::{TargetAddress, VirtualAddress};
use std::collections::HashMap;

type TranslationMap = HashMap<TargetAddress, VirtualAddress>;

#[derive(Debug)]
pub enum Error {
    /// The address does not belong to an instruction that is part of the code block.
    InvalidAddress,
}

pub struct LocationRange {
    base_address: TargetAddress,
    size: usize,
}

impl LocationRange {
    /// Creates a new location range from the start and end addresses
    pub fn new(start_addr: TargetAddress, end_addr: TargetAddress) -> Self {
        assert!(end_addr > start_addr);
        Self {
            base_address: start_addr,
            size: (end_addr - start_addr) as usize,
        }
    }

    /// Checks if the address is part of this location range
    pub fn contains(&self, address: TargetAddress) -> bool {
        (self.base_address <= address) && ((self.base_address + self.size as u16) > address)
    }
}

pub struct CompiledBlock {
    block: ExecutableBlock,
    location_range: LocationRange,
    translations: TranslationMap,
}

impl CompiledBlock {
    pub fn new(
        block: ExecutableBlock,
        location_range: LocationRange,
        translations: TranslationMap,
    ) -> Self {
        Self {
            block,
            location_range,
            translations,
        }
    }

    pub fn matches_address(&self, addr: TargetAddress) -> bool {
        self.location_range.contains(addr)
    }

    pub fn translate_address(&self, addr: TargetAddress) -> Result<VirtualAddress, Error> {
        self.translations
            .get(&addr)
            .ok_or(Error::InvalidAddress)
            .copied()
    }

    pub fn entrypoint(&self) -> VirtualAddress {
        self.block.entrypoint()
    }
}
