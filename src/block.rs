use crate::arm_asm::{OpCode, Udf};
use nix::errno::Errno;
use nix::sys::mman::{mmap, mprotect, MapFlags, ProtFlags};
use nix::unistd::{sysconf, SysconfVar};
use std::ffi::c_void;
use std::num::NonZeroUsize;

#[derive(Debug)]
pub enum Error {
    MmapError(Errno),
}

/// A marker
#[derive(Clone)]
pub struct Marker {
    index: i64,
    size: usize, // size in bytes
}

impl Marker {
    pub fn next(&self) -> Self {
        Self {
            index: self.index + (self.size / std::mem::size_of::<u32>()) as i64,
            size: 0,
        }
    }
}

/// Obtains the system page size in bytes
pub fn page_size() -> usize {
    sysconf(SysconfVar::PAGE_SIZE)
        .unwrap()
        .unwrap()
        .try_into()
        .unwrap()
}

/// Represents a page-aligned dynamic allocation. May be used to store code and data
pub struct Allocation {
    ptr: *mut c_void,
    size: usize,
}

impl Allocation {
    /// Returns the base address of the allocation as a const pointer
    pub fn as_ptr<T>(&self) -> *const T {
        self.ptr as *const _
    }

    /// Returns the base address of the allocation as a mut pointer
    pub fn as_mut_ptr<T>(&self) -> *mut T {
        self.ptr as *mut _
    }

    /// Returns the size of the allocation in bytes
    pub fn size(&self) -> usize {
        self.size
    }

    /// Changes the protection flags of the region of the allocation to read-exec
    fn protect_exec(&mut self) {
        unsafe {
            mprotect(
                self.ptr,
                self.size,
                ProtFlags::PROT_EXEC | ProtFlags::PROT_READ,
            )
            .unwrap()
        };
    }

    /// Changes the protection flags of the region of the allocation to read-write
    fn protect_write(&mut self) {
        unsafe {
            mprotect(
                self.ptr,
                self.size,
                ProtFlags::PROT_WRITE | ProtFlags::PROT_READ,
            )
            .unwrap()
        };
    }
}

/// Represents a stream of opcodes backed by an allocation
/// may be used to populate an allocation with instructions and data.
pub struct OpCodeStream {
    allocation: Allocation,
    index: usize,
}

impl OpCodeStream {
    /// Constructs an opcode stream using the passed allocation
    pub fn new(allocation: Allocation) -> Self {
        Self {
            allocation,
            index: 0,
        }
    }

    /// Pushes a new opcode into the stream
    pub fn push_opcode(&mut self, opcode: OpCode) -> Marker {
        let index = self.index;
        let data = self.get_mut_data();
        data[index] = opcode.value();
        self.index = index + 1;
        Marker {
            index: index as i64,
            size: 4,
        }
    }

    /// Aligns the opcode stream to the given size.
    pub fn align(&mut self, size: usize) {
        let size = size / std::mem::size_of::<u32>();
        let rest = self.index % size;
        if rest != 0 {
            self.index += size - rest;
        }
    }

    /// Pushes a pointer into the stream. It will implicitly align the index to 8 bytes. Returns a
    /// marker with the location of the instruction.
    pub fn push_pointer<T>(&mut self, label: *const T) -> Marker {
        self.align(8);
        let saved_index = self.index;
        let label = label as usize;

        let index = self.index;
        self.get_mut_data()[index] = (label & 0xffffffff) as u32;
        self.index += 1;

        let index = self.index;
        self.get_mut_data()[index] = (label >> 32) as u32;
        self.index += 1;

        Marker {
            index: saved_index as i64,
            size: 8,
        }
    }

    /// Pushes an undefined instruction. Returns a marker with the location of the instruction.
    pub fn push_undefined_instruction(&mut self) -> Marker {
        let index = self.index;
        self.push_opcode(Udf::new().generate());

        Marker {
            index: index as i64,
            size: 4,
        }
    }

    /// Gets a marker with the location of the next instruction that will be pushed
    pub fn get_current_marker(&mut self) -> Marker {
        Marker {
            index: self.index as i64,
            size: 0,
        }
    }

    /// Obtains the relative distance between two markers as an i64 value.
    /// This value represents the number of 4-byte instructions between the markers.
    pub fn relative_distance(&self, marker_a: &Marker, marker_b: &Marker) -> i64 {
        marker_a.index - marker_b.index
    }

    /// Obtains the address of the given marker as a const pointer
    pub fn marker_address(&self, marker: &Marker) -> *const () {
        &self.get_data()[marker.index as usize] as *const _ as *const _
    }

    /// Patches the opcode at the location given by the marker, replacing it.
    pub fn patch_opcode(&mut self, marker: &Marker, opcode: OpCode) {
        self.get_mut_data()[marker.index as usize] = opcode.value();
    }

    /// Writes the binary code to a file at the given file, panicking if the file could not be
    /// written
    pub fn to_file(&self, path: &std::path::Path) {
        let data: Vec<u8> = self.get_data()[..self.index]
            .iter()
            .flat_map(|value| value.to_le_bytes())
            .collect();
        std::fs::write(path, data).unwrap();
    }

    /// Turns the stream into an ExecutableBlock which may be run
    pub fn into_executable_code(mut self) -> ExecutableBlock {
        self.allocation.protect_exec();

        // Invalidate the instruction cache at this virtual address (for this page) because the
        // code in this page has changed
        unsafe { core::arch::asm!("ic ivau, {va}", va = in(reg) self.allocation.as_ptr::<()>()) };
        // Ensure the instruction finishes
        unsafe { core::arch::asm!("dsb ish") };
        // Then flush the instruction stream
        unsafe { core::arch::asm!("isb") };

        ExecutableBlock {
            allocation: self.allocation,
        }
    }

    fn get_data(&self) -> &[u32] {
        let ptr = self.allocation.as_ptr::<u32>();
        let size = self.allocation.size() / std::mem::size_of::<u32>();
        let slice = unsafe { std::slice::from_raw_parts(ptr, size) };
        slice
    }

    fn get_mut_data(&mut self) -> &mut [u32] {
        let ptr = self.allocation.as_mut_ptr::<u32>();
        let size = self.allocation.size() / std::mem::size_of::<u32>();
        let slice = unsafe { std::slice::from_raw_parts_mut(ptr, size) };
        slice
    }
}

/// An executable block backed by an Allocation
pub struct ExecutableBlock {
    allocation: Allocation,
}

impl ExecutableBlock {
    pub fn entrypoint(&self) -> *const () {
        self.allocation.as_ptr()
    }

    pub fn clear_code(mut self) -> Block {
        self.allocation.protect_write();
        Block {
            allocation: self.allocation,
        }
    }
}

pub struct Block {
    allocation: Allocation,
}

impl Block {
    pub fn allocate(size: usize) -> Result<Self, Error> {
        let ptr = unsafe {
            mmap(
                None,
                NonZeroUsize::new(size).expect("Invalid size to allocate a block"),
                ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
                MapFlags::MAP_ANONYMOUS | MapFlags::MAP_PRIVATE,
                -1,
                0,
            )
        }
        .map_err(Error::MmapError)?;
        Ok(Self {
            allocation: Allocation { ptr, size },
        })
    }

    pub fn into_stream(self) -> OpCodeStream {
        OpCodeStream::new(self.allocation)
    }
}

#[cfg(all(test, target_arch = "aarch64"))]
mod test {
    use super::*;
    use crate::arm_asm::{
        Adc, Add, And, Asr, Bfi, Bfxil, Branch, Condition, Immediate, LdrLit, Ldrd, Lsl, Lsr,
        MemoryAccessMode, Mov, MovShift, Movk, Movn, Movz, Mrs, Msr, Nop, OpSize, Or, RegShift,
        Register, Ret, Sbc, SetF8, SignedImmediate, Strd, Sub, Sxtb, Ubfx, Xor, NZCV,
    };

    macro_rules! invoke {
        ($jit:expr, $fn_type:ty $(, $args:expr)*) => {
            (|| {
                let ptr = $jit.entrypoint();
                let func: $fn_type = std::mem::transmute_copy(&ptr);
                func($($args),*)
            })()
        };
    }

    #[test]
    fn test_add_immediate() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Add::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(24))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 10) };
        assert_eq!(val, 34);
    }

    #[test]
    fn test_add_shifted_immediate() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Add::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(0x12))
                .shifted()
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 0x123) };
        assert_eq!(val, 0x12123);
    }

    #[test]
    fn test_sub_immediate() {
        let block = Block::allocate(page_size()).unwrap();

        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Sub::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(24))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 34) };
        assert_eq!(val, 10);
    }

    #[test]
    fn test_sub_shifted_immediate() {
        let block = Block::allocate(page_size()).unwrap();

        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Sub::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(0x1))
                .shifted()
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 0x1234) };
        assert_eq!(val, 0x234);
    }

    #[test]
    fn test_add_register_not_shifted() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Add::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 0x1234, 0x10001) };
        assert_eq!(val, 0x11235);
    }

    #[test]
    fn test_sub_register_not_shifted() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Sub::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 1234, 1230) };
        assert_eq!(val, 4);
    }

    #[test]
    fn test_add_register_shifted_left() {
        let block = Block::allocate(page_size()).unwrap();

        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Add::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .with_shift(RegShift::Lsl(8))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 0x12, 0x23) };
        assert_eq!(val, 0x2312);
    }

    #[test]
    fn test_sub_register_shifted_left() {
        let block = Block::allocate(page_size()).unwrap();

        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Sub::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .with_shift(RegShift::Lsl(8))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 0x2333, 0x23) };
        assert_eq!(val, 0x33);
    }

    #[test]
    fn test_or_immediate() {
        let block = Block::allocate(page_size()).unwrap();

        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Or::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(0x1111_1111_1111_1111u64))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 0xAAAA_0000_FFFF_5555) };
        assert_eq!(val, 0xBBBB_1111_FFFF_5555);
    }

    #[test]
    fn test_xor_immediate() {
        let block = Block::allocate(page_size()).unwrap();

        let mut opcode_stream = block.into_stream();
        opcode_stream.push_opcode(
            Xor::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(0xdddd_dddd_dddd_ddddu64))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());

        let block = opcode_stream.into_executable_code();
        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 0xAAAA_0000_FFFF_5555) };
        assert_eq!(val, 0x7777_dddd_2222_8888);
    }

    #[test]
    fn test_and_immediate() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            And::new(Register::X0, Register::X0)
                .with_immediate(Immediate::new(0xff00_ff00_ff00_ff00u64))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 0xAAAA_0000_FFFF_5555) };
        assert_eq!(val, 0xaa00_0000_ff00_5500);
    }

    #[test]
    fn test_or_register() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Or::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .with_shift(RegShift::Lsl(8))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(u64, u64) -> u64,
                0xAAAA_0000_FFFF_5555,
                0x5555_0000_AAAA_FFFF
            )
        };
        assert_eq!(val, 0xFFAA_00AA_FFFF_FF55);
    }

    #[test]
    fn test_and_register() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            And::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .with_shift(RegShift::Lsl(8))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(u64, u64) -> u64,
                0xAAAA_0000_FFFF_5555,
                0x5555_0000_AAAA_FFFF
            )
        };
        assert_eq!(val, 0x0000_0000_AAFF_5500);
    }

    #[test]
    fn test_xor_register() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Xor::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X1)
                .with_shift(RegShift::Lsl(8))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(u64, u64) -> u64,
                0xAAAA_0000_FFFF_5555,
                0x5555_0000_AAAA_FFFF
            )
        };
        assert_eq!(val, 0xFFAA_00AA_5500_AA55);
    }

    #[test]
    fn test_movz() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Movz::new(Register::X0)
                .with_immediate(Immediate::new(0x1234))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn() -> u64) };
        assert_eq!(val, 0x1234);
    }

    #[test]
    fn test_movk() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Movz::new(Register::X0)
                .with_immediate(Immediate::new(0x1234))
                .generate(),
        );
        opcode_stream.push_opcode(
            Movk::new(Register::X0)
                .with_immediate(Immediate::new(0x5678))
                .with_shift(MovShift::Bits16)
                .generate(),
        );
        opcode_stream.push_opcode(
            Movk::new(Register::X0)
                .with_immediate(Immediate::new(0xFFFF))
                .with_shift(MovShift::Bits32)
                .generate(),
        );
        opcode_stream.push_opcode(
            Movk::new(Register::X0)
                .with_immediate(Immediate::new(0x55AA))
                .with_shift(MovShift::Bits48)
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn() -> u64) };
        assert_eq!(val, 0x55AAFFFF56781234);
    }

    #[test]
    fn test_movn() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Movn::new(Register::X0)
                .with_immediate(Immediate::new(0xAA55))
                .with_shift(MovShift::Bits0)
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn() -> u64) };
        assert_eq!(val, 0xFFFFFFFFFFFF55AA);
    }

    #[test]
    fn test_mov_register() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Mov::new(Register::X0, Register::X1).generate());
        opcode_stream.push_opcode(Ret::new().generate());

        let block = opcode_stream.into_executable_code();
        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 123, 234) };
        assert_eq!(val, 234);
    }

    #[test]
    fn test_branch() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Movz::new(Register::X0)
                .with_immediate(Immediate::new(0xFFFF))
                .with_shift(MovShift::Bits0)
                .generate(),
        );

        let source_marker = opcode_stream.push_undefined_instruction();
        opcode_stream.push_opcode(
            Movz::new(Register::X0)
                .with_immediate(Immediate::new(0xAA55))
                .with_shift(MovShift::Bits0)
                .generate(),
        );
        let target_marker = opcode_stream.push_opcode(Ret::new().generate());

        opcode_stream.patch_opcode(
            &source_marker,
            Branch::new()
                .with_immediate(SignedImmediate::new(
                    opcode_stream.relative_distance(&target_marker, &source_marker),
                ))
                .generate(),
        );
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn() -> u64) };
        assert_eq!(val, 0xFFFF);
    }

    #[test]
    fn test_conditional_branch() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Add::new(Register::X2, Register::X0)
                .with_immediate(Immediate::new(0))
                .generate(),
        );
        opcode_stream.push_opcode(
            Movz::new(Register::X0)
                .with_immediate(Immediate::new(0))
                .generate(),
        );

        // Check condition
        opcode_stream.push_opcode(
            Add::new(Register::X1, Register::X1)
                .with_immediate(Immediate::new(0))
                .update_flags()
                .generate(),
        );
        let branch_forward_marker = opcode_stream.push_undefined_instruction();

        opcode_stream.push_opcode(
            Add::new(Register::X0, Register::X0)
                .with_shifted_reg(Register::X2)
                .generate(),
        );
        opcode_stream.push_opcode(
            Sub::new(Register::X1, Register::X1)
                .with_immediate(Immediate::new(1))
                .update_flags()
                .generate(),
        );
        let branch_back_marker = opcode_stream.push_undefined_instruction();
        let offset = opcode_stream.relative_distance(&branch_forward_marker, &branch_back_marker);
        opcode_stream.patch_opcode(
            &branch_back_marker,
            Branch::new()
                .with_immediate(SignedImmediate::new(offset))
                .generate(),
        );

        // Patching opcode
        let target_jump = opcode_stream.push_opcode(Ret::new().generate());
        let offset = opcode_stream.relative_distance(&target_jump, &branch_forward_marker);
        opcode_stream.patch_opcode(
            &branch_forward_marker,
            Branch::new()
                .with_immediate(SignedImmediate::new(offset))
                .iff(Condition::Eq)
                .generate(),
        );
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 4, 4) };
        assert_eq!(val, 16);

        let val = unsafe { invoke!(block, extern "C" fn(u64, u64) -> u64, 128, 128) };
        assert_eq!(val, 16384);
    }

    #[test]
    fn test_str_instruction() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Store the value in X0 in [X1]
        opcode_stream.push_opcode(
            Strd::new(Register::X0, Register::X1)
                .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(0)))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let input = 0x1234_5678_9012_3456u64;
        let mut output = 0u64;

        unsafe {
            invoke!(
                block,
                extern "C" fn(u64, *mut u64),
                input,
                &mut output as *mut u64
            )
        };

        assert_eq!(output, input);
    }

    #[test]
    fn test_ldr_instruction() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Store the address in X0 in [X0]
        opcode_stream.push_opcode(
            Ldrd::new(Register::X0, Register::X0)
                .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(0)))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let input = 0x1234_5678_9012_3456u64;
        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(*const u64) -> u64,
                &input as *const u64
            )
        };
        assert_eq!(val, 0x1234_5678_9012_3456u64);
    }

    #[test]
    fn test_str_with_imm_offset_instruction() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Store the value in X0 in [X1]
        opcode_stream.push_opcode(
            Strd::new(Register::X0, Register::X1)
                .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(1)))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let input = 0x1234_5678_9012_3456u64;
        let mut output = [0u64; 2];

        unsafe {
            invoke!(
                block,
                extern "C" fn(u64, *mut u64),
                input,
                &mut output[0] as *mut u64
            )
        };

        assert_eq!(output[0], 0);
        assert_eq!(output[1], input);
    }

    #[test]
    fn test_ldr_with_imm_offset_instruction() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Store the value in X0 in [X1]
        opcode_stream.push_opcode(
            Ldrd::new(Register::X0, Register::X0)
                .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(1)))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let input = [0u64, 0x1234_5678_9012_3456u64];

        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(*const u64) -> u64,
                &input[0] as *const u64
            )
        };

        assert_eq!(val, 0x1234_5678_9012_3456u64);
    }

    #[test]
    fn test_ldr_with_register_offset_instr() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Store the value in X0 in [X1]
        opcode_stream.push_opcode(
            Ldrd::new(Register::X0, Register::X0)
                .with_mode(MemoryAccessMode::ShiftedRegister(Register::X1))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let input = [123u64, 0x1234_5678_9012_3456u64];

        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(*const u64, u64) -> u64,
                &input[0] as *const u64,
                0
            )
        };

        assert_eq!(val, 123);

        let val = unsafe {
            invoke!(
                block,
                extern "C" fn(*const u64, u64) -> u64,
                &input[0] as *const u64,
                8
            )
        };

        assert_eq!(val, 0x1234_5678_9012_3456u64);
    }

    #[test]
    fn test_str_with_register_offset_instr() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Store the value in X0 in [X1]
        opcode_stream.push_opcode(
            Strd::new(Register::X2, Register::X0)
                .with_mode(MemoryAccessMode::ShiftedRegister(Register::X1))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let mut output = [0u64, 0u64];

        unsafe {
            invoke!(
                block,
                extern "C" fn(*mut u64, usize, u64),
                &mut output[0] as *mut u64,
                0,
                0x1234_5678_9012_3456u64
            )
        };

        assert_eq!(output[0], 0x1234_5678_9012_3456);
        assert_eq!(output[1], 0);

        unsafe {
            invoke!(
                block,
                extern "C" fn(*mut u64, usize, u64),
                &mut output[0] as *mut u64,
                8,
                0x1234_5678_9012_3456u64
            )
        };
        assert_eq!(output[0], 0x1234_5678_9012_3456);
        assert_eq!(output[1], 0x1234_5678_9012_3456);
    }

    #[test]
    fn test_sign_extend() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        // Sign extend byte on x0
        opcode_stream.push_opcode(Sxtb::new(Register::X0, Register::X0).generate());
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let value = unsafe { invoke!(block, extern "C" fn(u8) -> u32, 0x84) };
        assert_eq!(value, 0xffffff84);
        let value = unsafe { invoke!(block, extern "C" fn(u8) -> u32, 0x73) };
        assert_eq!(value, 0x73);
    }

    #[test]
    fn test_set_get_flags() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Msr::new(NZCV, Register::X0).generate());
        opcode_stream.push_opcode(Mrs::new(Register::X0, NZCV).generate());
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let value = unsafe { invoke!(block, extern "C" fn(u64) -> u64, 0xFFFFFFFFFFFFFFF) };
        assert_eq!(value, 0xF0000000);

        let mut opcode_stream = block.clear_code().into_stream();
        opcode_stream.push_opcode(
            Movz::new(Register::X1)
                .with_immediate(Immediate::new(0x00))
                .generate(),
        );
        opcode_stream.push_opcode(Msr::new(NZCV, Register::X1).generate());
        opcode_stream.push_opcode(SetF8::new(Register::X0).generate());
        opcode_stream.push_opcode(Mrs::new(Register::X0, NZCV).generate());
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        const N: u64 = 0x80000000;
        const Z: u64 = 0x40000000;
        const V: u64 = 0x10000000;
        assert_eq!(unsafe { invoke!(block, extern "C" fn(u32) -> u64, 0) }, Z);
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u32) -> u64, 0x80) },
            V | N
        );
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u32) -> u64, 0xffffff80) },
            N
        );
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u32) -> u64, 0xffffff7f) },
            V
        );
    }

    #[test]
    fn test_add_with_carry() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
        opcode_stream.push_opcode(Adc::new(Register::X0, Register::X0, Register::X1).generate());
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        const C: u64 = 0x20000000;
        const NO_FLAGS: u64 = 0x00000000;
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, C) },
            17
        );

        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, NO_FLAGS) },
            16
        );

        assert_eq!(
            unsafe {
                invoke!(
                    block,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0x7FFF_FFFF_FFFF_FFFF,
                    4,
                    C
                )
            },
            0x8000_0000_0000_0004
        );

        assert_eq!(
            unsafe {
                invoke!(
                    block,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0xFFFF_FFFF_FFFF_FFFF,
                    4,
                    C
                )
            },
            0x0000_0000_0000_0004
        );
    }

    #[test]
    fn test_add_with_carry_32_bit() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
        opcode_stream.push_opcode(
            Adc::new(Register::X0, Register::X0, Register::X1)
                .with_op_size(OpSize::Size32)
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        const C: u64 = 0x20000000;
        const NO_FLAGS: u64 = 0x00000000;
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, C) },
            17
        );

        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, NO_FLAGS) },
            16
        );

        assert_eq!(
            unsafe {
                invoke!(
                    block,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0x7FFF_FFFF,
                    4,
                    C
                )
            },
            0x8000_0004
        );

        assert_eq!(
            unsafe {
                invoke!(
                    block,
                    extern "C" fn(u64, u64, u64) -> u64,
                    0xFFFF_FFFF,
                    4,
                    C
                )
            },
            0x0000_0004
        );
    }

    #[test]
    fn test_sub_with_borrow() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
        opcode_stream.push_opcode(Sbc::new(Register::X0, Register::X0, Register::X1).generate());
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        const BORROW: u64 = 0x00000000;
        const NO_BORROW: u64 = 0x20000000;
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, BORROW) },
            7
        );

        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, NO_BORROW) },
            8
        );

        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 12, BORROW) },
            0xFFFF_FFFF_FFFF_FFFF
        );

        assert_eq!(
            unsafe {
                invoke!(
                    block,
                    extern "C" fn(u64, u64, u64) -> u64,
                    11,
                    12,
                    NO_BORROW
                )
            },
            0xFFFF_FFFF_FFFF_FFFF
        );
    }

    #[test]
    fn test_sub_with_borrow_32_bit() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Msr::new(NZCV, Register::X2).generate());
        opcode_stream.push_opcode(
            Sbc::new(Register::X0, Register::X0, Register::X1)
                .with_op_size(OpSize::Size32)
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        const BORROW: u64 = 0x00000000;
        const NO_BORROW: u64 = 0x20000000;
        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, BORROW) },
            7
        );

        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 4, NO_BORROW) },
            8
        );

        assert_eq!(
            unsafe { invoke!(block, extern "C" fn(u64, u64, u64) -> u64, 12, 12, BORROW) },
            0xFFFF_FFFF
        );

        assert_eq!(
            unsafe {
                invoke!(
                    block,
                    extern "C" fn(u64, u64, u64) -> u64,
                    11,
                    12,
                    NO_BORROW
                )
            },
            0xFFFF_FFFF
        );
    }

    #[test]
    fn test_load_literal() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        let value: u32 = 432;
        let value_ptr = &value as *const u32;

        let ldr_lit_marker = opcode_stream.push_undefined_instruction();
        opcode_stream.push_opcode(Ret::new().generate());

        let ptr_marker = opcode_stream.push_pointer(value_ptr);
        let ptr_distance = opcode_stream.relative_distance(&ptr_marker, &ldr_lit_marker);
        opcode_stream.patch_opcode(
            &ldr_lit_marker,
            LdrLit::new(Register::X0, SignedImmediate::new(ptr_distance)).generate(),
        );
        let block = opcode_stream.into_executable_code();

        let val = unsafe { invoke!(block, extern "C" fn() -> *const u32) };
        assert_eq!(val, value_ptr);
    }

    #[test]
    fn test_branch_register() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(
            Sub::new(Register::SpZr, Register::SpZr)
                .with_immediate(Immediate::new(16))
                .generate(),
        );
        // Save lr in the stack
        opcode_stream.push_opcode(
            Strd::new(Register::X30, Register::SpZr)
                .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(0)))
                .generate(),
        );

        // Call another procedure!
        opcode_stream.push_opcode(Branch::new().link().with_register(Register::X0).generate());
        opcode_stream.push_opcode(
            Movz::new(Register::X0)
                .with_immediate(Immediate::new(0x1234))
                .generate(),
        );

        // Save lr in the stack
        opcode_stream.push_opcode(
            Ldrd::new(Register::X30, Register::SpZr)
                .with_mode(MemoryAccessMode::UnsignedOffsetImmediate(Immediate::new(0)))
                .generate(),
        );
        opcode_stream.push_opcode(
            Add::new(Register::SpZr, Register::SpZr)
                .with_immediate(Immediate::new(16))
                .generate(),
        );
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        let func: fn() = || println!("Call happens!");
        let val = unsafe { invoke!(block, extern "C" fn(fn()) -> u16, func) };
        assert_eq!(val, 0x1234);
    }

    #[test]
    fn test_lsl() {
        let mut block = Some(Block::allocate(page_size()).unwrap());

        for shift in 0..63 {
            let mut opcode_stream = block.take().unwrap().into_stream();
            opcode_stream.push_opcode(
                Lsl::new(Register::X0, Register::X0, Immediate::new(shift)).generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
            let code_block = opcode_stream.into_executable_code();

            let val = unsafe { invoke!(code_block, extern "C" fn(u64) -> u64, 0xDEADC0DEDEADC0DE) };
            assert_eq!(val, 0xDEADC0DEDEADC0DE << shift);
            block.replace(code_block.clear_code());
        }
    }

    #[test]
    fn test_lsr() {
        let mut block = Some(Block::allocate(page_size()).unwrap());

        for shift in 0..63 {
            let mut opcode_stream = block.take().unwrap().into_stream();
            opcode_stream.push_opcode(
                Lsr::new(Register::X0, Register::X0, Immediate::new(shift)).generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
            let code_block = opcode_stream.into_executable_code();

            let val = unsafe { invoke!(code_block, extern "C" fn(u64) -> u64, 0xDEADC0DEDEADC0DE) };
            assert_eq!(val, 0xDEADC0DEDEADC0DE >> shift);
            block.replace(code_block.clear_code());
        }
    }

    #[test]
    fn test_asr() {
        let mut block = Some(Block::allocate(page_size()).unwrap());

        for shift in 0..63 {
            let mut opcode_stream = block.take().unwrap().into_stream();
            opcode_stream.push_opcode(
                Asr::new(Register::X0, Register::X0, Immediate::new(shift)).generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
            let code_block = opcode_stream.into_executable_code();

            let val = unsafe { invoke!(code_block, extern "C" fn(u64) -> u64, 0xDEADC0DEDEADC0DE) };

            let expected_val = if shift == 0 {
                0xDEADC0DEDEADC0DE_u64
            } else {
                (0xDEADC0DEDEADC0DE_u64 >> shift) | (0xFFFFFFFFFFFFFFFF_u64 << (64_u64 - shift))
            };
            assert_eq!(val, expected_val);
            block.replace(code_block.clear_code());
        }
    }

    #[test]
    fn test_nop() {
        let block = Block::allocate(page_size()).unwrap();
        let mut opcode_stream = block.into_stream();

        opcode_stream.push_opcode(Nop::new().generate());
        opcode_stream.push_opcode(Ret::new().generate());
        let block = opcode_stream.into_executable_code();

        unsafe { invoke!(block, extern "C" fn()) };
    }

    #[test]
    fn test_ubfx() {
        let mut block = Some(Block::allocate(page_size()).unwrap());

        for lsb in 0..58 {
            let mut opcode_stream = block.take().unwrap().into_stream();
            opcode_stream.push_opcode(
                Ubfx::new(
                    Register::X0,
                    Register::X0,
                    Immediate::new(lsb),
                    Immediate::new(6),
                )
                .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
            let code_block = opcode_stream.into_executable_code();

            let val = unsafe { invoke!(code_block, extern "C" fn(u64) -> u64, 0xDEADC0DEDEADC0DE) };

            let expected_val = (0xDEADC0DEDEADC0DE_u64 >> lsb) & 0x3f;
            assert_eq!(val, expected_val);
            block.replace(code_block.clear_code());
        }
    }

    #[test]
    fn test_bfi() {
        let mut block = Some(Block::allocate(page_size()).unwrap());

        for lsb in 0..58 {
            let mut opcode_stream = block.take().unwrap().into_stream();
            opcode_stream.push_opcode(
                Bfi::new(
                    Register::X0,
                    Register::X1,
                    Immediate::new(lsb),
                    Immediate::new(6),
                )
                .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
            let code_block = opcode_stream.into_executable_code();

            let val = unsafe {
                invoke!(
                    code_block,
                    extern "C" fn(u64, u64) -> u64,
                    0xFFFF_FFFF_FFFF_FFFF,
                    0xDEADC0DEDEADC0DE
                )
            };

            let expected_val = 0xFFFF_FFFF_FFFF_FFFF ^ ((0x3f ^ 0x1E) << lsb);
            assert_eq!(val, expected_val);
            block.replace(code_block.clear_code());
        }
    }

    #[test]
    fn test_bfxil() {
        let mut block = Some(Block::allocate(page_size()).unwrap());

        for lsb in 0..58 {
            let mut opcode_stream = block.take().unwrap().into_stream();
            opcode_stream.push_opcode(
                Bfxil::new(
                    Register::X0,
                    Register::X1,
                    Immediate::new(lsb),
                    Immediate::new(6),
                )
                .generate(),
            );
            opcode_stream.push_opcode(Ret::new().generate());
            let code_block = opcode_stream.into_executable_code();

            let val = unsafe {
                invoke!(
                    code_block,
                    extern "C" fn(u64, u64) -> u64,
                    0xFFFF_FFFF_FFFF_FFFF,
                    0xDEADC0DEDEADC0DE
                )
            };

            let expected_val = 0xFFFF_FFFF_FFFF_FFC0 | ((0xDEADC0DEDEADC0DE >> lsb) & 0x3f);
            assert_eq!(val, expected_val);
            block.replace(code_block.clear_code());
        }
    }
}
