# Apple I, just in time

`apple-one-jit` is a dynamic recompiler that emulates the mos6502, coupled with the peripherals
and memory of the apple I computer.

I have been somewhat instrigued by just-in-time machine code translation for a while and decided to build a simple
JIT translator in order to emulate a 6502 microprocessor on a mordern day computer.

The 6502 and particularly the Apple I seemed like a good choice of a simple machine that would allowed me
to focus on the recompilation process.

## Table of Contents

1. [About the Project](#about-the-project)
1. [Project Status](#project-status)
1. [Getting Started](#getting-started)
    1. [Dependencies](#dependencies)
    1. [Building](#building)
    1. [Running Tests](#running-tests)

### About The Project

At this moment the code generation backend is only available for the armv8-a architecture (in
aarch64 mode), so it should work with Apple Silicon macs, as well as other armv8-a devices like the
Raspberry Pi 3 or 4.

Adding a second backend for x86 or x64 should not be particularly complicated, but it is out of the
scope of this project for me. However, if you're interested, feel free to contribute it.

Since this project intentionally targets a single architecture backend it does not use any
intermediate representation between the 6502 code and the aarch64 code.

The dynamic compiler translates 6502 instructions into a stream of aarch64 instructions that emulate
the 6502 instruction. Given that the 6502 shares addressing modes across most cpu instructions, the
decoding of the operand is translated separately from the actual instruction in order to simplify
the design and share code (albeit this could be optimized for particular instructions to make the
generated code faster).

Registers of the 6502, as well as the decoded operand for the given addressing mode are statically
allocated into registers of the armv8 architecture. Processor flags of the 6502 cpu are a special
case, which are stored as the NZCV flags of the arm processor. This simplifies setting the flags
after instructions like add or subtract, as well as using the flags during conditional branches
without overhead. One downside of this approach is the fact that the 6502 instructions often affect
a different set of flags than the arm instructions. This is handled saving the unaffected flags
before the instruction is executed and later resotring them.

On the top-level a `VirtualMachine` allows the user to bring their own memory system. For the
emulation of the Apple I this is paired with the PIA peripheral located at `0xD010` that interacts
with the keyboard and the display, the Woz Monitor ROM located at `0xFF00` and a 4KiB RAM located at
address `0x0000`.

Memory is read and written by emitting calls to the `read_8_bytes()` and `write_8_bytes()` functions
of the `MemoryInterface` trait. This allows to easily handle memory-mapped peripherals and
potentially reuse this code translator for a different emulator (like a NES emulator).

The dynamic compiler translates instructions until an unconditional control flow break (like a call
to a subroutine, an unconditional branch, a `brk` instruction or a return from subroutine or
interrupt) is encountered. The code is then assembled into a `CompiledBlock` which can be executed
and contains address translations from the target machine (6502) to the backend machine (arm).

When control flow is broken, the `VirtualMachine` exits with the PC register pointing to the next
instruction to execute. Then the user can enter the `VirtualMachine` can be resumed.

When the `VirtualMachine` starts running code it must determine whether there is already a compiled
block that matches the next instruction (contains it). If so, it simply jumps to that block at the
specified offset. Otherwise, it uses the `Compiler` to create a new block and then runs it.

### Project Status

The emulator is currently functional, although it has several limitations that need to be addressed:
- All taken branches and control flow breaks cause an exit from the virtual machine, even though
they could be speed up by tying inside a block and even outside blocks. This requires relocations,
which is why it hasn't been done yet.
- Decimal mode of the 6502 is not supported and probably will never be (I don't have a usecase
for it).
- Interrupts are currently unsupported but may be implemented in the future (although they are not
relevant for the Apple I).
- There is no cycle count at the moment, so this translator is not suited right now for a gaming
console emulator.
- Only official opcodes of the 6502 are implemented at the moment.

### Getting Started

#### Dependencies

The 6502 programs used to test the emulator, as well as the Woz Monitor code are provided in source
form and therefore a 6502 compiler is required. The LLVM MOS backend is used by the build system,
which can be obtained [here](https://github.com/llvm-mos/llvm-mos/releases/). Once downloaded and
uncompressed, set the `LLVM_MOS` environment variable to point to the binaries.

```bash
# For x86 linux
wget https://github.com/llvm-mos/llvm-mos/releases/download/llvm-mos-linux-main/llvm-mos-linux-main.tar.xz
tar -xf llvm-mos-linux-main.tar.xz
export LLVM_MOS=$PWD/llvm-mos
```

If you plan on running/testing on an x86/x64 machine you will need to emulate the armv8 code.
The userspace static version of `qemu` may be used for this purpose. You will also need a

```bash
# Install gcc-aarch64-linux-gnu and qemu-user-static
sudo apt update && sudo apt install -y gcc-aarch64-linux-gnu qemu-user-static
```

#### Building

```bash
# If you are using an armv8 machine
cargo build

# If you are using an x64 machine
cargo build --target=aarch64-unknown-linux-musl
```

Note that the reason for using musl instead of the gnu variant of the aarch64-unknown-linux
toolchain is that the standard library is statically linked with musl, so qemu does not require a
functional armv8 userspace in order to run this binary.

#### Running Tests

```bash
# If you are using an armv8 machine
cargo test

# If you are using an x64 machine
cargo test --target=aarch64-unknown-linux-musl
```
