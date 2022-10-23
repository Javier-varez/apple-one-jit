###  Apple I, just in time

`apple-one-jit` is a dynamic recompiler that emulates the mos6502, coupled with the peripherals 
and memory of the apple I.

At this moment the code generation backend is only available for armv8 architectures, 
so it should work with Apple Silicon macs, as well as other armv8 devices like the Raspberry Pi 3 
or 4.

Adding a second backend for x86 or x64 should not be particularly complicated, but it is out of the 
scope of this project for me. However, if you're interested, feel free to contribute it.
