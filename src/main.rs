#![allow(dead_code)]

use apple_one_jit::dynamic_compiler::{Compiler, CpuState};

fn main() {
    let mut state = CpuState::default();
    let mut memory = [0u8; 256];

    let mut compiler = Compiler::new().unwrap();
    compiler
        .translate_code(&[0xA9, 0x0A, 0x69, 0x14, 0x60])
        .unwrap();
    unsafe { compiler.run(&mut state, &mut memory) };
}
