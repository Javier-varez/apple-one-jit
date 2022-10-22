#![allow(dead_code)]

mod arm_asm;
mod jit;

fn main() {
    jit::run_demo();
}
