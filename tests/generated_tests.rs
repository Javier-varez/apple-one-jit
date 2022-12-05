use apple_one_jit::dynamic_compiler::{Compiler, CpuState};
use serde::Deserialize;

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

#[derive(Debug, Deserialize)]
struct State {
    accumulator: u64,
    x: u64,
    y: u64,
    sp: u64,
    pc: u64,
    flags: Vec<char>,
}

#[derive(Debug, Deserialize)]
struct StateFile {
    entry: State,
    exit: State,
}

fn translate_flags(flags: &[char]) -> u64 {
    const N: u64 = 0x8000_0000;
    const Z: u64 = 0x4000_0000;
    const C: u64 = 0x2000_0000;
    const V: u64 = 0x1000_0000;

    flags.iter().fold(0, |folded, flag| {
        folded
            | match flag {
                'N' => N,
                'Z' => Z,
                'C' => C,
                'V' => V,
                _ => unreachable!(),
            }
    })
}

fn run_test(program: &[u8], state_str: &str) {
    let state: StateFile = serde_json::from_str(state_str).unwrap();

    let mut cpu_state = CpuState {
        a: state.entry.accumulator,
        x: state.entry.x,
        y: state.entry.y,
        sp: state.entry.sp,
        pc: state.entry.pc,
        flags: translate_flags(&state.entry.flags),
    };

    let expected_cpu_state = CpuState {
        a: state.exit.accumulator,
        x: state.exit.x,
        y: state.exit.y,
        sp: state.exit.sp,
        pc: state.exit.pc,
        flags: translate_flags(&state.exit.flags),
    };

    // TODO(javier-varez): move this to the state file
    let mut memory = [0u8; 256];

    let mut compiler = Compiler::new().unwrap();
    compiler.translate_code(program).unwrap();
    unsafe {
        {
            compiler.run(&mut cpu_state, &mut memory)
        }
    };

    assert_eq!(expected_cpu_state, cpu_state);
}
