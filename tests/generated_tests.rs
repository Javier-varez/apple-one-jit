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
    memory: Option<Vec<u8>>,
}

#[derive(Debug, Deserialize)]
struct Test {
    entry_state: State,
    exit_state: State,
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
                _ => panic!("Invalid flag {}", flag),
            }
    })
}

fn run_test(test_name: &str, program: &[u8], tests_str: &str) {
    let tests: std::collections::HashMap<String, Test> =
        toml::from_str(tests_str).expect("Invalid tests");
    let mut compiler = Compiler::new().unwrap();

    for (test_case, test) in tests {
        let mut cpu_state = CpuState {
            a: test.entry_state.accumulator,
            x: test.entry_state.x,
            y: test.entry_state.y,
            sp: test.entry_state.sp,
            pc: test.entry_state.pc,
            flags: translate_flags(&test.entry_state.flags),
        };

        let expected_cpu_state = CpuState {
            a: test.exit_state.accumulator,
            x: test.exit_state.x,
            y: test.exit_state.y,
            sp: test.exit_state.sp,
            pc: test.exit_state.pc,
            flags: translate_flags(&test.exit_state.flags),
        };

        let mut entry_memory = if let Some(memory) = test.entry_state.memory {
            memory
        } else {
            vec![]
        };
        entry_memory.resize(65536, 0);

        let mut expected_memory = if let Some(memory) = test.exit_state.memory {
            memory
        } else {
            vec![]
        };
        expected_memory.resize(65536, 0);

        compiler.translate_code(program).unwrap();
        unsafe { compiler.run(&mut cpu_state, &mut entry_memory) };

        assert_eq!(
            expected_cpu_state, cpu_state,
            "Unexpected CPU state in test `{}::{}` (expected != actual)",
            test_name, test_case
        );
        assert_eq!(
            expected_memory, entry_memory,
            "Unexpected memory in test `{}::{}` (expected != actual)",
            test_name, test_case
        );
    }
}