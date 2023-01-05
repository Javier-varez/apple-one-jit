use apple_one_jit::memory::{MemoryInterface, TargetAddress};
use apple_one_jit::virtual_machine::{ExitReason, VirtualMachine, VmState};
use serde::Deserialize;

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));

struct Memory<'a> {
    memory: &'a std::cell::RefCell<Vec<u8>>,
}

impl<'a> MemoryInterface for Memory<'a> {
    extern "C" fn read_8_bits(&self, addr: TargetAddress) -> u8 {
        let mem = self.memory.borrow_mut();
        mem[addr as usize]
    }
    extern "C" fn read_16_bits(&self, addr: TargetAddress) -> u16 {
        let mem = self.memory.borrow();
        (mem[addr as usize] as u16) | ((mem[(addr + 1) as usize] as u16) << 8)
    }
    extern "C" fn write_8_bits(&mut self, addr: TargetAddress, data: u8) {
        let mut mem = self.memory.borrow_mut();
        mem[addr as usize] = data;
    }
}

#[derive(Debug, Deserialize)]
struct State {
    accumulator: u64,
    x: u64,
    y: u64,
    sp: u64,
    pc: u64,
    flags: Vec<char>,
    // Map of base addr + data at addr
    memory: Option<std::collections::HashMap<String, Vec<u8>>>,
}

#[derive(Debug, Deserialize)]
struct Test {
    entry_state: State,
    exit_state: State,
}

fn parse_u16(string: &str) -> u16 {
    if string.starts_with("0x") {
        u16::from_str_radix(string.trim_start_matches("0x"), 16).unwrap()
    } else {
        u16::from_str_radix(string, 10).unwrap()
    }
}

fn build_memory(program: &[u8], state: &State) -> Vec<u8> {
    let mut data = Vec::from(program);
    data.resize(0x1_0000, 0);

    if let Some(memory) = &state.memory {
        for (base, slice) in memory {
            let base = parse_u16(base);
            let dest = data.iter_mut().skip(base as usize);
            for (dest, src) in dest.zip(slice.iter()) {
                *dest = *src;
            }
        }
    }

    data
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

    for (test_case, test) in tests {
        let entry_memory = std::cell::RefCell::new(build_memory(program, &test.entry_state));
        let mut memory_interface = Memory {
            memory: &entry_memory,
        };

        let mut vm = VirtualMachine::new(&mut memory_interface);

        // Set initial state
        *vm.get_mut_state() = VmState {
            a: test.entry_state.accumulator,
            x: test.entry_state.x,
            y: test.entry_state.y,
            sp: test.entry_state.sp,
            pc: test.entry_state.pc,
            flags: translate_flags(&test.entry_state.flags),
        };

        loop {
            let reason = vm.run().unwrap();
            println!("next instr 0x{:x}", vm.get_state().pc);

            if reason == ExitReason::TestEnd {
                break;
            }
        }

        let expected_cpu_state = VmState {
            a: test.exit_state.accumulator,
            x: test.exit_state.x,
            y: test.exit_state.y,
            sp: test.exit_state.sp,
            pc: test.exit_state.pc,
            flags: translate_flags(&test.exit_state.flags),
        };

        assert_eq!(
            expected_cpu_state,
            *vm.get_state(),
            "Unexpected CPU state in test `{}::{}` (expected != actual)",
            test_name,
            test_case
        );

        let expected_memory = build_memory(program, &test.exit_state);
        assert_eq!(
            expected_memory,
            *entry_memory.borrow(),
            "Unexpected memory in test `{}::{}` (expected != actual)",
            test_name,
            test_case
        );
    }
}
