use std::io::Write;

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let test_file = std::path::Path::new(&out_dir).join("generated_tests.rs");
    let mut f = std::fs::File::create(&test_file).unwrap();

    let folders = std::fs::read_dir("tests/data")
        .unwrap()
        .map(|entry| entry.unwrap().path());

    for folder in folders {
        let state_file = std::path::Path::new(&folder)
            .join("state.txt")
            .canonicalize()
            .unwrap();
        let program_file = std::path::Path::new(&folder)
            .join("program.bin")
            .canonicalize()
            .unwrap();
        let folder_name = folder.file_name().and_then(|e| e.to_str()).unwrap();

        write!(
            f,
            "
#[test]
fn {name}_test() {{
    use apple_one_jit::dynamic_compiler::{{CpuState, Compiler}};
    use serde::Deserialize;

    #[derive(Debug, Deserialize)]
    struct State {{
        accumulator: u64,
        x: u64,
        y: u64,
        sp: u64,
        pc: u64,
        flags: u64,
    }}

    #[derive(Debug, Deserialize)]
    struct StateFile {{
        entry: State,
        exit: State,
    }}

    let code = include_bytes!(\"{program_file}\");
    let state: StateFile = serde_json::from_str(include_str!(\"{state_file}\")).unwrap();

    let mut cpu_state = CpuState {{
        a: state.entry.accumulator,
        x: state.entry.x,
        y: state.entry.y,
        sp: state.entry.sp,
        pc: state.entry.pc,
        flags: state.entry.flags,
    }};

    let expected_cpu_state = CpuState {{
        a: state.exit.accumulator,
        x: state.exit.x,
        y: state.exit.y,
        sp: state.exit.sp,
        pc: state.exit.pc,
        flags: state.exit.flags,
    }};

    let mut memory = [0u8; 256];

    let mut compiler = Compiler::new().unwrap();
    compiler
        .translate_code(code)
        .unwrap();
    unsafe {{ compiler.run(&mut cpu_state, &mut memory) }};

    assert_eq!(expected_cpu_state, cpu_state);
}}",
            name = folder_name,
            program_file = program_file.display(),
            state_file = state_file.display()
        )
        .unwrap();
    }
}
