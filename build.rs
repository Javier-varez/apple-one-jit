use std::io::Write;
use xshell::{cmd, Shell};

fn build_6502_program(folder: &std::path::Path) -> std::path::PathBuf {
    let mos_6502_dir = std::env::var("LLVM_MOS").unwrap();
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let test_programs_dir = std::path::Path::new(&out_dir).join("test_programs");

    let program_name = folder.file_name().and_then(|p| p.to_str()).unwrap();

    println!("cargo:rerun-if-changed={}/main.s", folder.display());

    let sh = Shell::new().unwrap();
    sh.create_dir(&test_programs_dir).unwrap();
    cmd!(sh, "{mos_6502_dir}/bin/llvm-mc --arch=mos {folder}/main.s --assemble --filetype=obj -o {test_programs_dir}/{program_name}.o").run().unwrap();
    cmd!(sh, "{mos_6502_dir}/bin/llvm-objcopy -O binary {out_dir}/test_programs/{program_name}.o {test_programs_dir}/{program_name}").run().unwrap();

    std::path::Path::new(&out_dir)
        .join("test_programs")
        .join(program_name)
        .canonicalize()
        .unwrap()
}

fn main() {
    let out_dir = std::env::var("OUT_DIR").unwrap();
    let test_file = std::path::Path::new(&out_dir).join("generated_tests.rs");
    let mut f = std::fs::File::create(&test_file).unwrap();

    let folders = std::fs::read_dir("tests/data")
        .unwrap()
        .map(|entry| entry.unwrap().path());

    println!("cargo:rerun-if-changed=tests/data");

    for folder in folders {
        let program_file = build_6502_program(&folder);
        let state_file = std::path::Path::new(&folder)
            .join("state.txt")
            .canonicalize()
            .unwrap();
        let folder_name = folder.file_name().and_then(|e| e.to_str()).unwrap();

        write!(
            f,
            "
#[test]
fn {name}_test() {{
    let code = include_bytes!(\"{program_file}\");
    let state = include_str!(\"{state_file}\");
    run_test(code, state);
}}",
            name = folder_name,
            program_file = program_file.display(),
            state_file = state_file.display()
        )
        .unwrap();
    }
}
