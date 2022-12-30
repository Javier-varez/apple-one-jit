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
    cmd!(sh, "{mos_6502_dir}/bin/ld.lld -T tests/test.ld -o {test_programs_dir}/{program_name} {test_programs_dir}/{program_name}.o").run().unwrap();
    cmd!(sh, "{mos_6502_dir}/bin/llvm-objcopy -O binary {out_dir}/test_programs/{program_name} {test_programs_dir}/{program_name}.bin").run().unwrap();

    std::path::Path::new(&out_dir)
        .join("test_programs")
        .join(program_name)
        .with_extension("bin")
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
    println!("cargo:rerun-if-changed=tests/test.ld");

    for folder in folders {
        let program_file = build_6502_program(&folder);
        let test_file = std::path::Path::new(&folder)
            .join("tests.toml")
            .canonicalize()
            .unwrap();
        let folder_name = folder.file_name().and_then(|e| e.to_str()).unwrap();

        write!(
            f,
            "
#[test]
fn {name}_test() {{
    let code = include_bytes!(\"{program_file}\");
    let tests = include_str!(\"{test_file}\");
    let test_name = \"{name}\";
    run_test(test_name, code, tests);
}}",
            name = folder_name,
            program_file = program_file.display(),
            test_file = test_file.display()
        )
        .unwrap();
    }
}
