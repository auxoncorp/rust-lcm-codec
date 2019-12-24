use rust_lcm_codegen::generate;
use std::env;
use std::path::Path;
use std::fs::File;
use std::io::Read;

fn main() {
    println!("cargo:rerun-if-changed=../schemas");
    println!("cargo:rerun-if-changed=../../src");
    println!("cargo:rerun-if-changed=../../../rust-lcm-codegen");

    let schema_files = vec!["../schemas/primitives.lcm"];
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR");
    let out_path = Path::join(Path::new(&out_dir), "lcm.rs");

    generate(schema_files, &out_path);

    let mut out_file = File::open(&out_path).expect("open out file");
    let mut out_file_content = String::new();
    out_file.read_to_string(&mut out_file_content).expect("read out file");
    println!("{}", out_file_content);

    panic!("In the build.rs!");
}
