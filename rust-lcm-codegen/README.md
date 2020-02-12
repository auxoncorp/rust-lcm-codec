# rust-lcm-codegen

Generates Rust de/serialization code from [LCM type specification](https://lcm-proj.github.io/type_specification.html)
files.

## Overview

This is a sub-project of [rust-lcm-codec](../README.md) which specifically handles the
code generation which provides the Rust API to LCM encoded data. It is intended to be
usable from a Cargo `build.rs` script.

## Getting Started

This library requires a [Rust](https://www.rust-lang.org/) toolchain.
The recommended toolchain management system in Rust is [rustup](https://rustup.rs).

Once rustup is installed, you can build for your local device with:

```shell script
cargo build
```

## Usage

Add the following dependencies to your Rust project's `Cargo.toml`:

```toml
[build-dependencies]
rust-lcm-codegen = "0.1.0"
```

Import `rust-lcm-codegen:generate` in your Rust project's `build.rs` file,
and point the `generate` function at your `.lcm` schemas and desired
output location for the generated code:

```rust
use rust_lcm_codegen::generate;
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=../schemas");
    println!("cargo:rerun-if-changed=../../src");
    println!("cargo:rerun-if-changed=../../../rust-lcm-codegen");

    let schema_files = vec![
        "./schemas/example_schema_a.lcm",
        "path/to/schemas/example_schema_b.lcm",
    ];
    let out_dir = env::var("OUT_DIR").expect("OUT_DIR");
    let out_path = Path::join(Path::new(&out_dir), "generated_lcm.rs");
    generate(schema_files, &out_path);
}
```

## Tests

To run the tests for this project make sure you're in `/rust-lcm-codec/rust-lcm-codegen`
and then run:

```shell script
cargo test
```

## License

See [LICENSE](../LICENSE) for more details.

Copyright 2020 Auxon Corporation

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
