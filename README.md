# rust-lcm-codec

Rust support for reading and writing the [LCM data format](https://lcm-proj.github.io/).

## Overview

`rust-lcm-codec` is a Rust project for interacting with LCM encoded data. Other aspects of LCM,
like an RPC mechanism, are not supported by this project.

The `rust-lcm-codec` API is derived from the combination of the `rust-lcm-codec` runtime
library and code generated from your [LCM type specification](https://lcm-proj.github.io/type_specification.html)
data definitions/schemas by [rust-lcm-codegen](./rust-lcm-codegen/README.md).

### TODO

- [x] Works for `no_std`, no global allocator (read: embedded) use cases.
- [x] Builds on stable Rust.
- [x] Minimize performance costs of de/serialization.
- [x] Maximize de/serialization correctness through use of session types.
- [x] No rustc or clippy warnings in generated code.
- [ ] Support multidimensional arrays.
- [ ] Rigorously test for cross-implementation compatibility.

## Getting Started

This project requires a [Rust](https://www.rust-lang.org/) (stable) toolchain.
The recommended toolchain management system in Rust is [rustup](https://rustup.rs).

There are two key components to the `rust-lcm-codec` workflow:

1. Setup up your `build.rs` to use `rust-lcm-codegen` to generate API code.
2. Use the generated `rust-lcm-codec` API to interact with LCM encoded data.

## Usage

### `rust-lcm-codegen` setup

Add the following dependencies to your Rust project's `Cargo.toml`:

```toml
[dependencies]
rust-lcm-codec = "0.1.0"

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
    let out_path = Path::join(Path::new(&out_dir), "generated_lcm_api.rs");
    generate(schema_files, &out_path);
}
```

Include the generated API in your Rust project's code:

```rust
#![allow(non_snake_case)]
include!(concat!(env!("OUT_DIR"), "/generated_lcm_api.rs"));
```

### Generated API

Based on the above setup the generated types and API from your LCM schemas
would be in a file named `generated_lcm_api.rs` in the `OUT_DIR` from your last
`cargo build` or `cargo test` attempt.

You can find the location of this file this using:

```shell script
find ./target/ -name generated_lcm_api.rs -printf "%T@ %Tc %p\n" | sort -n
```

The file referenced at the end of the list will be the most recent one generated.

See [tests](tests) for more examples of how to use the
de/serializers exposed by the [generated](tests/generated) crate for the example
[schemas](schemas) included with this project.

## Tests

To run the tests for this project make sure you're in `/rust-lcm-codec`
and then run:

```shell script
cargo test
```

## License

See [LICENSE](./LICENSE) for more details.

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
