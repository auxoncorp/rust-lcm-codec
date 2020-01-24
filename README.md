# rust-lcm-code*

Rust support for reading and writing the [LCM data format](https://lcm-proj.github.io/).

## Overview

[rust-lcm-codegen](rust-lcm-codegen) generates de/serialization Rust code from [LCM type specification](https://lcm-proj.github.io/type_specification.html)
files. It is intended to be usable from a Cargo build.rs script.

[rust-lcm-codec](rust-lcm-codec) contains the runtime library that is used by the generated code
to accomplish common de/serialization tasks.


## Goals

* Works for `no_std`, no global allocator use cases (read: embedded systems)
  * The big trick here is to use iterators rather than materializing `Vec` instances
  for array types.
* Builds on stable Rust
* Minimize performance costs of serialization and deserialization
* Maximize correctness of serialization and deserialization
  * Use session types to enforce valid ordering of de/serialization actions
  * Compatible with other implementations of LCM
* Generated code should produce no rustc or clippy warnings.
  
## Examples

### Inline Example

In your Rust project's Cargo.toml:

```toml
[dependencies]
rust-lcm-codec = "0.1.0"

[build-dependencies]
rust-lcm-codegen = "0.1.0"
```

In your Rust project's `build.rs` file:

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

In your Rust project's code:

```rust
#![allow(non_snake_case)]
include!(concat!(env!("OUT_DIR"), "/generated_lcm.rs"));
```

### More Examples

See also the tiny crate in [rust-lcm-codec/tests/generated](rust-lcm-codec/tests/generated)
for a complete example of how to set up code generation from [LCM schema](rust-lcm-codec/tests/schemas).

See [rust-lcm-codec/tests](rust-lcm-codec/tests) for examples of how to use the
de/serializers exposed by the [generated](rust-lcm-codec/tests/generated) crate.

## Caveats

* Does not yet support multidimensional arrays.
* Not yet rigorously tested for cross-implementation compatibility

