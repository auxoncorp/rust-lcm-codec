#![allow(unused_variables)]
#![allow(dead_code)]

pub mod fingerprint;
pub mod parser;

use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;

pub fn generate<'a, P1: AsRef<Path>, SF: IntoIterator<Item = P1>, P2: AsRef<Path>>(
    schema_files: SF,
    out_file_path: P2,
) {
    let out_file_path: &Path = out_file_path.as_ref();
    let mut out_file = File::create(out_file_path).expect("Create out file");

    let mut all_schemas = vec![];
    for schema_file in schema_files.into_iter() {
        let mut schema = File::open(schema_file.as_ref()).expect("Open schema");
        let mut schema_content = String::new();
        schema
            .read_to_string(&mut schema_content)
            .expect("Read schema");

        let (remaining, ast) = parser::schema(&schema_content).expect("Parse schema");
        assert_eq!(remaining, "", "Unparsed text at end of schema");
        all_schemas.push(ast);
    }

    let schemas_code = all_schemas.iter().map(|schema| {
        let env = fingerprint::Environment {
            local_schema: schema.clone(),
            all_schemas: all_schemas.clone(),
        };

        emit_schema(&schema, &env)
    });

    let tokens = quote! {
        #(#schemas_code)*
    };

    write!(out_file, "{}", tokens).expect("Write out file");
    println!("{}", out_file_path.display());
    rustfmt(out_file_path);
    // println!("{}", tokens);
}

fn rustfmt<P: AsRef<Path>>(path: P) {
    let path = path.as_ref();

    Command::new("rustfmt")
        .arg("--edition")
        .arg("2018")
        .arg(path.as_os_str())
        .output()
        .expect("rustfmt");
}

fn emit_schema(schema: &parser::Schema, env: &fingerprint::Environment) -> TokenStream {
    let structs_code = schema.structs.iter().map(|s| emit_struct(s, env));
    match &schema.package {
        Some(name) => {
            let mod_ident = format_ident!("{}", name);
            quote! {
                mod #mod_ident {
                    #(#structs_code)*
                }
            }
        }
        None => quote! {
            #(#structs_code)*
        },
    }
}

// struct BoundArrayDimension {
//     name: String,
//     index: usize,
// }

struct WriterState {
    struct_name: String,
    name: String,
    // bound_array_dimensions: Vec<BoundArrayDimension>,
}

impl WriterState {
    fn ident(&self) -> Ident {
        format_ident!("{}_Write_{}", self.struct_name, self.name)
    }
}

fn emit_struct(s: &parser::Struct, env: &fingerprint::Environment) -> TokenStream {
    let schema_hash_ident = format_ident!("{}_SCHEMA_HASH", s.name.to_uppercase());
    let schema_hash = fingerprint::struct_hash(&s, &env);

    let mut writer_states = vec![WriterState {
        struct_name: s.name.clone(),
        name: "READY".to_string(),
    }];

    for member in s.members.iter() {
        if let parser::StructMember::Field(f) = member {
            writer_states.push(WriterState {
                struct_name: s.name.clone(),
                name: f.name.clone(),
            });
        }
    }

    writer_states.push(WriterState {
        struct_name: s.name.clone(),
        name: "DONE".to_string(),
    });

    let writer_states_decl_code = writer_states
        .iter()
        .map(|ws| emit_writer_state_decl(&ws, &env));

    quote! {
        const #schema_hash_ident : u64 = #schema_hash;
        #( #writer_states_decl_code )*
    }
}

fn emit_writer_state_decl(ws: &WriterState, env: &fingerprint::Environment) -> TokenStream {
    let struct_ident = ws.ident();
    quote! {
        struct #struct_ident<'a, W: rust_lcm_codec::StreamingWriter> {
            writer: &'a mut W,
        }
    }
}
