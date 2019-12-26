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
                #[allow(non_camel_case_types)]
                pub mod #mod_ident {
                    #(#structs_code)*
                }
            }
        }
        None => quote! {
            #(#structs_code)*
        },
    }
}

struct WriterState {
    state_name: String,
    /// The name of the LCM struct this state is for
    struct_name: String,
    /// field that's written when transitioning out of this state
    field: Option<parser::Field>,
}

impl WriterState {
    fn ident(&self) -> Ident {
        format_ident!("{}_Write_{}", self.struct_name, self.state_name)
    }
}

fn emit_struct(s: &parser::Struct, env: &fingerprint::Environment) -> TokenStream {
    let schema_hash_ident = format_ident!("{}_SCHEMA_HASH", s.name.to_uppercase());
    let schema_hash = fingerprint::struct_hash(&s, &env);

    let mut writer_states = vec![];

    for i in 0..s.members.len() {
        let member = &s.members[i];
        if let parser::StructMember::Field(f) = member {
            writer_states.push(WriterState {
                state_name: if i == 0 {
                    "READY".to_string()
                } else {
                    f.name.to_owned()
                },
                struct_name: s.name.clone(),
                field: Some(f.clone()),
            });
        }
    }

    writer_states.push(WriterState {
        state_name: "DONE".to_string(),
        struct_name: s.name.clone(),
        field: None,
    });

    let writer_states_decl_code = writer_states
        .iter()
        .map(|ws| emit_writer_state_decl(&ws, &env));

    let mut writer_states_transition_code = vec![];
    for i in 1..writer_states.len() {
        let start_state = &writer_states[i - 1];
        let end_state = &writer_states[i];
        writer_states_transition_code.push(emit_writer_state_transition(
            &start_state,
            &end_state,
            &env,
        ));
    }

    let mut main_struct_name = s.name.clone();
    main_struct_name[0..1].make_ascii_uppercase();
    let main_struct = format_ident!("{}", main_struct_name);
    let ready_type = writer_states[0].ident();

    quote! {
        pub const #schema_hash_ident : u64 = #schema_hash;

        pub struct #main_struct { }
        impl #main_struct {
            #[inline(always)]
            pub fn begin_write<'a, W: rust_lcm_codec::StreamingWriter>(writer: &'a mut W) -> Result<#ready_type<'a, W>, W::Error> {
                writer.write_bytes(&#schema_hash.to_be_bytes())?;

                Ok(#ready_type {
                    writer
                })
            }
        }

        #( #writer_states_decl_code )*


        #( #writer_states_transition_code )*
    }
}

fn emit_writer_state_decl(ws: &WriterState, env: &fingerprint::Environment) -> TokenStream {
    let struct_ident = ws.ident();
    if ws.field.is_some() {
        quote! {
            pub struct #struct_ident<'a, W: rust_lcm_codec::StreamingWriter> {
                writer: &'a mut W,
            }
        }
    } else {
        // Don't include the writer for the DONE state, which has no fields to write
        quote! {
            pub struct #struct_ident<'a, W: rust_lcm_codec::StreamingWriter> {
                _phantom_writer: core::marker::PhantomData<&'a mut W>,
            }
        }
    }
}

fn primitive_type_to_rust(pt: &parser::PrimitiveType) -> &str {
    match pt {
        parser::PrimitiveType::Int8 => "i8",
        parser::PrimitiveType::Int16 => "i16",
        parser::PrimitiveType::Int32 => "i32",
        parser::PrimitiveType::Int64 => "i64",
        parser::PrimitiveType::Float => "f32",
        parser::PrimitiveType::Double => "f64",
        parser::PrimitiveType::String => "str",
        parser::PrimitiveType::Boolean => "bool",
        parser::PrimitiveType::Byte => "u8",
    }
}

fn emit_writer_state_transition(
    ws: &WriterState,
    ws_next: &WriterState,
    env: &fingerprint::Environment,
) -> TokenStream {
    match ws.field {
        Some(ref f) => {
            let start_type = ws.ident();
            let next_type = ws_next.ident();
            let write_method = format_ident!("write_{}", f.name); //unimplemented!();
            let rust_field_type = match f.ty {
                parser::Type::Primitive(pt) => {
                    Some(format_ident!("{}", primitive_type_to_rust(&pt)))
                }
                _ => unimplemented!(),
            };

            let next_state = if ws_next.field.is_some() {
                quote! {
                    #next_type {
                        writer: self.writer,
                    }
                }
            } else {
                quote! {
                    #next_type {
                        _phantom_writer: core::marker::PhantomData { },
                    }
                }
            };

            quote! {
                impl<'a, W: rust_lcm_codec::StreamingWriter> #start_type<'a, W> {
                    #[inline(always)]
                    pub fn #write_method(self, val: & #rust_field_type) -> Result<#next_type<'a, W>, W::Error> {
                        use rust_lcm_codec::SerializeValue;
                        val.write_value(self.writer)?;

                        Ok(#next_state)
                    }
                }
            }
        }
        None => quote! {},
    }
}
