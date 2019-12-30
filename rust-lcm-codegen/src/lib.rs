#![allow(unused_variables)]
#![allow(dead_code)]

pub mod fingerprint;
pub mod parser;

use crate::parser::{PrimitiveType, Type};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;

const STATE_NAME_READY: &str = "READY";
const STATE_NAME_DONE: &str = "DONE";

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

// TODO - see if we can / ought to collapse WriterState and ReaderState
struct ReaderState {
    state_name: String,
    /// The name of the LCM struct this state is for
    struct_name: String,
    /// field that's read when transitioning out of this state
    field: Option<parser::Field>,
}

impl ReaderState {
    fn ident(&self) -> Ident {
        format_ident!("{}_Read_{}", self.struct_name, self.state_name)
    }
}

fn emit_struct(s: &parser::Struct, env: &fingerprint::Environment) -> TokenStream {
    let schema_hash_ident = format_ident!("{}_SCHEMA_HASH", s.name.to_uppercase());
    let schema_hash = fingerprint::struct_hash(&s, &env);

    let mut writer_states = vec![];
    let mut reader_states = vec![];

    for (i, member) in s.members.iter().enumerate() {
        if let parser::StructMember::Field(f) = member {
            writer_states.push(WriterState {
                state_name: if i == 0 {
                    STATE_NAME_READY.to_string()
                } else {
                    f.name.to_owned()
                },
                struct_name: s.name.clone(),
                field: Some(f.clone()),
            });
            reader_states.push(ReaderState {
                state_name: if i == 0 {
                    STATE_NAME_READY.to_string()
                } else {
                    f.name.to_owned()
                },
                struct_name: s.name.clone(),
                field: Some(f.clone()),
            });
        }
    }

    writer_states.push(WriterState {
        state_name: STATE_NAME_DONE.to_string(),
        struct_name: s.name.clone(),
        field: None,
    });

    reader_states.push(ReaderState {
        state_name: STATE_NAME_DONE.to_string(),
        struct_name: s.name.clone(),
        field: None,
    });

    let writer_states_decl_code = writer_states
        .iter()
        .map(|ws| emit_writer_state_decl(&ws, &env));

    let reader_states_decl_code = reader_states
        .iter()
        .map(|rs| emit_reader_state_decl(&rs, &env));

    let mut writer_states_transition_code = vec![];
    for window in writer_states.windows(2) {
        if let [start_state, end_state] = window {
            writer_states_transition_code.push(emit_writer_state_transition(
                &start_state,
                &end_state,
                &env,
            ));
        } else {
            panic!("Unexpected window size in writer state transitions")
        }
    }
    let mut reader_states_transition_code = vec![];
    for window in reader_states.windows(2) {
        if let [start_state, end_state] = window {
            reader_states_transition_code.push(emit_reader_state_transition(
                &start_state,
                &end_state,
                &env,
            ));
        } else {
            panic!("Unexpected window size in reader state transitions")
        }
    }

    let mut main_struct_name = s.name.clone();
    main_struct_name[0..1].make_ascii_uppercase();
    let main_struct = format_ident!("{}", main_struct_name);
    let write_ready_type = writer_states[0].ident();
    let read_ready_type = reader_states[0].ident();

    quote! {
        pub const #schema_hash_ident : u64 = #schema_hash;

        pub struct #main_struct { }
        impl #main_struct {
            #[inline(always)]
            pub fn begin_write<'a, W: rust_lcm_codec::StreamingWriter>(writer: &'a mut W)
                    -> Result<#write_ready_type<'a, W>, W::Error> {
                writer.write_bytes(&#schema_hash.to_be_bytes())?;

                Ok(#write_ready_type {
                    writer
                })
            }
            pub fn begin_read<'a, R: rust_lcm_codec::StreamingReader>(reader: &'a mut R)
                    -> Result<#read_ready_type<'a, R>, rust_lcm_codec::DecodeFingerprintError<R::Error>> {
                let mut hash_buffer = 0u64.to_ne_bytes();
                reader.read_bytes(&mut hash_buffer)?;
                let found_hash = u64::from_be_bytes(hash_buffer);
                if found_hash != #schema_hash_ident {
                    return Err(rust_lcm_codec::DecodeFingerprintError::InvalidFingerprint(found_hash));
                }

                Ok(#read_ready_type {
                    reader
                })
            }
        }

        #( #writer_states_decl_code )*

        #( #writer_states_transition_code )*


        #( #reader_states_decl_code )*

        #( #reader_states_transition_code )*
    }
}

fn emit_writer_state_decl(ws: &WriterState, env: &fingerprint::Environment) -> TokenStream {
    let struct_ident = ws.ident();
    let allow_dead = if ws.state_name == STATE_NAME_DONE {
        Some(quote!(#[allow(dead_code)]))
    } else {
        None
    };
    quote! {
        pub struct #struct_ident<'a, W: rust_lcm_codec::StreamingWriter> {
            #allow_dead
            pub(super) writer: &'a mut W,
        }
    }
}

fn emit_reader_state_decl(rs: &ReaderState, env: &fingerprint::Environment) -> TokenStream {
    let struct_ident = rs.ident();
    let allow_dead = if rs.state_name == STATE_NAME_DONE {
        Some(quote!(#[allow(dead_code)]))
    } else {
        None
    };
    quote! {
        pub struct #struct_ident<'a, W: rust_lcm_codec::StreamingReader> {
            #allow_dead
            pub(super) reader: &'a mut W,
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
            let write_method_ident = format_ident!("write_{}", f.name);
            let write_method = match &f.ty {
                parser::Type::Primitive(pt) => {
                    let rust_field_type = format_ident!("{}", primitive_type_to_rust(&pt));
                    let write_invocation = match pt {
                        PrimitiveType::String => quote! {
                            rust_lcm_codec::write_str_value(val, self.writer)?;
                        },
                        _ => quote! {
                            use rust_lcm_codec::SerializeValue;
                            val.write_value(self.writer)?;
                        },
                    };
                    quote! {
                        pub fn #write_method_ident(self, val: & #rust_field_type) -> Result<#next_type<'a, W>, W::Error> {
                            #write_invocation
                            Ok(#next_type {
                                writer: self.writer,
                            })
                        }
                    }
                }
                parser::Type::Struct(st) => {
                    let field_struct_write_ready: Ident = WriterState {
                        state_name: STATE_NAME_READY.to_string(),
                        struct_name: st.name.clone(),
                        field: None,
                    }
                    .ident();
                    let field_struct_write_done: Ident = WriterState {
                        state_name: STATE_NAME_DONE.to_string(),
                        struct_name: st.name.clone(),
                        field: None,
                    }
                    .ident();
                    let struct_ns_prefix = if let Some(ns) = &st.namespace {
                        let namespace_ident = format_ident!("{}", ns);
                        Some(quote!(super::#namespace_ident::))
                    } else {
                        None
                    };
                    quote! {
                        pub fn #write_method_ident<F>(self, f: F) -> Result<#next_type<'a, W>, W::Error>
                            where F: FnOnce(#struct_ns_prefix#field_struct_write_ready<'a, W>) -> Result<#struct_ns_prefix#field_struct_write_done<'a, W>, W::Error>
                        {
                            let ready = #struct_ns_prefix#field_struct_write_ready {
                                writer: self.writer,
                            };
                            let done = f(ready)?;
                            Ok(#next_type {
                                writer: done.writer,
                            })
                        }
                    }
                }
                _ => unimplemented!(),
            };

            quote! {
                impl<'a, W: rust_lcm_codec::StreamingWriter> #start_type<'a, W> {
                    #[inline(always)]
                    #write_method
                }
            }
        }
        None => quote! {},
    }
}

fn emit_reader_state_transition(
    rs: &ReaderState,
    rs_next: &ReaderState,
    env: &fingerprint::Environment,
) -> TokenStream {
    match rs.field {
        Some(ref f) => {
            let start_type = rs.ident();
            let next_type = rs_next.ident();
            let read_method_ident = format_ident!("read_{}", f.name);
            let read_method_ident_into = format_ident!("read_{}_into", f.name);

            let read_methods = match &f.ty {
                Type::Primitive(pt) => {
                    let rust_field_type = Some(format_ident!("{}", primitive_type_to_rust(&pt)));
                    let next_state = quote! {
                        #next_type {
                            reader: self.reader,
                        }
                    };
                    match pt {
                        PrimitiveType::String => quote! {
                            pub fn #read_method_ident(self) -> Result<(&'a #rust_field_type, #next_type<'a, R>), rust_lcm_codec::DecodeValueError<R::Error>> {
                                let s = unsafe { core::mem::transmute(rust_lcm_codec::read_str_value(self.reader)?) };
                                Ok((s, #next_state))
                            }
                        },
                        _ => quote! {
                            pub fn #read_method_ident(self) -> Result<(#rust_field_type, #next_type<'a, R>), rust_lcm_codec::DecodeValueError<R::Error>> {
                                use rust_lcm_codec::SerializeValue;
                                let v = SerializeValue::read_new_value(self.reader)?;
                                Ok((v, #next_state))
                            }

                            pub fn #read_method_ident_into(self, val: &mut #rust_field_type) -> Result<#next_type<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>> {
                                use rust_lcm_codec::SerializeValue;
                                val.read_value(self.reader)?;
                                Ok(#next_state)
                            }
                        },
                    }
                }
                Type::Struct(st) => {
                    // TODO - incorporate namespace
                    let field_struct_read_ready: Ident = ReaderState {
                        state_name: STATE_NAME_READY.to_string(),
                        struct_name: st.name.clone(),
                        field: None,
                    }
                    .ident();
                    let field_struct_read_done: Ident = ReaderState {
                        state_name: STATE_NAME_DONE.to_string(),
                        struct_name: st.name.clone(),
                        field: None,
                    }
                    .ident();
                    let struct_ns_prefix = if let Some(ns) = &st.namespace {
                        let namespace_ident = format_ident!("{}", ns);
                        Some(quote!(super::#namespace_ident::))
                    } else {
                        None
                    };
                    quote! {
                        pub fn #read_method_ident<F>(self, f: F) -> Result<#next_type<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>>
                            where F: FnOnce(#struct_ns_prefix#field_struct_read_ready<'a, R>) -> Result<#struct_ns_prefix#field_struct_read_done<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>>
                        {
                            let ready = #struct_ns_prefix#field_struct_read_ready {
                                reader: self.reader,
                            };
                            let done = f(ready)?;
                            Ok(#next_type {
                                reader: done.reader,
                            })
                        }
                    }
                }
                Type::Array(_) => unimplemented!(),
            };

            quote! {
                impl<'a, R: rust_lcm_codec::StreamingReader> #start_type<'a, R> {

                    #[inline(always)]
                    #read_methods
                }
            }
        }
        None => quote! {},
    }
}
