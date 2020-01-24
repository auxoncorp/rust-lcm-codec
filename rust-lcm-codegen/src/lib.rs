//! Code generation for LCM serialization and deserialization in Rust
#![allow(unused_variables)]
#![allow(dead_code)]
#![deny(warnings)]

pub mod fingerprint;
pub mod parser;

use crate::parser::{ArrayDimension, ArrayType, PrimitiveType, StructType, Type};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};
use std::fs::File;
use std::io::{Read, Write};
use std::path::Path;
use std::process::Command;

/// Generate a single Rust file from a collection of LCM schema files.
pub fn generate<P1: AsRef<Path>, SF: IntoIterator<Item = P1>, P2: AsRef<Path>>(
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
    // TODO - either merge schema contents in the same package
    // or error out when more than one file declares the same package

    let schemas_code = all_schemas.iter().map(|schema| {
        let env = Environment {
            local_schema: schema.clone(),
            all_schemas: all_schemas.clone(),
        };

        emit_schema(&schema, &env)
    });

    let tokens = quote! {
        #(#schemas_code)*
    };

    write!(out_file, "{}", tokens).expect("Write out file");
    rustfmt(out_file_path);
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

fn emit_schema(schema: &parser::Schema, env: &Environment) -> TokenStream {
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

#[derive(Debug, PartialEq, Eq)]
enum StateName {
    Ready,
    HandlingField(String),
    Done,
}

impl StateName {
    fn name(&self) -> &str {
        match self {
            StateName::Ready => "ready",
            StateName::HandlingField(s) => s.as_str(),
            StateName::Done => "done",
        }
    }
}

#[derive(Debug)]
struct CodecState {
    state_name: StateName,
    /// The name of the LCM struct this state is for
    struct_name: String,
    /// field that's written when transitioning out of this state,
    /// and whether that value needs to be captured for use in
    /// tracking the length of an array
    field: Option<(parser::Field, bool)>,
    /// The array-length values that this state needs to pass along to
    /// future states for the purposes of correctly sizing arrays,
    /// identified by the name of the field they serve
    baggage_dimensions: Vec<BaggageDimension>,
}

impl CodecState {
    fn writer_struct_state_decl_ident(struct_name: &str, state_name: &StateName) -> Ident {
        format_ident!("{}_write_{}", struct_name, state_name.name())
    }
    fn reader_struct_state_decl_ident(struct_name: &str, state_name: &StateName) -> Ident {
        format_ident!("{}_read_{}", struct_name, state_name.name())
    }
    fn writer_ident(&self) -> Ident {
        CodecState::writer_struct_state_decl_ident(&self.struct_name, &self.state_name)
    }
    fn reader_ident(&self) -> Ident {
        CodecState::reader_struct_state_decl_ident(&self.struct_name, &self.state_name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct BaggageDimension {
    array_field_name: String,
    len_field_name: String,
    dimension_depth: usize,
}

impl BaggageDimension {
    fn field_declarations(baggage_dimensions: &[BaggageDimension]) -> Vec<TokenStream> {
        baggage_dimensions
            .iter()
            .map(|d| {
                let field_ident = format_ident!("baggage_{}", d.len_field_name);
                quote!(#field_ident: usize,)
            })
            .collect()
    }
    fn field_initializations_from_self<'a>(
        baggage_dimensions: impl IntoIterator<Item = &'a BaggageDimension>,
    ) -> Vec<TokenStream> {
        baggage_dimensions
            .into_iter()
            .map(|d| {
                let baggage_field_ident = format_ident!("baggage_{}", d.len_field_name);
                quote!(#baggage_field_ident: self.#baggage_field_ident)
            })
            .collect()
    }
}

fn to_underscored_literal(v: u64) -> proc_macro2::Literal {
    use std::str::FromStr;
    let raw = format!("{}", v);
    let original_len = raw.len();
    let mut s = String::with_capacity(original_len);
    for (index, digit) in raw.chars().rev().enumerate() {
        if index % 3 == 0 && index != 0 && index != original_len {
            s.insert(0, '_')
        }
        s.insert(0, digit)
    }
    s.push_str("u64");
    if let proc_macro2::TokenTree::Literal(l) = proc_macro2::TokenStream::from_str(&s)
        .expect("Invalid underscored literal creation, failed lexing")
        .into_iter()
        .next()
        .expect("Should have made at least one token")
    {
        l
    } else {
        panic!("Created the wrong type of token when trying to make an underscored literal")
    }
}

fn emit_struct(s: &parser::Struct, env: &Environment) -> TokenStream {
    let schema_hash_ident = format_ident!("{}_SCHEMA_HASH", s.name.to_uppercase());
    let schema_hash = fingerprint::struct_hash(&s, &env);
    let schema_hash = to_underscored_literal(schema_hash);

    let codec_states = gather_states(s);

    let writer_states_decl_code = codec_states
        .iter()
        .map(|ws| emit_writer_state_decl(&ws, &env));

    let reader_states_decl_code = codec_states
        .iter()
        .map(|rs| emit_reader_state_decl(&rs, &env));

    let mut writer_states_transition_code = vec![];
    let mut reader_states_transition_code = vec![];
    for window in codec_states.windows(2) {
        if let [start_state, end_state] = window {
            writer_states_transition_code.push(emit_writer_state_transition(
                &start_state,
                &end_state,
                &env,
            ));
            reader_states_transition_code.push(emit_reader_state_transition(
                &start_state,
                &end_state,
                &env,
            ));
        } else {
            panic!("Unexpected window size in state transitions")
        }
    }

    let write_ready_type = codec_states[0].writer_ident();
    let read_ready_type = codec_states[0].reader_ident();
    let begin_write = format_ident!("begin_{}_write", s.name);
    let begin_read = format_ident!("begin_{}_read", s.name);

    quote! {
        pub const #schema_hash_ident : u64 = #schema_hash;

            #[inline]
            pub fn #begin_write<W: rust_lcm_codec::StreamingWriter>(writer: &'_ mut W)
                    -> Result<#write_ready_type<'_, W>, rust_lcm_codec::EncodeFingerprintError<W::Error>> {
                writer.write_bytes(&#schema_hash.to_be_bytes())?;

                Ok(#write_ready_type {
                    writer
                })
            }
            #[inline]
            pub fn #begin_read<R: rust_lcm_codec::StreamingReader>(reader: &'_ mut R)
                    -> Result<#read_ready_type<'_, R>, rust_lcm_codec::DecodeFingerprintError<R::Error>> {
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

        #( #writer_states_decl_code )*

        #( #writer_states_transition_code )*


        #( #reader_states_decl_code )*

        #( #reader_states_transition_code )*
    }
}

fn gather_states(s: &parser::Struct) -> Vec<CodecState> {
    let mut codec_states = Vec::new();

    codec_states.insert(
        0,
        CodecState {
            state_name: StateName::Done,
            struct_name: s.name.clone(),
            field: None,
            baggage_dimensions: Vec::with_capacity(0),
        },
    );

    // Iterate backwards to collect and manage required dimensional metadata
    // Note that the current approach does not handle multidimensional arrays
    let mut baggage_dimensions = Vec::new();
    for (i, member) in s.members.iter().enumerate().rev() {
        if let parser::StructMember::Field(f) = member {
            let mut local_dynamic_dimensions = vec![];
            if let Type::Array(at) = &f.ty {
                for (depth, dim) in at.dimensions.iter().enumerate() {
                    if let ArrayDimension::Dynamic { field_name } = dim {
                        local_dynamic_dimensions.push(BaggageDimension {
                            array_field_name: f.name.clone(),
                            len_field_name: field_name.clone(),
                            dimension_depth: depth,
                        })
                    }
                }
            }
            baggage_dimensions.extend(local_dynamic_dimensions.clone());
            if local_dynamic_dimensions.len() > 1 {
                panic!("Arrays with more than one dimension are not yet supported");
            }
            let mut field_serves_as_dimension = false;
            while let Some(bi) = baggage_dimensions
                .iter()
                .position(|dim| dim.len_field_name == f.name.as_str())
            {
                // This dimension will be discharged by the transition out of this state, no need to
                // keep tracking it
                let bd = baggage_dimensions.remove(bi);
                field_serves_as_dimension = true;
            }
            codec_states.insert(
                0,
                CodecState {
                    state_name: if i == 0 {
                        StateName::Ready
                    } else {
                        StateName::HandlingField(f.name.to_owned())
                    },
                    struct_name: s.name.clone(),
                    field: Some((f.clone(), field_serves_as_dimension)),
                    baggage_dimensions: baggage_dimensions.clone(),
                },
            );
        }
    }
    codec_states
}

fn emit_writer_state_decl(ws: &CodecState, env: &Environment) -> TokenStream {
    let struct_ident = ws.writer_ident();
    let allow_dead = if ws.state_name == StateName::Done {
        Some(quote!(#[allow(dead_code)]))
    } else {
        None
    };
    let dimensions_fields = BaggageDimension::field_declarations(&ws.baggage_dimensions);
    let (current_iter_count_field, array_item_writer_decl) = if let Some((
        parser::Field {
            ty: parser::Type::Array(at),
            name,
        },
        _,
    )) = &ws.field
    {
        let current_count_field_ident = at
            .array_current_count_field_ident(name.as_str(), 0)
            .expect("Arrays must have at least one dimension");
        let item_writer_struct_ident = format_ident!("{}_item", struct_ident);
        let array_item_writer_decl = quote! {
            #[must_use]
            pub struct #item_writer_struct_ident<'a, W: rust_lcm_codec::StreamingWriter> {
                parent: &'a mut #struct_ident<'a, W>,
            }
        };
        (
            Some(quote!(#current_count_field_ident: usize, )),
            Some(array_item_writer_decl),
        )
    } else {
        (None, None)
    };
    let maybe_must_use = if ws.state_name != StateName::Done {
        Some(quote!(#[must_use]))
    } else {
        None
    };
    quote! {
        #maybe_must_use
        pub struct #struct_ident<'a, W: rust_lcm_codec::StreamingWriter> {
            #allow_dead
            pub(super) writer: &'a mut W,
            #current_iter_count_field
            #( #dimensions_fields )*
        }

        #array_item_writer_decl
    }
}

fn emit_reader_state_decl(rs: &CodecState, env: &Environment) -> TokenStream {
    let struct_ident = rs.reader_ident();
    let allow_dead = if rs.state_name == StateName::Done {
        Some(quote!(#[allow(dead_code)]))
    } else {
        None
    };
    let dimensions_fields = BaggageDimension::field_declarations(&rs.baggage_dimensions);
    let (current_iter_count_field, array_item_reader_decl) = if let Some((
        parser::Field {
            ty: parser::Type::Array(at),
            name,
        },
        _,
    )) = &rs.field
    {
        let current_count_field_ident = at
            .array_current_count_field_ident(name.as_str(), 0)
            .expect("Arrays must have at least one dimension");
        let item_reader_struct_ident = format_ident!("{}_item", struct_ident);
        let array_item_reader_decl = quote! {
            #[must_use]
            pub struct #item_reader_struct_ident<'a, R: rust_lcm_codec::StreamingReader> {
                parent: &'a mut #struct_ident<'a, R>,
            }
        };
        (
            Some(quote!(#current_count_field_ident: usize, )),
            Some(array_item_reader_decl),
        )
    } else {
        (None, None)
    };
    let maybe_must_use = if rs.state_name != StateName::Done {
        Some(quote!(#[must_use]))
    } else {
        None
    };
    quote! {
        #maybe_must_use
        pub struct #struct_ident<'a, W: rust_lcm_codec::StreamingReader> {
            #allow_dead
            pub(super) reader: &'a mut W,
            #current_iter_count_field
            #( #dimensions_fields )*
        }

        #array_item_reader_decl
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
    ws: &CodecState,
    ws_next: &CodecState,
    env: &Environment,
) -> TokenStream {
    match ws.field {
        Some((ref f, serves_as_dimension)) => {
            let start_type = ws.writer_ident();
            let next_type = ws_next.writer_ident();
            let write_method_ident = format_ident!("write_{}", f.name);
            match &f.ty {
                parser::Type::Primitive(pt) => emit_writer_field_state_transition_primitive(
                    start_type,
                    ws_next,
                    f.name.as_str(),
                    *pt,
                    serves_as_dimension,
                ),
                parser::Type::Struct(st) => emit_writer_field_state_transition_struct(
                    start_type,
                    ws_next,
                    f.name.as_str(),
                    st,
                ),
                parser::Type::Array(at) => emit_writer_field_state_transition_array(
                    start_type,
                    ws_next,
                    f.name.as_str(),
                    at,
                ),
            }
        }
        None => quote! {},
    }
}

#[derive(Copy, Clone, Debug)]
enum WriterPath {
    Bare,
    ViaSelf,
    ViaSelfParent,
}

impl WriterPath {
    fn path(self) -> TokenStream {
        match self {
            WriterPath::Bare => quote!(writer),
            WriterPath::ViaSelf => quote!(self.writer),
            WriterPath::ViaSelfParent => quote!(self.parent.writer),
        }
    }
}
fn emit_write_primitive_invocation(pt: PrimitiveType, writer_path: WriterPath) -> TokenStream {
    let path = writer_path.path();
    match pt {
        PrimitiveType::String => quote! {
            rust_lcm_codec::write_str_value(val, #path)?;
        },
        _ => quote! {
            rust_lcm_codec::SerializeValue::write_value(val, #path)?;
        },
    }
}

fn emit_next_field_current_iter_count_initialization(
    next_state: &CodecState,
) -> Option<TokenStream> {
    if let Some((
        parser::Field {
            ty: parser::Type::Array(at),
            name,
        },
        _,
    )) = &next_state.field
    {
        let current_iter_count_field_ident = at
            .array_current_count_field_ident(name.as_str(), 0)
            .expect("Arrays must have at least one dimension");
        Some(quote!(#current_iter_count_field_ident: 0, ))
    } else {
        None
    }
}

fn emit_writer_field_state_transition_primitive(
    start_type: Ident,
    next_state: &CodecState,
    field_name: &str,
    pt: PrimitiveType,
    field_serves_as_dimension: bool,
) -> TokenStream {
    let write_method_ident = format_ident!("write_{}", field_name);
    let write_method = {
        let maybe_ref = if pt == PrimitiveType::String {
            Some(quote!(&))
        } else {
            None
        };
        let rust_field_type = format_ident!("{}", primitive_type_to_rust(&pt));
        let write_invocation = emit_write_primitive_invocation(pt, WriterPath::ViaSelf);
        let dimensional_capture = if field_serves_as_dimension {
            let baggage_field_ident = format_ident!("baggage_{}", field_name);
            Some(quote!(#baggage_field_ident: val as usize,))
        } else {
            None
        };
        let next_type = next_state.writer_ident();
        let next_dimensions_fields = BaggageDimension::field_initializations_from_self(
            next_state
                .baggage_dimensions
                .iter()
                .filter(|d| !field_serves_as_dimension || d.len_field_name.as_str() != field_name),
        );
        let current_iter_count_initialization =
            emit_next_field_current_iter_count_initialization(next_state);
        quote! {
            #[inline]
            pub fn #write_method_ident(self, val: #maybe_ref #rust_field_type) -> Result<#next_type<'a, W>, rust_lcm_codec::EncodeValueError<W::Error>> {
                #write_invocation
                Ok(#next_type {
                    writer: self.writer,
                    #dimensional_capture
                    #current_iter_count_initialization
                    #( #next_dimensions_fields )*
                })
            }
        }
    };

    quote! {
        impl<'a, W: rust_lcm_codec::StreamingWriter> #start_type<'a, W> {
            #[inline]
            #write_method
        }
    }
}

fn emit_write_struct_method(
    st: &StructType,
    write_method_ident: Ident,
    pre_field_write: Option<TokenStream>,
    post_field_write: Option<TokenStream>,
    after_field_type: TokenStream,
    after_field_constructor: TokenStream,
    writer_path: WriterPath,
) -> TokenStream {
    let field_struct_write_ready: Ident =
        CodecState::writer_struct_state_decl_ident(&st.name, &StateName::Ready);
    let field_struct_write_done: Ident =
        CodecState::writer_struct_state_decl_ident(&st.name, &StateName::Done);
    let struct_ns_prefix = if let Some(ns) = &st.namespace {
        let namespace_ident = format_ident!("{}", ns);
        Some(quote!(super::#namespace_ident::))
    } else {
        None
    };
    let writer_path_tokens = writer_path.path();
    quote! {
        #[inline]
        pub fn #write_method_ident<F>(self, f: F) -> Result<#after_field_type, rust_lcm_codec::EncodeValueError<W::Error>>
            where F: FnOnce(#struct_ns_prefix#field_struct_write_ready<'a, W>)
                -> Result<#struct_ns_prefix#field_struct_write_done<'a, W>, rust_lcm_codec::EncodeValueError<W::Error>>
        {
            #pre_field_write
            let ready = #struct_ns_prefix#field_struct_write_ready {
                writer: #writer_path_tokens,
            };
            #[allow(unused_variables)]
            let done = f(ready)?;
            #post_field_write
            Ok(#after_field_constructor)
        }
    }
}

fn emit_writer_field_state_transition_struct(
    start_type: Ident,
    next_state: &CodecState,
    field_name: &str,
    st: &StructType,
) -> TokenStream {
    let next_type = next_state.writer_ident();
    let write_method_ident = format_ident!("write_{}", field_name);
    let after_field_type = quote!(#next_type<'a, W>);

    let current_iter_count_initialization =
        emit_next_field_current_iter_count_initialization(next_state);
    let next_dimensions_fields =
        BaggageDimension::field_initializations_from_self(&next_state.baggage_dimensions);
    let after_field_constructor = quote! {
                #next_type {
                    writer: done.writer,
                    #current_iter_count_initialization
                    #( #next_dimensions_fields )*
                }
    };
    let write_method = emit_write_struct_method(
        st,
        write_method_ident,
        None,
        None,
        after_field_type,
        after_field_constructor,
        WriterPath::ViaSelf,
    );
    quote! {
        impl<'a, W: rust_lcm_codec::StreamingWriter> #start_type<'a, W> {
            #[inline]
            #write_method
        }
    }
}

impl ArrayType {
    fn array_current_count_field_ident(
        &self,
        array_field_name: &str,
        index: usize,
    ) -> Option<Ident> {
        match self.dimensions.get(index) {
            Some(ArrayDimension::Static { size }) => {
                // Use the field_name of the array
                Some(format_ident!("current_{}_count", array_field_name))
            }
            Some(ArrayDimension::Dynamic { field_name }) => {
                // Use the field_name of the field supplying the dynamic array length
                Some(format_ident!("current_{}_count", field_name))
            }
            None => None,
        }
    }
    fn array_current_count_gte_expected_check(
        &self,
        array_field_name: &str,
        index: usize,
        use_parent: bool,
    ) -> Option<TokenStream> {
        self.array_current_count_vs_expected(array_field_name, index, use_parent)
            .map(
                |CountComparisonParts {
                     current_count,
                     expected_count,
                 }| quote!(#current_count >= #expected_count ),
            )
    }
    fn array_current_count_under_expected_check(
        &self,
        array_field_name: &str,
        index: usize,
        use_parent: bool,
    ) -> Option<TokenStream> {
        self.array_current_count_vs_expected(array_field_name, index, use_parent)
            .map(
                |CountComparisonParts {
                     current_count,
                     expected_count,
                 }| quote!(#current_count < #expected_count ),
            )
    }
    fn array_current_count_remainder_value(
        &self,
        array_field_name: &str,
        index: usize,
        use_parent: bool,
    ) -> Option<TokenStream> {
        self.array_current_count_vs_expected(array_field_name, index, use_parent)
            .map(
                |CountComparisonParts {
                     current_count,
                     expected_count,
                 }| quote!(#expected_count - #current_count),
            )
    }

    fn array_current_count_vs_expected(
        &self,
        array_field_name: &str,
        index: usize,
        use_parent: bool,
    ) -> Option<CountComparisonParts> {
        let current_count_ident = self.array_current_count_field_ident(array_field_name, index)?;
        let path_prefix = if use_parent {
            quote!(self.parent)
        } else {
            quote!(self)
        };
        match self.dimensions.get(index) {
            Some(ArrayDimension::Static { size }) => Some(CountComparisonParts {
                current_count: quote!(#path_prefix.#current_count_ident),
                expected_count: quote!(#size),
            }),
            Some(ArrayDimension::Dynamic { field_name }) => {
                let expected_count_ident = format_ident!("baggage_{}", field_name);
                Some(CountComparisonParts {
                    current_count: quote!(#path_prefix.#current_count_ident),
                    expected_count: quote!(#path_prefix.#expected_count_ident),
                })
            }
            None => None,
        }
    }
}

struct CountComparisonParts {
    current_count: TokenStream,
    expected_count: TokenStream,
}

/// The goal here is to make this current state implement an Iterator
/// which returns a number items equal to the previously-written size
/// of this array. The items produced by the iterator are single-shot
/// "ItemWriter" instances that exist to facilitate writing a single
/// value.
///
/// After the Iterator has been exhausted, the user is expected to
/// call `done` on this state instance to consume it and move on.
///
/// If the array is over bytes, provide alternatives to iterating
/// which allow direct slice operations.
fn emit_writer_field_state_transition_array(
    start_type: Ident,
    next_state: &CodecState,
    field_name: &str,
    at: &ArrayType,
) -> TokenStream {
    let current_count_ident = at
        .array_current_count_field_ident(field_name, 0)
        .expect("Arrays should have at least one dimension");
    let next_type = next_state.writer_ident();
    let next_dimensions_fields =
        BaggageDimension::field_initializations_from_self(&next_state.baggage_dimensions);
    let item_writer_struct_ident = format_ident!("{}_item", start_type);
    let write_item_method_ident = format_ident!("write");

    let item_writer_over_len_check = at
        .array_current_count_gte_expected_check(field_name, 0, true)
        .expect("Arrays should have at least one dimension");
    let pre_field_write = Some(quote! {
        if #item_writer_over_len_check {
            return Err(rust_lcm_codec::EncodeValueError::ArrayLengthMismatch(
                "array length mismatch discovered while iterating",
            ));
        }
    });
    let post_field_write = Some(quote! {
        self.parent.#current_count_ident += 1;
    });
    let write_item_method = match &*at.item_type {
        Type::Primitive(pt) => {
            let maybe_ref = if *pt == PrimitiveType::String {
                Some(quote!(&))
            } else {
                None
            };
            let rust_field_type = Some(format_ident!("{}", primitive_type_to_rust(&pt)));
            let write_invocation = emit_write_primitive_invocation(*pt, WriterPath::ViaSelfParent);
            quote! {
                #[inline]
                pub fn #write_item_method_ident(self, val: #maybe_ref #rust_field_type) -> Result<(), rust_lcm_codec::EncodeValueError<W::Error>> {
                    #pre_field_write
                    #write_invocation
                    #post_field_write
                    Ok(())
                }
            }
        }
        Type::Struct(st) => {
            let after_field_type = quote!(()); // unit
            let after_field_constructor = quote!(()); // unit instantiation looks like its typedef
            emit_write_struct_method(
                st,
                write_item_method_ident,
                pre_field_write,
                post_field_write,
                after_field_type,
                after_field_constructor,
                WriterPath::ViaSelfParent,
            )
        }
        Type::Array(at) => panic!("Multidimensional arrays are not supported yet."),
    };

    let current_iter_count_initialization =
        emit_next_field_current_iter_count_initialization(next_state);
    let top_level_under_len_check = at
        .array_current_count_under_expected_check(field_name, 0, false)
        .expect("Arrays should have at least one dimension");

    let maybe_slice_writer_methods = match &*at.item_type {
        Type::Primitive(PrimitiveType::Byte) => {
            let remainder_value = at.array_current_count_remainder_value(field_name, 0, false);
            let copy_field_from_slice_ident = format_ident!("copy_{}_from_slice", field_name);
            let get_field_as_mut_slice_ident = format_ident!("{}_as_mut_slice", field_name);
            Some(quote! {
            #[inline]
            pub fn #copy_field_from_slice_ident(self, val: &[u8]) -> Result<#next_type<'a, W>, rust_lcm_codec::EncodeValueError<W::Error>> {
                if #remainder_value != val.len() {
                    Err(rust_lcm_codec::EncodeValueError::ArrayLengthMismatch(
                        "slice provided to copy_FIELD_from_slice had a length which did not match the remaining expected size of the array",
                    ))
                } else {
                    self.writer.write_bytes(val)?;
                    Ok(#next_type {
                        writer: self.writer,
                        #current_iter_count_initialization
                        #( #next_dimensions_fields )*
                    })
                }
            }
            /// This method exposes the underlying writer's raw bytes for a region of size equal
            /// to the previously-written array length field value (minus any values already written
            /// via iteration).  This provides a mechanism
            /// for doing direct operations into byte blob style fields without extraneous copies,
            ///
            /// Since we don't know anything about the underlying writer's bytes preceding content,
            /// return the bytes with a type hint showing they may be uninitialized.
            /// In implementations where the writer's backing storage mechanism is understood by the
            /// user (e.g. backed by a previously initialized array buffer), it may be safe to
            /// transmute the slice to a plain byte slice.
            #[inline]
            pub fn #get_field_as_mut_slice_ident(self) -> Result<(&'a mut [core::mem::MaybeUninit<u8>], #next_type<'a, W>), rust_lcm_codec::EncodeValueError<W::Error>> {
                    // Use transmute to help link the generated bytes reference to the underlying Writer's lifetime
                    //
                    // Here we depend on the documented invariant of share_bytes wherein the Writer
                    // promises not to allow itself to mutate the shared bytes at any point in the future.
                    let shared_bytes = unsafe { core::mem::transmute(self.writer.share_bytes_mut(#remainder_value)?) };
                    Ok((shared_bytes,
                        #next_type {
                            writer: self.writer,
                            #current_iter_count_initialization
                            #( #next_dimensions_fields )*
                        }))
            }
            })
        }
        _ => None,
    };
    // TODO - create location-specific error message for array length mismatch
    quote! {

        impl<'a, W: rust_lcm_codec::StreamingWriter> Iterator for #start_type<'a, W> {
            type Item = #item_writer_struct_ident<'a, W>;
            fn next(&mut self) -> Option<Self::Item> {
                if #top_level_under_len_check {
                    // We cheat here to allow normally-evil multiple parent-mutable
                    // references because we know that the generated code in the
                    // child acts on the parent in a convergent manner:
                    // * Each child consumes itself when it exercises its only method,
                    //   and is thus limited to a single shot at mutating the parent.
                    // * The child mutation of the parent is gated on boundary checks in the parent
                    //   (max child operations and the underlying writer bounds checks)
                    unsafe {
                        Some(#item_writer_struct_ident {
                            parent: core::mem::transmute(self),
                        })
                    }
                } else {
                    None
                }
            }
        }
        impl<'a, W: rust_lcm_codec::StreamingWriter> #item_writer_struct_ident<'a, W> {
            #[inline]
            #write_item_method
        }
        impl<'a, W: rust_lcm_codec::StreamingWriter> #start_type<'a, W> {

            #maybe_slice_writer_methods

            #[inline]
            pub fn done(self) -> Result<#next_type<'a, W>, rust_lcm_codec::EncodeValueError<W::Error>> {
                if #top_level_under_len_check {
                    Err(rust_lcm_codec::EncodeValueError::ArrayLengthMismatch(
                        "array length mismatch discovered when `done` called",
                    ))
                } else {
                    Ok(#next_type {
                        writer: self.writer,
                        #current_iter_count_initialization
                        #( #next_dimensions_fields )*
                    })
                }
            }
        }
    }
}

impl parser::StructType {
    fn namespace_prefix(&self) -> Option<TokenStream> {
        if let Some(ns) = &self.namespace {
            let namespace_ident = format_ident!("{}", ns);
            Some(quote!(super::#namespace_ident::))
        } else {
            None
        }
    }
}

fn emit_reader_state_transition(
    rs: &CodecState,
    next_state: &CodecState,
    env: &Environment,
) -> TokenStream {
    match rs.field {
        Some((ref f, field_serves_as_dimension)) => {
            let start_type = rs.reader_ident();
            let next_type = next_state.reader_ident();
            let read_method_ident = format_ident!("read_{}", f.name);
            let next_dimensions_fields = BaggageDimension::field_initializations_from_self(
                next_state.baggage_dimensions.iter().filter(|d| {
                    !field_serves_as_dimension || d.len_field_name.as_str() != f.name.as_str()
                }),
            );
            let current_iter_count_initialization =
                emit_next_field_current_iter_count_initialization(next_state);
            match &f.ty {
                Type::Primitive(pt) => {
                    let rust_field_type = Some(format_ident!("{}", primitive_type_to_rust(&pt)));
                    let dimensional_capture = if field_serves_as_dimension {
                        let baggage_field_ident = format_ident!("baggage_{}", f.name);
                        Some(quote!(#baggage_field_ident: v as usize,))
                    } else {
                        None
                    };
                    let next_state = quote! {
                        #next_type {
                            reader: self.reader,
                            #dimensional_capture
                            #current_iter_count_initialization
                            #( #next_dimensions_fields )*
                        }
                    };
                    let read_methods = match pt {
                        PrimitiveType::String => quote! {
                            pub fn #read_method_ident(self) -> Result<(&'a #rust_field_type, #next_type<'a, R>), rust_lcm_codec::DecodeValueError<R::Error>> {
                                // Use transmute to link the generated string reference to the underlying Reader's lifetime
                                let v = unsafe { core::mem::transmute(rust_lcm_codec::read_str_value(self.reader)?) };
                                Ok((v, #next_state))
                            }
                        },
                        _ => {
                            let capture_binding = if dimensional_capture.is_some() {
                                Some(quote!(let v = *val;))
                            } else {
                                None
                            };
                            quote! {
                                pub fn #read_method_ident(self) -> Result<(#rust_field_type, #next_type<'a, R>), rust_lcm_codec::DecodeValueError<R::Error>> {
                                    let v = rust_lcm_codec::SerializeValue::read_value(self.reader)?;
                                    Ok((v, #next_state))
                                }
                            }
                        }
                    };

                    quote! {
                        impl<'a, R: rust_lcm_codec::StreamingReader> #start_type<'a, R> {

                            #[inline]
                            #read_methods
                        }
                    }
                }
                Type::Struct(st) => {
                    let field_struct_read_ready: Ident =
                        CodecState::reader_struct_state_decl_ident(&st.name, &StateName::Ready);
                    let field_struct_read_done: Ident =
                        CodecState::reader_struct_state_decl_ident(&st.name, &StateName::Done);
                    let struct_ns_prefix = st.namespace_prefix();
                    quote! {
                        impl<'a, R: rust_lcm_codec::StreamingReader> #start_type<'a, R> {

                            #[inline]
                            pub fn #read_method_ident<F>(self, f: F) -> Result<#next_type<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>>
                                where F: FnOnce(#struct_ns_prefix#field_struct_read_ready<'a, R>) -> Result<#struct_ns_prefix#field_struct_read_done<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>>
                            {
                                let ready = #struct_ns_prefix#field_struct_read_ready {
                                    reader: self.reader,
                                };
                                let done = f(ready)?;
                                Ok(#next_type {
                                    reader: done.reader,
                                    #current_iter_count_initialization
                                    #( #next_dimensions_fields )*
                                })
                            }
                        }
                    }
                }
                Type::Array(at) => {
                    let read_method_ident = format_ident!("read");
                    let current_iter_count_field_ident = at
                        .array_current_count_field_ident(f.name.as_str(), 0)
                        .expect("Arrays should have at least one dimension");
                    let item_reader_over_len_check = at
                        .array_current_count_gte_expected_check(f.name.as_str(), 0, true)
                        .expect("Arrays should have at least one dimension");
                    let pre_field_read = quote! {
                        if #item_reader_over_len_check {
                            return Err(rust_lcm_codec::DecodeValueError::ArrayLengthMismatch(
                                "array length mismatch discovered while iterating to read",
                            ));
                        }
                    };
                    let post_field_read = quote!(self.parent.#current_iter_count_field_ident += 1;);
                    let read_item_method = match &*at.item_type {
                        Type::Primitive(pt) => {
                            let rust_field_type = format_ident!("{}", primitive_type_to_rust(pt));
                            match pt {
                                PrimitiveType::String => quote! {
                                    pub fn #read_method_ident(self) -> Result<&'a #rust_field_type, rust_lcm_codec::DecodeValueError<R::Error>> {
                                        #pre_field_read
                                        // Use transmute to link the generated string reference to the underlying Reader's lifetime
                                        let v = unsafe { core::mem::transmute(rust_lcm_codec::read_str_value(self.parent.reader)?) };
                                        #post_field_read
                                        Ok(v)
                                    }
                                },
                                _ => quote! {
                                    pub fn #read_method_ident(self) -> Result<#rust_field_type, rust_lcm_codec::DecodeValueError<R::Error>> {
                                        #pre_field_read
                                        let v = rust_lcm_codec::SerializeValue::read_value(self.parent.reader)?;
                                        #post_field_read
                                        Ok(v)
                                    }
                                },
                            }
                        }

                        Type::Struct(st) => {
                            let struct_ns_prefix = st.namespace_prefix();
                            let field_struct_read_ready: Ident =
                                CodecState::reader_struct_state_decl_ident(
                                    &st.name,
                                    &StateName::Ready,
                                );
                            let field_struct_read_done: Ident =
                                CodecState::reader_struct_state_decl_ident(
                                    &st.name,
                                    &StateName::Done,
                                );
                            quote! {
                                pub fn #read_method_ident<F>(self, f: F) -> Result<(), rust_lcm_codec::DecodeValueError<R::Error>>
                                    where F: FnOnce(#struct_ns_prefix#field_struct_read_ready<'a, R>) -> Result<#struct_ns_prefix#field_struct_read_done<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>>
                                {
                                    #pre_field_read
                                    let ready = #struct_ns_prefix#field_struct_read_ready {
                                        reader: self.parent.reader,
                                    };
                                    let _done = f(ready)?;
                                    #post_field_read
                                    Ok(())
                                }
                            }
                        }
                        Type::Array(at) => panic!("Multidimensional arrays are not supported yet."),
                    };
                    let item_reader_struct_ident = format_ident!("{}_item", start_type);
                    let read_item_method_ident = format_ident!("read");
                    let top_level_under_len_check = at
                        .array_current_count_under_expected_check(f.name.as_str(), 0, false)
                        .expect("Arrays should have at least one dimension");
                    let maybe_slice_reader_method = match &*at.item_type {
                        Type::Primitive(PrimitiveType::Byte) => {
                            let field_name = f.name.as_str();
                            let remainder_value =
                                at.array_current_count_remainder_value(field_name, 0, false);
                            let get_field_as_slice_ident = format_ident!("{}_as_slice", field_name);
                            Some(quote! {
                            /// This method exposes the underlying reader's raw bytes for a region of size equal
                            /// to the previously-written array length field value (minus any values
                            /// previously read through iteration).  This provides a mechanism
                            /// for doing direct operations from byte blob style fields without extraneous copies,
                            #[inline]
                            pub fn #get_field_as_slice_ident(self) -> Result<(&'a [u8], #next_type<'a, R>), rust_lcm_codec::DecodeValueError<R::Error>> {
                                    // Use transmute to help link the generated bytes reference to the underlying Reader's lifetime
                                    //
                                    // Here we depend on the documented invariant of share_bytes_mut wherein the Reader
                                    // promises not to allow itself to mutate the shared bytes at any point in the future.
                                    let shared_bytes = unsafe { core::mem::transmute(self.reader.share_bytes(#remainder_value)?) };
                                    Ok((shared_bytes,
                                        #next_type {
                                            reader: self.reader,
                                            #current_iter_count_initialization
                                            #( #next_dimensions_fields )*
                                        }))
                            }
                            })
                        }
                        _ => None,
                    };
                    quote! {
                        impl<'a, R: rust_lcm_codec::StreamingReader> Iterator for #start_type<'a, R> {
                            type Item = #item_reader_struct_ident<'a, R>;
                            fn next(&mut self) -> Option<Self::Item> {
                                if #top_level_under_len_check {
                                    // We cheat here to allow normally-evil multiple parent-mutable
                                    // references because we know that the generated code in the
                                    // child acts on the parent in a convergent manner:
                                    // * Each child consumes itself when it exercises its only method,
                                    //   and is thus limited to a single shot at mutating the parent.
                                    // * The child mutation of the parent is gated on boundary checks in the parent
                                    //   (max child operations and the underlying reader bounds checks)
                                    unsafe {
                                        Some(#item_reader_struct_ident {
                                            parent: core::mem::transmute(self),
                                        })
                                    }
                                } else {
                                    None
                                }
                            }
                        }
                        impl<'a, R: rust_lcm_codec::StreamingReader> #item_reader_struct_ident<'a, R> {
                            #[inline]
                            #read_item_method
                        }
                        impl<'a, R: rust_lcm_codec::StreamingReader> #start_type<'a, R> {
                            #[inline]
                            pub fn done(self) -> Result<#next_type<'a, R>, rust_lcm_codec::DecodeValueError<R::Error>> {
                                if #top_level_under_len_check {
                                    Err(rust_lcm_codec::DecodeValueError::ArrayLengthMismatch(
                                        "array length mismatch discovered when read `done` called",
                                    ))
                                } else {
                                    Ok(#next_type {
                                        reader: self.reader,
                                        #current_iter_count_initialization
                                        #( #next_dimensions_fields )*
                                    })
                                }
                            }

                            #maybe_slice_reader_method
                        }
                    }
                }
            }
        }
        None => quote! {},
    }
}

/// Collection of a schema and its peers.
pub struct Environment {
    local_schema: parser::Schema,
    all_schemas: Vec<parser::Schema>,
}

impl Environment {
    /// Find a struct in the environment by it's StructType (name + ns)
    fn resolve_struct_type(&self, st: &parser::StructType) -> Option<&parser::Struct> {
        match &st.namespace {
            None => self
                .local_schema
                .structs
                .iter()
                .find(|curr_st| curr_st.name == st.name),
            Some(ns) => {
                for sch in self.all_schemas.iter() {
                    match &sch.package {
                        Some(this_ns) => {
                            if this_ns == ns {
                                for curr_st in sch.structs.iter() {
                                    if curr_st.name == st.name {
                                        return Some(curr_st);
                                    }
                                }
                            }
                        }
                        None => (),
                    }
                }
                None
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn manual_underscore_integer_check() {
        assert_eq!("0u64", format!("{}", to_underscored_literal(0)));
        assert_eq!("1u64", format!("{}", to_underscored_literal(1)));
        assert_eq!("10u64", format!("{}", to_underscored_literal(10)));
        assert_eq!("100u64", format!("{}", to_underscored_literal(100)));
        assert_eq!("1_000u64", format!("{}", to_underscored_literal(1_000)));
        assert_eq!("10_000u64", format!("{}", to_underscored_literal(10_000)));
        assert_eq!("100_000u64", format!("{}", to_underscored_literal(100_000)));
        assert_eq!(
            "1_000_000u64",
            format!("{}", to_underscored_literal(1_000_000))
        );
    }
}
