#![allow(dead_code)]
#![allow(unused_variables)]
#![allow(unused_imports)]

use crate::parser::{self, Schema};

// Hashing operations

const INITIAL_STRUCT_HASH: i64 = 0x12345678i64;

fn hash_update(v: i64, c: i8) -> i64 {
    ((v << 8) ^ (v >> 55)).wrapping_add(c as i64)
}

fn hash_string_update(mut v: i64, s: &str) -> i64 {
    v = hash_update(v, s.len() as i8);
    for c in s.bytes() {
        v = hash_update(v, c as i8);
    }

    v
}

fn primitive_name_for_hash(p: parser::PrimitiveType) -> &'static str {
    match p {
        parser::PrimitiveType::Int8 => "int8_t",
        parser::PrimitiveType::Int16 => "int16_t",
        parser::PrimitiveType::Int32 => "int32_t",
        parser::PrimitiveType::Int64 => "int64_t",
        parser::PrimitiveType::Float => "float",
        parser::PrimitiveType::Double => "double",
        parser::PrimitiveType::String => "string",
        parser::PrimitiveType::Boolean => "boolean",
        parser::PrimitiveType::Byte => "byte",
    }
}

fn dimension_id_for_hash(d: &parser::ArrayDimension) -> i8 {
    match d {
        parser::ArrayDimension::Static { .. } => 0,
        parser::ArrayDimension::Dynamic { .. } => 1,
    }
}

fn dimension_name_for_hash(d: &parser::ArrayDimension) -> String {
    match d {
        parser::ArrayDimension::Static { size } => format!("{}", size).to_owned(),
        parser::ArrayDimension::Dynamic { field_name } => field_name.to_owned(),
    }
}

fn struct_base_hash(s: &parser::Struct) -> u64 {
    let mut v = INITIAL_STRUCT_HASH;

    for member in s.members.iter() {
        match member {
            parser::StructMember::Const(_) => (),
            parser::StructMember::Field(f) => {
                v = hash_string_update(v, &f.name);
                match &f.ty {
                    parser::Type::Primitive(p) => {
                        v = hash_string_update(v, primitive_name_for_hash(*p));
                        v = hash_update(v, 0); // 0 array dimensions
                    }
                    parser::Type::Array(a) => {
                        if let parser::Type::Primitive(p) = *a.item_type {
                            v = hash_string_update(v, primitive_name_for_hash(p));
                        }
                        v = hash_update(v, a.dimensions.len() as i8);
                        for dim in a.dimensions.iter() {
                            v = hash_update(v, dimension_id_for_hash(dim));
                            v = hash_string_update(v, &dimension_name_for_hash(dim));
                        }
                    }
                    parser::Type::Struct(_) => (),
                }
            }
        }
    }

    // This implicitly happens via the code generator in the C impl.
    v as u64
}

fn struct_child_struct_types<'a>(
    s: &parser::Struct,
) -> impl Iterator<Item = parser::StructType> + '_ {
    s.members.iter().flat_map(|mem| match mem {
        parser::StructMember::Const(_) => None,
        parser::StructMember::Field(f) => match &f.ty {
            parser::Type::Struct(st) => Some(st.clone()),
            parser::Type::Array(a) => match &*a.item_type {
                parser::Type::Struct(st) => Some(st.clone()),
                _ => None,
            },
            _ => None,
        },
    })
}

pub struct Environment {
    pub local_schema: parser::Schema,
    pub all_schemas: Vec<parser::Schema>,
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
                        None => ()
                    }
                }
                return None;
            }
        }
    }
}

fn struct_hash_internal(s: &parser::Struct, env: &Environment, mut stack: &mut Vec<u64>) -> u64 {
    let mut v = struct_base_hash(s);
    if stack.contains(&v) {
        return 0;
    }
    stack.push(v);

    for st in struct_child_struct_types(s) {
        let resolved_struct = env
            .resolve_struct_type(&st)
            .expect(&format!("Can't resolve struct type {:?}", st));
        v = v.wrapping_add(struct_hash_internal(&resolved_struct, env, &mut stack));
    }
    stack.pop();
    v = v.rotate_left(1);
    v
}

/// Get the fingerprint for struct `s`, in the context of typing environment `env`.
pub fn struct_hash(s: &parser::Struct, env: &Environment) -> u64 {
    let mut visited = vec![];
    struct_hash_internal(s, env, &mut visited)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hash_update() {
        assert_eq!(hash_update(INITIAL_STRUCT_HASH, 0), 0x1234567800);
        assert_eq!(hash_update(INITIAL_STRUCT_HASH, 42), 0x123456782a);
        assert_eq!(hash_update(INITIAL_STRUCT_HASH, -42), 0x12345677d6);
    }

    #[test]
    fn test_hash_string_update() {
        assert_eq!(hash_string_update(INITIAL_STRUCT_HASH, ""), 0x1234567800);
        assert_eq!(hash_string_update(INITIAL_STRUCT_HASH, "a"), 0x123456780161);
        assert_eq!(
            hash_string_update(INITIAL_STRUCT_HASH, "test"),
            0x3456780474657398
        );

        assert_eq!(
            hash_string_update(INITIAL_STRUCT_HASH, "テスト"),
            0x7d79f9159a2d6b4d
        );

        // 256 characters
        assert_eq!(hash_string_update(INITIAL_STRUCT_HASH,
                                      "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"),
                   0x33dc2d5adeb3ed47);
    }

    #[test]
    fn test_recursive_struct_hash() {
        let schema_text = "
package rec_test;
struct node {
  int32_t count;
  node children[count];
}";
        let (_, schema) = parser::schema(schema_text).unwrap();

        let s = &schema.structs[0].clone();
        assert_eq!(struct_base_hash(&s), 0xAC4115EBB89101A9);

        let env = Environment {
            local_schema: schema.clone(),
            all_schemas: vec![schema],
        };

        // TODO check this against the reference impl
        assert_eq!(struct_hash(&s, &env), 0xAC4115EBB89101A9u64.rotate_left(1));
    }

    #[test]
    fn test_cross_struct_hash() {
        let schema_text_a = "
package cross_test;
struct a {
  int32_t foo;
  int16_t bar;
}";
        let schema_text_b = "
package cross_test;
struct b {
  int32_t baz;
  cross_test.a cross;
}";
        let (_, schema_a) = parser::schema(schema_text_a).unwrap();
        let (_, schema_b) = parser::schema(schema_text_b).unwrap();
        let all_schemas = vec![schema_a.clone(), schema_b.clone()];

        let env = Environment {
            local_schema: schema_b.clone(),
            all_schemas
        };

        let b = &schema_b.structs[0];

        // TODO check this against the reference impl
        assert_eq!(struct_hash(&b, &env), 0x1C6222CC3AFC3285);
    }
}
