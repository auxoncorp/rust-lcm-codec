use nom::{
    branch::alt,
    bytes::complete::{tag, tag_no_case, take_while1},
    character::complete::{
        digit1, hex_digit1, multispace0, multispace1, oct_digit1, space0, space1,
    },
    combinator::{map, map_res, opt, recognize, value, verify},
    multi::{many0, separated_nonempty_list},
    sequence::{preceded, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum PrimitiveType {
    Int8,
    Int16,
    Int32,
    Int64,
    Float,
    Double,
    String,
    Boolean,
    Byte,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ArrayDimension {
    Static { size: u32 },
    Dynamic { field_name: String },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct ArrayType {
    pub item_type: Box<Type>,
    pub dimensions: Vec<ArrayDimension>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct StructType {
    pub namespace: Option<String>,
    pub name: String,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Type {
    Primitive(PrimitiveType),
    Array(ArrayType),
    Struct(StructType),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum ConstValue {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    // Keep floats and doubles as strings, since f32/f64 don't have Eq defined on them
    Float(String),
    Double(String),
    Boolean(bool),
    Byte(u8),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Field {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Const {
    pub name: String,
    pub ty: PrimitiveType,
    pub value: ConstValue,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum StructMember {
    Field(Field),
    Const(Const),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Struct {
    pub name: String,
    pub members: Vec<StructMember>,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Schema {
    pub package: Option<String>,
    pub structs: Vec<Struct>,
}

/// Match a comma, optionally surrounded by spaces
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::spaced_comma;
///
/// assert_eq!(spaced_comma(","), Ok(("", ",")));
/// assert_eq!(spaced_comma(" ,\t"), Ok(("", " ,\t")));
/// assert_eq!(spaced_comma("x"), Err(Err::Error(("x", ErrorKind::Tag))));
/// ```
pub fn spaced_comma(input: &str) -> IResult<&str, &str> {
    recognize(tuple((space0, tag(","), space0)))(input)
}

/// Names that can be used for structs, packages, or fields
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::ident;
///
/// assert_eq!(ident("foo"), Ok(("", "foo")));
/// assert_eq!(ident("foo_bar"), Ok(("", "foo_bar")));
/// assert_eq!(ident(""), Err(Err::Error(("", ErrorKind::TakeWhile1))));
/// ```
pub fn ident(input: &str) -> IResult<&str, &str> {
    recognize(take_while1(|c: char| c.is_alphanumeric() || c == '_'))(input)
}

/// Parse an LCM primitive type
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{primitive_type, PrimitiveType};
///
/// assert_eq!(primitive_type("int8_t"), Ok(("", PrimitiveType::Int8)));
/// assert_eq!(primitive_type("int16_t"), Ok(("", PrimitiveType::Int16)));
/// assert_eq!(primitive_type("int32_t"), Ok(("", PrimitiveType::Int32)));
/// assert_eq!(primitive_type("int64_t"), Ok(("", PrimitiveType::Int64)));
/// assert_eq!(primitive_type("float"), Ok(("", PrimitiveType::Float)));
/// assert_eq!(primitive_type("double"), Ok(("", PrimitiveType::Double)));
/// assert_eq!(primitive_type("string"), Ok(("", PrimitiveType::String)));
/// assert_eq!(primitive_type("boolean"), Ok(("", PrimitiveType::Boolean)));
/// assert_eq!(primitive_type("byte"), Ok(("", PrimitiveType::Byte)));
///
/// assert_eq!(primitive_type(""), Err(Err::Error(("", ErrorKind::Tag))));
/// assert_eq!(primitive_type("foo"), Err(Err::Error(("foo", ErrorKind::Tag))));
/// ```
pub fn primitive_type(input: &str) -> IResult<&str, PrimitiveType> {
    alt((
        map(tag("int8_t"), |_| PrimitiveType::Int8),
        map(tag("int16_t"), |_| PrimitiveType::Int16),
        map(tag("int32_t"), |_| PrimitiveType::Int32),
        map(tag("int64_t"), |_| PrimitiveType::Int64),
        map(tag("float"), |_| PrimitiveType::Float),
        map(tag("double"), |_| PrimitiveType::Double),
        map(tag("string"), |_| PrimitiveType::String),
        map(tag("boolean"), |_| PrimitiveType::Boolean),
        map(tag("byte"), |_| PrimitiveType::Byte),
    ))(input)
}

/// Parse the type part of a field decl.
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{field_type, Type, PrimitiveType, StructType};
///
/// assert_eq!(field_type("int8_t"), Ok(("", Type::Primitive(PrimitiveType::Int8))));
///
/// assert_eq!(field_type("foo.bar"),
///            Ok(("", Type::Struct(StructType { namespace: Some("foo".to_string()), name: "bar".to_string() }))));
///
/// assert_eq!(field_type("foo"),
///            Ok(("", Type::Struct(StructType { namespace: None, name: "foo".to_string() }))));
///
/// ```
pub fn field_type(input: &str) -> IResult<&str, Type> {
    alt((
        map(primitive_type, Type::Primitive),
        map(separated_pair(ident, tag("."), ident), |(ns, n)| {
            Type::Struct(StructType {
                namespace: Some(ns.to_string()),
                name: n.to_string(),
            })
        }),
        map(ident, |n| {
            Type::Struct(StructType {
                namespace: None,
                name: n.to_string(),
            })
        }),
    ))(input)
}

fn array_dimension(input: &str) -> IResult<&str, ArrayDimension> {
    preceded(
        tag("["),
        terminated(
            alt((
                map(map_res(digit1, |s: &str| s.parse::<u32>()), |size| {
                    ArrayDimension::Static { size }
                }),
                map(ident, |s| ArrayDimension::Dynamic {
                    field_name: s.to_string(),
                }),
            )),
            tag("]"),
        ),
    )(input)
}

/// Parse a field declaration, inside a struct, including array dimensions.
/// (doesn't handle the semicolon or preceding whitespace)
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{field_decl, Field, PrimitiveType, Type, StructType, ArrayDimension, ArrayType};
///
/// assert_eq!(
///     field_decl("int8_t foo"),
///     Ok((
///         "",
///         Field {
///             name: "foo".to_owned(),
///             ty: Type::Primitive(PrimitiveType::Int8)
///         }
///     ))
/// );
///
/// assert_eq!(
///     field_decl("ns.name foo[dim][2]"),
///     Ok((
///         "",
///         Field {
///           name: "foo".to_owned(),
///           ty: Type::Array(ArrayType {
///             item_type: Box::new(
///               Type::Struct(StructType { namespace: Some("ns".to_string()),
///                                         name: "name".to_string() })
///             ),
///             dimensions: vec![
///               ArrayDimension::Dynamic { field_name: "dim".to_string() },
///               ArrayDimension::Static { size: 2 },
///             ]
///           })
///         }
///     ))
/// );
///
/// assert_eq!(field_decl(""), Err(Err::Error(("", ErrorKind::TakeWhile1))));
/// assert_eq!(field_decl("int8_t *!@"), Err(Err::Error(("*!@", ErrorKind::TakeWhile1))));
///
/// ```
pub fn field_decl(input: &str) -> IResult<&str, Field> {
    let (input, mut ty) = field_type(input)?;
    let (input, _) = space1(input)?;
    let (input, name) = ident(input)?;
    let (input, dims) = many0(array_dimension)(input)?;

    if dims.len() > 0 {
        ty = Type::Array(ArrayType {
            item_type: Box::new(ty),
            dimensions: dims,
        });
    }

    Ok((
        input,
        Field {
            ty,
            name: name.to_owned(),
        },
    ))
}

/// Recognize something that looks like an integer, positive or negative. Return a tuple of the
/// content part of the string and the radix.
fn recognize_int(input: &str) -> IResult<&str, (String, u32)> {
    let (input, minus) = opt(tag("-"))(input)?;
    let minus = minus.unwrap_or("");

    let (input, radix) = alt((
        value(16, tag_no_case("0x")),
        value(8, tag("0")),
        value(10, tag("")),
    ))(input)?;

    let (input, body) = match radix {
        16 => hex_digit1(input)?,
        10 => digit1(input)?,
        8 => oct_digit1(input)?,
        _ => unreachable!(),
    };

    Ok((input, (format!("{}{}", minus, body), radix)))
}

/// Recognize something that looks like an float, positive or negative. Keep it a string.
fn recognize_float(input: &str) -> IResult<&str, &str> {
    recognize(tuple((
        opt(tag("-")),
        digit1,
        opt(tuple((tag("."), digit1, opt(tuple((tag("e"), digit1)))))),
    )))(input)
}

/// Parse a const value of the given type.
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{const_value, ConstValue, PrimitiveType};
///
/// assert_eq!(const_value(PrimitiveType::Int8,    "42"), Ok(("", ConstValue::Int8(   42))));
/// assert_eq!(const_value(PrimitiveType::Int8,   "-42"), Ok(("", ConstValue::Int8(  -42))));
/// assert_eq!(const_value(PrimitiveType::Int8,  "0x2f"), Ok(("", ConstValue::Int8( 0x2f))));
/// assert_eq!(const_value(PrimitiveType::Int8, "-0x2f"), Ok(("", ConstValue::Int8(-0x2f))));
/// assert_eq!(const_value(PrimitiveType::Int8,   "022"), Ok(("", ConstValue::Int8( 0o22))));
/// assert_eq!(const_value(PrimitiveType::Int8,  "-022"), Ok(("", ConstValue::Int8(-0o22))));
/// assert_eq!(const_value(PrimitiveType::Int8,  "1024"),
///            Err(Err::Error(("1024", ErrorKind::MapRes))));
///
/// assert_eq!(const_value(PrimitiveType::Int16,  "1024"), Ok(("", ConstValue::Int16( 1024))));
/// assert_eq!(const_value(PrimitiveType::Int16, "-1024"), Ok(("", ConstValue::Int16(-1024))));
/// assert_eq!(const_value(PrimitiveType::Int16, "32768"),
///            Err(Err::Error(("32768", ErrorKind::MapRes))));
///
/// assert_eq!(const_value(PrimitiveType::Int32,  "32768"), Ok(("", ConstValue::Int32( 32768))));
/// assert_eq!(const_value(PrimitiveType::Int32, "-32768"), Ok(("", ConstValue::Int32(-32768))));
/// assert_eq!(const_value(PrimitiveType::Int32, "2147483648"),
///            Err(Err::Error(("2147483648", ErrorKind::MapRes))));
///
/// assert_eq!(const_value(PrimitiveType::Int64,  "2147483648"), Ok(("", ConstValue::Int64( 2147483648))));
/// assert_eq!(const_value(PrimitiveType::Int64, "-2147483648"), Ok(("", ConstValue::Int64(-2147483648))));
/// assert_eq!(const_value(PrimitiveType::Int64, "92233720368547758073"),
///            Err(Err::Error(("92233720368547758073", ErrorKind::MapRes))));
///
/// assert_eq!(const_value(PrimitiveType::Float,           "10"), Ok(("", ConstValue::Float(         "10".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Float,        "10.35"), Ok(("", ConstValue::Float(      "10.35".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Float,       "-10.35"), Ok(("", ConstValue::Float(     "-10.35".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Float,     "10.35e12"), Ok(("", ConstValue::Float(   "10.35e12".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Float,    "-10.35e12"), Ok(("", ConstValue::Float(  "-10.35e12".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Float,  "10.35e12000"), Ok(("", ConstValue::Float("10.35e12000".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Float, "asdf"),
///            Err(Err::Error(("asdf", ErrorKind::Digit))));
///
/// assert_eq!(const_value(PrimitiveType::Double,           "10"), Ok(("", ConstValue::Double(         "10".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Double,        "10.35"), Ok(("", ConstValue::Double(      "10.35".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Double,       "-10.35"), Ok(("", ConstValue::Double(     "-10.35".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Double,     "10.35e12"), Ok(("", ConstValue::Double(   "10.35e12".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Double,    "-10.35e12"), Ok(("", ConstValue::Double(  "-10.35e12".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Double,  "10.35e12000"), Ok(("", ConstValue::Double("10.35e12000".to_owned()))));
/// assert_eq!(const_value(PrimitiveType::Double, "asdf"),
///            Err(Err::Error(("asdf", ErrorKind::Digit))));
///
/// assert_eq!(const_value(PrimitiveType::Byte,   "42"), Ok(("", ConstValue::Byte( 42))));
/// assert_eq!(const_value(PrimitiveType::Byte,  "-42"), Err(Err::Error(("-42", ErrorKind::Digit))));
/// assert_eq!(const_value(PrimitiveType::Byte, "1024"), Err(Err::Error(("1024", ErrorKind::MapRes))));
///
/// ```
pub fn const_value(ty: PrimitiveType, input: &str) -> IResult<&str, ConstValue> {
    match ty {
        PrimitiveType::Int8 => map(
            map_res(recognize_int, |(s, radix)| i8::from_str_radix(&s, radix)),
            ConstValue::Int8,
        )(input),
        PrimitiveType::Int16 => map(
            map_res(recognize_int, |(s, radix)| i16::from_str_radix(&s, radix)),
            ConstValue::Int16,
        )(input),
        PrimitiveType::Int32 => map(
            map_res(recognize_int, |(s, radix)| i32::from_str_radix(&s, radix)),
            ConstValue::Int32,
        )(input),
        PrimitiveType::Int64 => map(
            map_res(recognize_int, |(s, radix)| i64::from_str_radix(&s, radix)),
            ConstValue::Int64,
        )(input),
        PrimitiveType::Float => map(
            verify(recognize_float, |s: &str| s.parse::<f32>().is_ok()),
            |s: &str| ConstValue::Float(s.to_owned()),
        )(input),
        PrimitiveType::Double => map(
            verify(recognize_float, |s: &str| s.parse::<f64>().is_ok()),
            |s: &str| ConstValue::Double(s.to_owned()),
        )(input),
        PrimitiveType::String => panic!("String constants are not supported"),
        PrimitiveType::Boolean => panic!("Boolean constants are not supported"),
        PrimitiveType::Byte => {
            map(map_res(digit1, |s: &str| s.parse::<u8>()), ConstValue::Byte)(input)
        }
    }
}

fn const_name_val(ty: PrimitiveType) -> impl Fn(&str) -> IResult<&str, (&str, ConstValue)> {
    move |input: &str| {
        let (input, name) = ident(input)?;
        let (input, _) = tuple((space0, tag("="), space0))(input)?;
        let (input, value) = const_value(ty, input)?;

        Ok((input, (name, value)))
    }
}

/// Parse a const declaration, inside a struct. (doesn't handle the semicolon or
/// preceding whitespace)
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{const_decl, Const, ConstValue, PrimitiveType};
///
/// assert_eq!(
///     const_decl("const int32_t YELLOW=1, GOLDENROD=2, CANARY=3"),
///     Ok((
///         "",
///         vec![
///           Const {
///             name: "YELLOW".to_owned(),
///             ty: PrimitiveType::Int32,
///             value: ConstValue::Int32(1)
///           },
///           Const {
///             name: "GOLDENROD".to_owned(),
///             ty: PrimitiveType::Int32,
///             value: ConstValue::Int32(2)
///           },
///           Const {
///             name: "CANARY".to_owned(),
///             ty: PrimitiveType::Int32,
///             value: ConstValue::Int32(3)
///           },
///         ]
///     ))
/// );
///
/// assert_eq!(
///     const_decl("const double PI=3.14159"),
///     Ok((
///         "",
///         vec![Const {
///             name: "PI".to_owned(),
///             ty: PrimitiveType::Double,
///             value: ConstValue::Double("3.14159".to_owned())
///         }]
///     ))
/// );
///
///
/// ```
pub fn const_decl(input: &str) -> IResult<&str, Vec<Const>> {
    let (input, _) = tuple((tag("const"), space1))(input)?;
    let (input, ty) = primitive_type(input)?;
    let (input, _) = space1(input)?;
    let (input, name_vals) = separated_nonempty_list(spaced_comma, const_name_val(ty))(input)?;

    Ok((
        input,
        name_vals
            .into_iter()
            .map(|(name, value)| Const {
                name: name.to_string(),
                value,
                ty,
            })
            .collect(),
    ))
}

pub fn struct_member(input: &str) -> IResult<&str, Vec<StructMember>> {
    alt((
        map(const_decl, |cds| {
            cds.into_iter().map(StructMember::Const).collect()
        }),
        map(field_decl, |fd| vec![StructMember::Field(fd)]),
    ))(input)
}

/// Parse a whole struct declaration.
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{struct_decl, Struct, Const, ConstValue, PrimitiveType, StructMember, Field, Type};
///
/// assert_eq!(
///     struct_decl("struct empty_struct { }"),
///     Ok((
///         "",
///         Struct {
///           name: "empty_struct".to_string(),
///           members: vec![],
///         }
///     ))
/// );
///
/// assert_eq!(
///     struct_decl("struct my_struct {\n  const int32_t YELLOW=1;\n  int32_t color;\n}"),
///     Ok((
///         "",
///         Struct {
///           name: "my_struct".to_string(),
///           members: vec![
///             StructMember::Const(
///               Const {
///                 name: "YELLOW".to_owned(),
///                 ty: PrimitiveType::Int32,
///                 value: ConstValue::Int32(1)
///               }
///             ),
///             StructMember::Field(
///               Field {
///                 name: "color".to_owned(),
///                 ty: Type::Primitive(PrimitiveType::Int32),
///               }
///             ),
///           ]
///         }
///     ))
/// );
/// ```
pub fn struct_decl(input: &str) -> IResult<&str, Struct> {
    let (input, _) = tuple((tag("struct"), space1))(input)?;
    let (input, name) = ident(input)?;
    let (input, _) = tuple((multispace1, tag("{"), multispace1))(input)?;

    let (input, member_vecs) = many0(terminated(
        struct_member,
        tuple((space0, tag(";"), multispace0)),
    ))(input)?;

    let (input, _) = tag("}")(input)?;

    let mut members = vec![];
    for mv in member_vecs.into_iter() {
        members.extend(mv);
    }

    Ok((
        input,
        Struct {
            name: name.to_owned(),
            members,
        },
    ))
}

/// Parse a package line, not including the semicolon.
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::package_decl;
///
/// assert_eq!(
///     package_decl("package my_package"),
///     Ok((
///         "",
///         "my_package".to_string(),
///     ))
/// );
/// ```
pub fn package_decl(input: &str) -> IResult<&str, String> {
    map(
        preceded(tuple((tag("package"), space1)), ident),
        |name: &str| name.to_string(),
    )(input)
}

/// Parse an entire schema file
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{schema, Schema, Struct, Const, ConstValue, PrimitiveType, StructMember, Field};
///
/// assert_eq!(
///     schema("package test;\n\nstruct empty { }\nstruct empty2 { }"),
///     Ok((
///         "",
///         Schema {
///           package: Some("test".to_string()),
///           structs: vec![
///             Struct {
///               name: "empty".to_string(),
///               members: vec![],
///             },
///             Struct {
///               name: "empty2".to_string(),
///               members: vec![],
///             }
///           ]
///         },
///     ))
/// );
///
///  assert_eq!(
///     schema("struct empty { }"),
///     Ok((
///         "",
///         Schema {
///           package: None,
///           structs: vec![
///             Struct {
///               name: "empty".to_string(),
///               members: vec![],
///             }
///           ]
///         },
///     ))
/// );
/// ```
pub fn schema(input: &str) -> IResult<&str, Schema> {
    let (input, _) = multispace0(input)?;
    let (input, package) = opt(terminated(
        package_decl,
        tuple((space0, tag(";"), multispace0)),
    ))(input)?;

    let (input, structs) = many0(terminated(struct_decl, multispace0))(input)?;

    Ok((input, Schema { package, structs }))
}
