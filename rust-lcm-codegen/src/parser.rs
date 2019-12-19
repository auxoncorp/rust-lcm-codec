#![allow(dead_code)]
#![allow(unused_imports)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{alpha1, digit0, digit1, multispace0, multispace1, space0, space1},
    character::is_alphanumeric,
    combinator::{cut, flat_map, map, map_res, opt, recognize, value, verify},
    do_parse,
    error::ParseError,
    multi::separated_nonempty_list,
    named,
    sequence::{delimited, separated_pair, terminated, tuple},
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

#[derive(Debug, Eq, PartialEq)]
pub enum ConstValue {
    Int8(i8),
    Int16(i16),
    Int32(i32),
    Int64(i64),
    // Keep floats and doubles as strings, since we f32/f64 don't have Eq defined on them
    Float(String),
    Double(String),
    Boolean(bool),
    Byte(u8),
}

#[derive(Debug, Eq, PartialEq)]
pub struct FieldDecl {
    pub name: String,
    pub ty: PrimitiveType,
}

#[derive(Debug, Eq, PartialEq)]
pub struct ConstDecl {
    pub name: String,
    pub ty: PrimitiveType,
    pub value: ConstValue,
}

#[derive(Debug, Eq, PartialEq)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<FieldDecl>,
    pub constnats: Vec<ConstDecl>,
}

/// Match a comma, optionally surrounded by spaces
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::spaced_comma;
/// let parser = spaced_comma;
///
/// assert_eq!(parser(","), Ok(("", ",")));
/// assert_eq!(parser(" ,\t"), Ok(("", " ,\t")));
/// assert_eq!(parser("x"), Err(Err::Error(("x", ErrorKind::Tag))));
/// ```
pub fn spaced_comma(input: &str) -> IResult<&str, &str> {
    recognize(tuple((space0, tag(","), space0)))(input)
}

/// Parse an LCM primitive type
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{primitive_type, PrimitiveType};
/// let parser = primitive_type;
///
/// assert_eq!(parser("int8_t"), Ok(("", PrimitiveType::Int8)));
/// assert_eq!(parser("int16_t"), Ok(("", PrimitiveType::Int16)));
/// assert_eq!(parser("int32_t"), Ok(("", PrimitiveType::Int32)));
/// assert_eq!(parser("int64_t"), Ok(("", PrimitiveType::Int64)));
/// assert_eq!(parser("float"), Ok(("", PrimitiveType::Float)));
/// assert_eq!(parser("double"), Ok(("", PrimitiveType::Double)));
/// assert_eq!(parser("string"), Ok(("", PrimitiveType::String)));
/// assert_eq!(parser("boolean"), Ok(("", PrimitiveType::Boolean)));
/// assert_eq!(parser("byte"), Ok(("", PrimitiveType::Byte)));
///
/// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
/// assert_eq!(parser("foo"), Err(Err::Error(("foo", ErrorKind::Tag))));
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

/// Parse an LCM field name
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// let parser = rust_lcm_codegen::parser::field_name;
///
/// assert_eq!(parser("foo"), Ok(("", "foo")));
/// assert_eq!(parser("foo_bar"), Ok(("", "foo_bar")));
/// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::TakeWhile1))));
/// ```
pub fn field_name(input: &str) -> IResult<&str, &str> {
    recognize(take_while1(|c: char| c.is_alphanumeric() || c == '_'))(input)
}

/// Parse a field declaration, inside a struct. (doesn't handle the semicolon or
/// preceding whitespace)
///
/// ```
/// # use nom::{Err, error::ErrorKind, Needed};
/// use rust_lcm_codegen::parser::{field_decl, FieldDecl, PrimitiveType};
/// let parser = field_decl;
///
/// assert_eq!(
///     parser("int8_t foo"),
///     Ok((
///         "",
///         FieldDecl {
///             name: "foo".to_owned(),
///             ty: PrimitiveType::Int8
///         }
///     ))
/// );
///
/// assert_eq!(parser(""), Err(Err::Error(("", ErrorKind::Tag))));
/// assert_eq!(parser("int8_t *!@"), Err(Err::Error(("*!@", ErrorKind::TakeWhile1))));
///
/// ```
pub fn field_decl(input: &str) -> IResult<&str, FieldDecl> {
    map(
        separated_pair(primitive_type, space1, field_name),
        |(ty, name)| FieldDecl {
            ty,
            name: name.to_owned(),
        },
    )(input)
}

/// Recognize something that looks like an integer, positive or negative. Keep it a string.
fn recognize_int(input: &str) -> IResult<&str, &str> {
    recognize(tuple((opt(tag("-")), digit1)))(input)
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
/// let parser = const_value;
///
/// assert_eq!(parser(PrimitiveType::Int8,  "42"), Ok(("", ConstValue::Int8( 42))));
/// assert_eq!(parser(PrimitiveType::Int8, "-42"), Ok(("", ConstValue::Int8(-42))));
/// assert_eq!(parser(PrimitiveType::Int8, "1024"),
///            Err(Err::Error(("1024", ErrorKind::MapRes))));
///
/// assert_eq!(parser(PrimitiveType::Int16,  "1024"), Ok(("", ConstValue::Int16( 1024))));
/// assert_eq!(parser(PrimitiveType::Int16, "-1024"), Ok(("", ConstValue::Int16(-1024))));
/// assert_eq!(parser(PrimitiveType::Int16, "32768"),
///            Err(Err::Error(("32768", ErrorKind::MapRes))));
///
/// assert_eq!(parser(PrimitiveType::Int32,  "32768"), Ok(("", ConstValue::Int32( 32768))));
/// assert_eq!(parser(PrimitiveType::Int32, "-32768"), Ok(("", ConstValue::Int32(-32768))));
/// assert_eq!(parser(PrimitiveType::Int32, "2147483648"),
///            Err(Err::Error(("2147483648", ErrorKind::MapRes))));
///
/// assert_eq!(parser(PrimitiveType::Int64,  "2147483648"), Ok(("", ConstValue::Int64( 2147483648))));
/// assert_eq!(parser(PrimitiveType::Int64, "-2147483648"), Ok(("", ConstValue::Int64(-2147483648))));
/// assert_eq!(parser(PrimitiveType::Int64, "92233720368547758073"),
///            Err(Err::Error(("92233720368547758073", ErrorKind::MapRes))));
///
/// assert_eq!(parser(PrimitiveType::Float,           "10"), Ok(("", ConstValue::Float(         "10".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Float,        "10.35"), Ok(("", ConstValue::Float(      "10.35".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Float,       "-10.35"), Ok(("", ConstValue::Float(     "-10.35".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Float,     "10.35e12"), Ok(("", ConstValue::Float(   "10.35e12".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Float,    "-10.35e12"), Ok(("", ConstValue::Float(  "-10.35e12".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Float,  "10.35e12000"), Ok(("", ConstValue::Float("10.35e12000".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Float, "asdf"),
///            Err(Err::Error(("asdf", ErrorKind::Digit))));
///
/// assert_eq!(parser(PrimitiveType::Double,           "10"), Ok(("", ConstValue::Double(         "10".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Double,        "10.35"), Ok(("", ConstValue::Double(      "10.35".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Double,       "-10.35"), Ok(("", ConstValue::Double(     "-10.35".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Double,     "10.35e12"), Ok(("", ConstValue::Double(   "10.35e12".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Double,    "-10.35e12"), Ok(("", ConstValue::Double(  "-10.35e12".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Double,  "10.35e12000"), Ok(("", ConstValue::Double("10.35e12000".to_owned()))));
/// assert_eq!(parser(PrimitiveType::Double, "asdf"),
///            Err(Err::Error(("asdf", ErrorKind::Digit))));
///
/// assert_eq!(parser(PrimitiveType::Boolean, "true"), Ok(("", ConstValue::Boolean(true))));
/// assert_eq!(parser(PrimitiveType::Boolean, "false"), Ok(("", ConstValue::Boolean(false))));
/// assert_eq!(parser(PrimitiveType::Boolean, "bogus"),
///            Err(Err::Error(("bogus", ErrorKind::Tag))));
///
/// assert_eq!(parser(PrimitiveType::Byte,   "42"), Ok(("", ConstValue::Byte( 42))));
/// assert_eq!(parser(PrimitiveType::Byte,  "-42"), Err(Err::Error(("-42", ErrorKind::Digit))));
/// assert_eq!(parser(PrimitiveType::Byte, "1024"), Err(Err::Error(("1024", ErrorKind::MapRes))));
///
/// ```
pub fn const_value(ty: PrimitiveType, input: &str) -> IResult<&str, ConstValue> {
    match ty {
        PrimitiveType::Int8 => map(
            map_res(recognize_int, |s: &str| s.parse::<i8>()),
            ConstValue::Int8,
        )(input),
        PrimitiveType::Int16 => map(
            map_res(recognize_int, |s: &str| s.parse::<i16>()),
            ConstValue::Int16,
        )(input),
        PrimitiveType::Int32 => map(
            map_res(recognize_int, |s: &str| s.parse::<i32>()),
            ConstValue::Int32,
        )(input),
        PrimitiveType::Int64 => map(
            map_res(recognize_int, |s: &str| s.parse::<i64>()),
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
        PrimitiveType::Boolean => alt((
            map(tag("true"), |_| ConstValue::Boolean(true)),
            map(tag("false"), |_| ConstValue::Boolean(false)),
        ))(input),
        PrimitiveType::Byte => {
            map(map_res(digit1, |s: &str| s.parse::<u8>()), ConstValue::Byte)(input)
        }
    }
}

fn const_name_val(ty: PrimitiveType) -> impl Fn(&str) -> IResult<&str, (&str, ConstValue)> {
    move |input: &str| {
        let (input, name) = field_name(input)?;
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
/// use rust_lcm_codegen::parser::{const_decl, ConstDecl, ConstValue, PrimitiveType};
/// let parser = const_decl;
///
/// assert_eq!(
///     parser("const int32_t YELLOW=1, GOLDENROD=2, CANARY=3"),
///     Ok((
///         "",
///         vec![
///           ConstDecl {
///             name: "YELLOW".to_owned(),
///             ty: PrimitiveType::Int32,
///             value: ConstValue::Int32(1)
///           },
///           ConstDecl {
///             name: "GOLDENROD".to_owned(),
///             ty: PrimitiveType::Int32,
///             value: ConstValue::Int32(2)
///           },
///           ConstDecl {
///             name: "CANARY".to_owned(),
///             ty: PrimitiveType::Int32,
///             value: ConstValue::Int32(3)
///           },
///         ]
///     ))
/// );
///
/// assert_eq!(
///     parser("const double PI=3.14159"),
///     Ok((
///         "",
///         vec![ConstDecl {
///             name: "PI".to_owned(),
///             ty: PrimitiveType::Double,
///             value: ConstValue::Double("3.14159".to_owned())
///         }]
///     ))
/// );
///
///
/// ```
pub fn const_decl(input: &str) -> IResult<&str, Vec<ConstDecl>> {
    let (input, _) = tuple((tag("const"), space1))(input)?;
    let (input, ty) = primitive_type(input)?;
    let (input, _) = space1(input)?;
    let (input, name_vals) = separated_nonempty_list(spaced_comma, const_name_val(ty))(input)?;

    Ok((
        input,
        name_vals
            .into_iter()
            .map(|(name, value)| ConstDecl {
                name: name.to_string(),
                value,
                ty,
            })
            .collect(),
    ))
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{error::ErrorKind, Err};
}
