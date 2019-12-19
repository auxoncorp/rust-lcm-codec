#![allow(dead_code)]
#![allow(unused_imports)]

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{alpha1, multispace0, multispace1, space0, space1},
    character::is_alphanumeric,
    combinator::{cut, map, opt, recognize, value},
    do_parse,
    error::ParseError,
    multi::separated_nonempty_list,
    named,
    sequence::{delimited, separated_pair, terminated, tuple},
    IResult,
};

#[derive(Debug, Eq, PartialEq)]
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
pub struct Field {
    pub name: String,
    pub ty: PrimitiveType,
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
/// use rust_lcm_codegen::parser::{primitive, PrimitiveType};
/// let parser = primitive;
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
pub fn primitive(input: &str) -> IResult<&str, PrimitiveType> {
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
/// use rust_lcm_codegen::parser::{field_decl, Field, PrimitiveType};
/// let parser = field_decl;
///
/// assert_eq!(
///     parser("int8_t foo"),
///     Ok((
///         "",
///         Field {
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
pub fn field_decl(input: &str) -> IResult<&str, Field> {
    map(separated_pair(primitive, space1, field_name), |(ty, name)| {
        Field {
            ty,
            name: name.to_owned(),
        }
    })(input)
}

#[cfg(test)]
mod test {
    use super::*;
    use nom::{error::ErrorKind, Err};

}
