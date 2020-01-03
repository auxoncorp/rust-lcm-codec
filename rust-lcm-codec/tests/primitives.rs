extern crate generated;
extern crate rust_lcm_codec;

use rust_lcm_codec::BufferWriterError;

#[derive(Debug)]
enum TestError {
    BufferWriterError(rust_lcm_codec::BufferWriterError),
    DecodeFingerprintError(
        rust_lcm_codec::DecodeFingerprintError<rust_lcm_codec::BufferReaderError>,
    ),
    DecodeValueError(rust_lcm_codec::DecodeValueError<rust_lcm_codec::BufferReaderError>),
}

impl From<rust_lcm_codec::BufferWriterError> for TestError {
    fn from(e: BufferWriterError) -> Self {
        TestError::BufferWriterError(e)
    }
}

impl From<rust_lcm_codec::DecodeFingerprintError<rust_lcm_codec::BufferReaderError>> for TestError {
    fn from(e: rust_lcm_codec::DecodeFingerprintError<rust_lcm_codec::BufferReaderError>) -> Self {
        TestError::DecodeFingerprintError(e)
    }
}

impl From<rust_lcm_codec::DecodeValueError<rust_lcm_codec::BufferReaderError>> for TestError {
    fn from(e: rust_lcm_codec::DecodeValueError<rust_lcm_codec::BufferReaderError>) -> Self {
        TestError::DecodeValueError(e)
    }
}

#[test]
fn prim_test_read_direct() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::primitives::begin_primitives_t_write(&mut w)?;
        let _write_done: generated::primitives::primitives_t_write_done<_> = pw
            .write_int8_field(1)?
            .write_int16_field(2)?
            .write_int32_field(3)?
            .write_int64_field(4)?
            .write_float_field(5.0)?
            .write_double_field(6.0)?
            .write_string_field("seven")?
            .write_bool_field(true)?
            .write_byte_field(8)?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::primitives::begin_primitives_t_read(&mut r)?;
    let (int8_field, pr) = pr.read_int8_field()?;
    let (int16_field, pr) = pr.read_int16_field()?;
    let (int32_field, pr) = pr.read_int32_field()?;
    let (int64_field, pr) = pr.read_int64_field()?;
    let (float_field, pr) = pr.read_float_field()?;
    let (double_field, pr) = pr.read_double_field()?;
    let (string_field, pr) = pr.read_string_field()?;
    let (bool_field, pr) = pr.read_bool_field()?;
    let (byte_field, pr) = pr.read_byte_field()?;
    let _read_done: generated::primitives::primitives_t_read_done<_> = pr;
    assert_eq!(1, int8_field);
    assert_eq!(2, int16_field);
    assert_eq!(3, int32_field);
    assert_eq!(4, int64_field);
    assert_eq!(5.0, float_field);
    assert_eq!(6.0, double_field);
    assert_eq!("seven", string_field);
    assert_eq!(true, bool_field);
    assert_eq!(8, byte_field);
    Ok(())
}

// TODO - test writer error piping
