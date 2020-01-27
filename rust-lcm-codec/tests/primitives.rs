extern crate generated;
extern crate rust_lcm_codec;

use rust_lcm_codec::BufferWriterError;

type TestError = rust_lcm_codec::CodecError<
    rust_lcm_codec::BufferReaderError,
    rust_lcm_codec::BufferWriterError,
>;

#[test]
fn not_big_enough_buffer_for_fingerprint() {
    let mut buf = [0u8; 7];
    let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
    if let Err(e) = generated::primitives::begin_primitives_t_write(&mut w) {
        assert_eq!(
            rust_lcm_codec::EncodeFingerprintError::WriterError(BufferWriterError),
            e
        );
    } else {
        panic!("Expected an error, dagnabit");
    }
}

#[test]
fn not_big_enough_buffer_for_field() {
    let mut buf = [0u8; 8];
    let mut buffer_writer = rust_lcm_codec::BufferWriter::new(&mut buf);
    let w = generated::primitives::begin_primitives_t_write(&mut buffer_writer)
        .expect("Enough space for fingerprint");
    if let Err(e) = w.write_int8_field(1) {
        assert_eq!(
            rust_lcm_codec::EncodeValueError::WriterError(BufferWriterError),
            e
        );
    } else {
        panic!("Expected an error");
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
