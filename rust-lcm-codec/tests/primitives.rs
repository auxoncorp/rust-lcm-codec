extern crate generated;
extern crate rust_lcm_codec;

#[test]
fn prim_test() -> Result<(), rust_lcm_codec::BufferWriterError> {
    let mut buf = [0u8; 256];
    let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);

    let pw = generated::primitives::Primitives_t::begin_write(&mut w)?;
    let _done: generated::primitives::primitives_t_Write_DONE<_> = pw
        .write_int8_field(&1)?
        .write_int16_field(&2)?
        .write_int32_field(&3)?
        .write_int64_field(&4)?
        .write_float_field(&5.0)?
        .write_double_field(&6.0)?
        .write_string_field("seven")?
        .write_bool_field(&true)?
        .write_byte_field(&8)?;

    // println!("{:#x?}", &buf[..]);
    // panic!();

    Ok(())
}
