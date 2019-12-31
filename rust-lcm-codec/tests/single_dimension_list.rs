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
    EncodeValueError(rust_lcm_codec::EncodeValueError<rust_lcm_codec::BufferWriterError>),
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

impl From<rust_lcm_codec::EncodeValueError<rust_lcm_codec::BufferWriterError>> for TestError {
    fn from(e: rust_lcm_codec::EncodeValueError<rust_lcm_codec::BufferWriterError>) -> Self {
        TestError::EncodeValueError(e)
    }
}

#[test]
fn primitive_list_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::single_dimension_list::Point_list_t::begin_write(&mut w)?;
        let mut pw: generated::single_dimension_list::point_list_t_Write_points<_> =
            pw.write_npoints(&5)?;
        let item_values_to_write = [0.0f64, 0.1, 0.2, 0.3, 0.4];
        // Use the point-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        for (item_writer, val) in (&mut pw).zip(&item_values_to_write) {
            item_writer.write(val)?;
        }
        //let pw: generated::single_dimension_list::point_list_t_Write_points<_> = pw;
        let _write_done: generated::single_dimension_list::point_list_t_Write_DONE<_> =
            pw.done()?;
    }
    Ok(())
    // TODO - reading
    //let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    //let pr = generated::nested::Local_nested_t::begin_read(&mut r)?;
    //let mut found_m = Default::default();
    //let mut found_n = Default::default();
    //let (found_j, pr) = pr.read_j()?;
    //let pr = pr.read_k(|field_reader| {
    //    let (m, field_reader) = field_reader.read_m()?;
    //    found_m = m; // Copy content out of the closure to the externally visible state
    //    let (n, field_reader) = field_reader.read_n()?;
    //    found_n = n;
    //    Ok(field_reader)
    //})?;
    //let (found_p, pr) = pr.read_p()?;
    //let _read_done: generated::nested::local_nested_t_Read_DONE<_> = pr;
    //assert_eq!(1, found_j);
    //assert_eq!(2, found_m);
    //assert_eq!(3, found_n);
    //assert_eq!(4, found_p);
    //Ok(())
}
#[test]
fn struct_list_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::single_dimension_list::Struct_list_t::begin_write(&mut w)?;
        let mut pw: generated::single_dimension_list::struct_list_t_Write_points<_> =
            pw.write_npoints(&5)?;
        let item_values_to_write = [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)];
        // Use the point-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        for (item_writer, val) in (&mut pw).zip(&item_values_to_write) {
            item_writer.write(|struct_field_writer| {
                let struct_field_writer = struct_field_writer.write_left(&val.0)?;
                let struct_field_writer = struct_field_writer.write_right(&val.1)?;
                Ok(struct_field_writer)
            })?;
        }
        let _write_done: generated::single_dimension_list::struct_list_t_Write_DONE<_> =
            pw.done()?;
    }
    Ok(())
    // TODO - reading
}
