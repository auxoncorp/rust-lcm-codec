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
    let item_values_to_write = [0.0f64, 0.1, 0.2, 0.3, 0.4];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::single_dimension_list::begin_point_list_t_write(&mut w)?;
        let mut pw: generated::single_dimension_list::point_list_t_write_points<_> =
            pw.write_npoints(5)?;
        // Use the point-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        for (item_writer, val) in (&mut pw).zip(&item_values_to_write) {
            item_writer.write(*val)?;
        }
        let _write_done: generated::single_dimension_list::point_list_t_write_done<_> =
            pw.done()?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::single_dimension_list::begin_point_list_t_read(&mut r)?;
    let (found_npoints, mut pr) = pr.read_npoints()?;
    let mut point_storage = [0.0f64; 5];
    for (item_reader, point_destination) in (&mut pr).zip(point_storage.iter_mut()) {
        *point_destination = item_reader.read()?;
    }
    assert_eq!(5, found_npoints);
    assert_eq!(&point_storage, &item_values_to_write);
    let _read_done: generated::single_dimension_list::point_list_t_read_done<_> = pr.done()?;
    Ok(())
}
#[test]
fn struct_list_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [(0, 0), (1, 2), (2, 4), (3, 6), (4, 8)];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::single_dimension_list::begin_struct_list_t_write(&mut w)?;
        let mut pw: generated::single_dimension_list::struct_list_t_write_pairs<_> =
            pw.write_npairs(5)?;
        // Use the pair-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        for (item_writer, val) in (&mut pw).zip(&item_values_to_write) {
            item_writer.write(|struct_field_writer| {
                let struct_field_writer = struct_field_writer.write_left(val.0)?;
                let struct_field_writer = struct_field_writer.write_right(val.1)?;
                Ok(struct_field_writer)
            })?;
        }
        let _write_done: generated::single_dimension_list::struct_list_t_write_done<_> =
            pw.done()?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::single_dimension_list::begin_struct_list_t_read(&mut r)?;
    let (found_npairs, mut pr) = pr.read_npairs()?;
    assert_eq!(5, found_npairs);
    let mut read_pairs = vec![];
    for item_reader in &mut pr {
        let mut found_left = Default::default();
        let mut found_right = Default::default();
        item_reader.read(|struct_field_reader| {
            let (left, struct_field_reader) = struct_field_reader.read_left()?;
            found_left = left;
            let (right, struct_field_reader) = struct_field_reader.read_right()?;
            found_right = right;
            Ok(struct_field_reader)
        })?;
        read_pairs.push((found_left, found_right));
    }
    assert_eq!(&read_pairs, &item_values_to_write);
    let _read_done: generated::single_dimension_list::struct_list_t_read_done<_> = pr.done()?;
    Ok(())
}
#[test]
fn multiple_list_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let dots_to_write = [1, 10, 100, 1000, 10000];
    let dashes_to_write = [2i8, 20, 120];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::single_dimension_list::begin_morse_segment_t_write(&mut w)?;
        let mut pw: generated::single_dimension_list::morse_segment_t_write_dots<_> = pw
            .write_ndots(dots_to_write.len() as i32)?
            .write_ndashes(dashes_to_write.len() as i32)?;
        for (item_writer, dot) in (&mut pw).zip(&dots_to_write) {
            item_writer.write(*dot)?;
        }
        let mut pw: generated::single_dimension_list::morse_segment_t_write_dashes<_> =
            pw.done()?;
        for (item_writer, dash) in (&mut pw).zip(&dashes_to_write) {
            item_writer.write(*dash)?;
        }
        let _pw: generated::single_dimension_list::morse_segment_t_write_done<_> = pw.done()?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::single_dimension_list::begin_morse_segment_t_read(&mut r)?;
    let (found_ndots, pr) = pr.read_ndots()?;
    assert_eq!(found_ndots as usize, dots_to_write.len());
    let (found_ndashes, mut pr) = pr.read_ndashes()?;
    assert_eq!(found_ndashes as usize, dashes_to_write.len());
    let mut found_dots = vec![];
    for item_reader in &mut pr {
        found_dots.push(item_reader.read()?);
    }
    let mut pr = pr.done()?;
    assert_eq!(&found_dots, &dots_to_write);
    let mut found_dashes = vec![];
    for item_reader in &mut pr {
        found_dashes.push(item_reader.read()?);
    }
    assert_eq!(&found_dashes, &dashes_to_write);
    let _pr: generated::single_dimension_list::morse_segment_t_read_done<_> = pr.done()?;
    Ok(())
}
