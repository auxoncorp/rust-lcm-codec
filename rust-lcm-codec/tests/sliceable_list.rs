extern crate generated;
extern crate rust_lcm_codec;
use core::mem::{transmute, MaybeUninit};

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
fn byte_slice_write_round_trip() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        let _write_done: generated::sliceable_list::blob_t_write_done<_> =
            pw.bytes_copy_from_slice(&item_values_to_write)?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, mut pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    for (item_reader, point_destination) in (&mut pr).zip(point_storage.iter_mut()) {
        *point_destination = item_reader.read()?;
    }
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    let _read_done: generated::sliceable_list::blob_t_read_done<_> = pr.done()?;
    Ok(())
}

#[test]
fn byte_slice_writer_mixed_with_item_iterator_round_trip() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let mut pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        // Use the point-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        // ... for just the first 3 values
        for (item_writer, val) in (&mut pw).zip(&item_values_to_write).take(3) {
            item_writer.write(*val)?;
        }
        // then use bulk slice operations for the rest of the values
        let _write_done: generated::sliceable_list::blob_t_write_done<_> =
            pw.bytes_copy_from_slice(&item_values_to_write[3..])?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, mut pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    for (item_reader, point_destination) in (&mut pr).zip(point_storage.iter_mut()) {
        *point_destination = item_reader.read()?;
    }
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    let _read_done: generated::sliceable_list::blob_t_read_done<_> = pr.done()?;
    Ok(())
}

#[test]
fn byte_slice_direct_bulk_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        let (bytes_ref, _write_done): (
            &mut [MaybeUninit<u8>],
            generated::sliceable_list::blob_t_write_done<_>,
        ) = pw.bytes_as_mut_slice()?;
        let bytes_ref: &mut [u8] = unsafe { transmute(bytes_ref) }; // bulk assume init equivalent
        bytes_ref.copy_from_slice(&item_values_to_write);
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, mut pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    for (item_reader, point_destination) in (&mut pr).zip(point_storage.iter_mut()) {
        *point_destination = item_reader.read()?;
    }
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    let _read_done: generated::sliceable_list::blob_t_read_done<_> = pr.done()?;
    Ok(())
}

#[test]
fn byte_slice_direct_individual_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        let (bytes_ref, _write_done): (_, generated::sliceable_list::blob_t_write_done<_>) =
            pw.bytes_as_mut_slice()?;
        for (dest_byte, source_byte) in bytes_ref.iter_mut().zip(&item_values_to_write) {
            *dest_byte = MaybeUninit::new(*source_byte);
        }
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, mut pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    for (item_reader, point_destination) in (&mut pr).zip(point_storage.iter_mut()) {
        *point_destination = item_reader.read()?;
    }
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    let _read_done: generated::sliceable_list::blob_t_read_done<_> = pr.done()?;
    Ok(())
}

#[test]
fn byte_slice_read_round_trip() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];

    let n_bytes_written = {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        let _write_done: generated::sliceable_list::blob_t_write_done<_> =
            pw.bytes_copy_from_slice(&item_values_to_write)?;
        w.cursor()
    };
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    let (point_slice, _pr_done): (&[u8], generated::sliceable_list::blob_t_read_done<_>) =
        pr.bytes_as_slice()?;
    (&mut point_storage).copy_from_slice(point_slice);
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    assert_eq!(n_bytes_written, r.cursor());
    Ok(())
}

#[test]
fn byte_slice_read_after_full_iteration_round_trip() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];

    let n_bytes_written = {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        let _write_done: generated::sliceable_list::blob_t_write_done<_> =
            pw.bytes_copy_from_slice(&item_values_to_write)?;
        w.cursor()
    };
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, mut pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    for (bytes_item_reader, storage_destination) in (&mut pr).zip(point_storage.iter_mut()) {
        *storage_destination = bytes_item_reader.read()?;
    }
    let (point_slice, _pr_done): (&[u8], generated::sliceable_list::blob_t_read_done<_>) =
        pr.bytes_as_slice()?;
    assert_eq!(0, point_slice.len());
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    assert_eq!(n_bytes_written, r.cursor());
    Ok(())
}

#[test]
fn byte_slice_read_after_partial_iteration_round_trip() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    let item_values_to_write = [2u8, 4, 8, 16, 32, 64];

    let n_bytes_written = {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::sliceable_list::begin_blob_t_write(&mut w)?;
        let pw: generated::sliceable_list::blob_t_write_bytes<_> =
            pw.write_nbytes(item_values_to_write.len() as i32)?;
        let _write_done: generated::sliceable_list::blob_t_write_done<_> =
            pw.bytes_copy_from_slice(&item_values_to_write)?;
        w.cursor()
    };
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::sliceable_list::begin_blob_t_read(&mut r)?;
    let (found_nbytes, mut pr) = pr.read_nbytes()?;
    let mut point_storage = [0u8; 6];
    // Note that we only consume the first 3 items
    for (bytes_item_reader, storage_destination) in (&mut pr).zip(point_storage.iter_mut()).take(3)
    {
        *storage_destination = bytes_item_reader.read()?;
    }
    let (point_slice, _pr_done): (&[u8], generated::sliceable_list::blob_t_read_done<_>) =
        pr.bytes_as_slice()?;
    // Note that we work on the latter half of the bytes
    (&mut point_storage[3..]).copy_from_slice(point_slice);
    assert_eq!(6, found_nbytes);
    assert_eq!(&point_storage, &item_values_to_write);
    assert_eq!(n_bytes_written, r.cursor());
    Ok(())
}
