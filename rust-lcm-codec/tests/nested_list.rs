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
fn nested_list_round_trip_happy_path() -> Result<(), TestError> {
    #[derive(Default, Debug, PartialEq)]
    struct Message {
        arbitrary: u8,
        segments: Vec<Segment>,
    }
    #[derive(Default, Debug, PartialEq)]
    struct Segment {
        singles: Vec<i32>,
        pairs: Vec<(i64, f64)>,
    }

    let input = Message {
        arbitrary: 1,
        segments: vec![
            Segment {
                singles: vec![2, 3, 4],
                pairs: vec![(5, 6.0), (7, 8.0)],
            },
            Segment {
                singles: vec![9, 10, 11, 12],
                pairs: vec![(13, 14.0)],
            },
        ],
    };
    let mut buf = [0u8; 256];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::nested_list::Nested_list_t::begin_write(&mut w)?;
        let mut pw: generated::nested_list::nested_list_t_Write_segments<_> = pw
            .write_arbitrary(&input.arbitrary)?
            .write_nsegments(&(input.segments.len() as i32))?;
        // Use the point-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        for (item_writer, segment) in (&mut pw).zip(&input.segments) {
            item_writer.write(|segment_writer| {
                let mut sw = segment_writer.write_nsingles(&(segment.singles.len() as i32))?;
                for (singles_writer, single) in (&mut sw).zip(&segment.singles) {
                    singles_writer.write(single)?;
                }
                let sw = sw.done()?;
                let mut sw = sw.write_npairs(&(segment.pairs.len() as i32))?;
                for (pair_writer, pair) in (&mut sw).zip(&segment.pairs) {
                    pair_writer.write(|pw| {
                        let pw = pw.write_left(&pair.0)?;
                        let pw = pw.write_right(&pair.1)?;
                        Ok(pw)
                    })?;
                }
                let sw = sw.done()?;
                Ok(sw)
            })?;
        }
        //let pw: generated::nested_list::nested_list_t_Write_points<_> = pw;
        let _write_done: generated::nested_list::nested_list_t_Write_DONE<_> = pw.done()?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::nested_list::Nested_list_t::begin_read(&mut r)?;
    let mut output = Message::default();
    let (found_arbitrary, pr) = pr.read_arbitrary()?;
    output.arbitrary = found_arbitrary;
    let (_, mut pr) = pr.read_nsegments()?;
    for item_reader in &mut pr {
        let mut segment = Segment::default();
        item_reader.read(|segment_reader| {
            let (_, mut segment_reader) = segment_reader.read_nsingles()?;
            for single_reader in &mut segment_reader {
                segment.singles.push(single_reader.read()?);
            }
            let (_, mut segment_reader) = segment_reader.done()?.read_npairs()?;
            for pair_reader in &mut segment_reader {
                let mut left = Default::default();
                let mut right = Default::default();
                pair_reader.read(|pair_reader| {
                    let (l, pair_reader) = pair_reader.read_left()?;
                    left = l;
                    let (r, pair_reader) = pair_reader.read_right()?;
                    right = r;
                    Ok(pair_reader)
                })?;
                segment.pairs.push((left, right));
            }
            Ok(segment_reader.done()?)
        })?;
        output.segments.push(segment);
    }
    assert_eq!(output, input);
    Ok(())
}
