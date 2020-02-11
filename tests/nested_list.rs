extern crate generated;
extern crate rust_lcm_codec;

type TestError = rust_lcm_codec::CodecError<
    rust_lcm_codec::BufferReaderError,
    rust_lcm_codec::BufferWriterError,
>;
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
        let pw = generated::nested_list::begin_nested_list_t_write(&mut w)?;
        let mut pw: generated::nested_list::nested_list_t_write_segments<_> = pw
            .write_arbitrary(input.arbitrary)?
            .write_nsegments(input.segments.len() as i32)?;
        // Use the point-list writer at the array-writing state as an iterator
        // that generates single-value-writing delegates
        for (item_writer, segment) in (&mut pw).zip(&input.segments) {
            item_writer.write(|segment_writer| {
                let mut sw = segment_writer.write_nsingles(segment.singles.len() as i32)?;
                for (singles_writer, single) in (&mut sw).zip(&segment.singles) {
                    singles_writer.write(*single)?;
                }
                let sw = sw.done()?;
                let mut sw = sw.write_npairs(segment.pairs.len() as i32)?;
                for (pair_writer, pair) in (&mut sw).zip(&segment.pairs) {
                    pair_writer.write(|pw| {
                        let pw = pw.write_left(pair.0)?;
                        let pw = pw.write_right(pair.1)?;
                        Ok(pw)
                    })?;
                }
                let sw = sw.done()?;
                Ok(sw)
            })?;
        }
        //let pw: generated::nested_list::nested_list_t_Write_points<_> = pw;
        let _write_done: generated::nested_list::nested_list_t_write_done<_> = pw.done()?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::nested_list::begin_nested_list_t_read(&mut r)?;
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
