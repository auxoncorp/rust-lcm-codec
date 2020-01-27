extern crate generated;
extern crate rust_lcm_codec;

type TestError = rust_lcm_codec::CodecError<
    rust_lcm_codec::BufferReaderError,
    rust_lcm_codec::BufferWriterError,
>;
#[test]
fn nested_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::nested::begin_local_nested_t_write(&mut w)?;
        let _write_done: generated::nested::local_nested_t_write_done<_> = pw
            .write_j(1)?
            .write_k(|local_primitive_write_ready| {
                Ok(local_primitive_write_ready.write_m(2)?.write_n(3)?)
            })?
            .write_p(4)?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::nested::begin_local_nested_t_read(&mut r)?;
    let mut found_m = Default::default();
    let mut found_n = Default::default();
    let (found_j, pr) = pr.read_j()?;
    let pr = pr.read_k(|field_reader| {
        let (m, field_reader) = field_reader.read_m()?;
        found_m = m; // Copy content out of the closure to the externally visible state
        let (n, field_reader) = field_reader.read_n()?;
        found_n = n;
        Ok(field_reader)
    })?;
    let (found_p, pr) = pr.read_p()?;
    let _read_done: generated::nested::local_nested_t_read_done<_> = pr;
    assert_eq!(1, found_j);
    assert_eq!(2, found_m);
    assert_eq!(3, found_n);
    assert_eq!(4, found_p);
    Ok(())
}

#[test]
fn nested_remote_package_field_round_trip_happy_path() -> Result<(), TestError> {
    let mut buf = [0u8; 256];
    {
        let mut w = rust_lcm_codec::BufferWriter::new(&mut buf);
        let pw = generated::nested::begin_remote_nested_t_write(&mut w)?;
        let _write_done: generated::nested::remote_nested_t_write_done<_> = pw
            .write_j(1)?
            .write_k(|local_primitive_write_ready| {
                Ok(local_primitive_write_ready
                    .write_utime(2)?
                    .write_degCelsius(3.0)?)
            })?
            .write_p(4)?;
    }
    let mut r = rust_lcm_codec::BufferReader::new(&mut buf);
    let pr = generated::nested::begin_remote_nested_t_read(&mut r)?;
    let mut found_utime = Default::default();
    let mut found_deg_celsius = Default::default();
    let (found_j, pr) = pr.read_j()?;
    let pr = pr.read_k(|field_reader| {
        let (utime, field_reader) = field_reader.read_utime()?;
        found_utime = utime; // Copy content out of the closure to the externally visible state
        let (deg_celsius, field_reader) = field_reader.read_degCelsius()?;
        found_deg_celsius = deg_celsius;
        Ok(field_reader)
    })?;
    let (found_p, pr) = pr.read_p()?;
    let _read_done: generated::nested::remote_nested_t_read_done<_> = pr;
    assert_eq!(1, found_j);
    assert_eq!(2, found_utime);
    assert_eq!(3.0, found_deg_celsius);
    assert_eq!(4, found_p);
    Ok(())
}
// TODO - test writer error piping
