
// Writer backend trait
pub trait StreamingWriter {
    type Error;
    // TODO: return size written?
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    fn flush() -> Result<(), Self::Error>;
}

// Writer backend for a byte slice

#[derive(Debug)]
pub enum BufferWriterError { }

pub struct BufferWriter<'a> {
    buffer: &'a mut [u8],
    cursor: usize,
}

impl<'a> BufferWriter<'a> {
    pub fn new(buffer: &'a mut [u8]) -> BufferWriter<'a> {
        BufferWriter { buffer, cursor: 0 }
    }
}

impl<'a> StreamingWriter for BufferWriter<'a> {
    type Error = BufferWriterError;

    #[inline(always)]
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let len = bytes.len();
        self.buffer[self.cursor..self.cursor + len].copy_from_slice(bytes);
        self.cursor += len;
        Ok(())
    }

    #[inline(always)]
    fn flush() -> Result<(), Self::Error> {
        Ok(())
    }
}

// Value serialization trait
pub trait SerializeValue {
    fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error>;
}

macro_rules! primitive_serialize_impl {
    ($ty:ty) => {
        impl SerializeValue for $ty {
            #[inline(always)]
            fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
                writer.write_bytes(&self.to_be_bytes())
            }
        }
    }
}

primitive_serialize_impl!(i8);
primitive_serialize_impl!(i16);
primitive_serialize_impl!(i32);
primitive_serialize_impl!(i64);
primitive_serialize_impl!(f32);
primitive_serialize_impl!(f64);
primitive_serialize_impl!(u8);


impl SerializeValue for str {
    fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        writer.write_bytes(&(&(self.len() as i32 + 1)).to_be_bytes())?;
        writer.write_bytes(&self.as_bytes())?;
        writer.write_bytes(&[0])
    }
}

impl SerializeValue for bool {
    fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        (if *self { 1i8 } else { 0i8 }).write_value(writer)
    }
}
