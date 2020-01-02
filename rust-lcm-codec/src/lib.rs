#![no_std]
#[derive(Debug)]
pub enum DecodeFingerprintError<E> {
    InvalidFingerprint(u64),
    ReaderError(E),
}

impl<E> From<E> for DecodeFingerprintError<E> {
    fn from(e: E) -> Self {
        DecodeFingerprintError::ReaderError(e)
    }
}

#[derive(Debug)]
pub enum DecodeValueError<E> {
    ArrayLengthMismatch(&'static str),
    InvalidValue(&'static str),
    ReaderError(E),
}

impl<E> From<E> for DecodeValueError<E> {
    fn from(e: E) -> Self {
        DecodeValueError::ReaderError(e)
    }
}

#[derive(Debug)]
pub enum EncodeValueError<E> {
    ArrayLengthMismatch(&'static str),
    InvalidValue(&'static str),
    WriterError(E),
}

impl<E> From<E> for EncodeValueError<E> {
    fn from(e: E) -> Self {
        EncodeValueError::WriterError(e)
    }
}

/// Reader backend trait
pub trait StreamingReader {
    type Error;
    fn read_bytes(&mut self, buf: &mut [u8]) -> Result<(), Self::Error>;
    fn share_bytes(&mut self, len: usize) -> Result<&[u8], Self::Error>;
}

/// Reader backend for a byte slice
#[derive(Debug)]
pub struct BufferReaderError;

pub struct BufferReader<'a> {
    buffer: &'a [u8],
    cursor: usize,
}

impl<'a> BufferReader<'a> {
    pub fn new(buffer: &'a [u8]) -> BufferReader<'a> {
        BufferReader { buffer, cursor: 0 }
    }
}

impl<'a> StreamingReader for BufferReader<'a> {
    type Error = BufferReaderError;

    fn read_bytes(&mut self, buf: &mut [u8]) -> Result<(), Self::Error> {
        let len = buf.len();
        let end = self.cursor + len;
        if end <= self.buffer.len() {
            buf.copy_from_slice(&self.buffer[self.cursor..end]);
            self.cursor += len;
            Ok(())
        } else {
            Err(BufferReaderError)
        }
    }

    fn share_bytes(&mut self, len: usize) -> Result<&[u8], Self::Error> {
        let end = self.cursor + len;
        if end <= self.buffer.len() {
            //buf.copy_from_slice(&self.buffer[self.cursor..end]);
            let s =
                unsafe { core::slice::from_raw_parts(self.buffer.as_ptr().add(self.cursor), len) };
            self.cursor += len;
            Ok(s)
        } else {
            Err(BufferReaderError)
        }
    }
}

// Writer backend trait
pub trait StreamingWriter {
    type Error;
    // TODO: return size written?
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    fn flush() -> Result<(), Self::Error>;
}

// Writer backend for a byte slice

#[derive(Debug)]
pub enum BufferWriterError {}

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
pub trait SerializeValue: Sized {
    fn read_value<R: StreamingReader>(
        &mut self,
        reader: &mut R,
    ) -> Result<(), DecodeValueError<R::Error>>;
    fn read_new_value<R: StreamingReader>(
        reader: &mut R,
    ) -> Result<Self, DecodeValueError<R::Error>>;
    fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error>;
}

macro_rules! primitive_serialize_impl {
    ($ty:ty) => {
        impl SerializeValue for $ty {
            #[inline(always)]
            fn read_value<R: StreamingReader>(
                &mut self,
                reader: &mut R,
            ) -> Result<(), DecodeValueError<R::Error>> {
                let mut bytes = self.to_ne_bytes();
                reader.read_bytes(&mut bytes)?;
                *self = Self::from_be_bytes(bytes);
                Ok(())
            }

            #[inline(always)]
            fn read_new_value<R: StreamingReader>(
                reader: &mut R,
            ) -> Result<Self, DecodeValueError<R::Error>> {
                let mut bytes = Self::default().to_ne_bytes();
                reader.read_bytes(&mut bytes)?;
                Ok(Self::from_be_bytes(bytes))
            }

            #[inline(always)]
            fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
                writer.write_bytes(&self.to_be_bytes())
            }
        }
    };
}

primitive_serialize_impl!(i8);
primitive_serialize_impl!(i16);
primitive_serialize_impl!(i32);
primitive_serialize_impl!(i64);
primitive_serialize_impl!(f32);
primitive_serialize_impl!(f64);
primitive_serialize_impl!(u8);

pub fn write_str_value<W: StreamingWriter>(string: &str, writer: &mut W) -> Result<(), W::Error> {
    writer.write_bytes(&(&(string.len() as i32 + 1)).to_be_bytes())?;
    writer.write_bytes(&string.as_bytes())?;
    writer.write_bytes(&[0])
}

pub fn read_str_value<R: StreamingReader>(
    reader: &mut R,
) -> Result<&str, DecodeValueError<<R as StreamingReader>::Error>> {
    let mut len: i32 = 0;
    len.read_value(reader)?;
    if len < 0 {
        return Err(DecodeValueError::InvalidValue("str length was less than 0"));
    }
    let len = len as usize;
    // Read the bytes, including the null terminator
    let bytes = reader.share_bytes(len)?;
    let s = match core::str::from_utf8(&bytes[..len - 1]) {
        Ok(s) => s,
        Err(_) => return Err(DecodeValueError::InvalidValue("str was not valid UTF8")),
    };
    Ok(s)
}

impl SerializeValue for bool {
    fn read_value<R: StreamingReader>(
        &mut self,
        reader: &mut R,
    ) -> Result<(), DecodeValueError<<R as StreamingReader>::Error>> {
        let mut buffer = [0u8; 1];
        reader.read_bytes(&mut buffer)?;
        *self = match buffer[0] {
            0 => false,
            1 => true,
            _ => return Err(DecodeValueError::InvalidValue("invalid bool value")),
        };
        Ok(())
    }

    fn read_new_value<R: StreamingReader>(
        reader: &mut R,
    ) -> Result<Self, DecodeValueError<<R as StreamingReader>::Error>> {
        let mut buffer = [0u8; 1];
        reader.read_bytes(&mut buffer)?;
        match buffer[0] {
            0 => Ok(false),
            1 => Ok(true),
            _ => return Err(DecodeValueError::InvalidValue("invalid bool value")),
        }
    }

    fn write_value<W: StreamingWriter>(&self, writer: &mut W) -> Result<(), W::Error> {
        (if *self { 1i8 } else { 0i8 }).write_value(writer)
    }
}
