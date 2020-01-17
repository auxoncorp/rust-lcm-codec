//! Runtime support for LCM serialization and deserialization in Rust
#![no_std]
#![deny(warnings)]
#![deny(missing_docs)]

/// The errors that can occur when decoding the LCM type hash / fingerprint
/// for a message.
#[derive(Debug)]
pub enum DecodeFingerprintError<E> {
    /// The fingerprint value found did not match the expected value for the
    /// message type of interest.
    InvalidFingerprint(u64),
    /// The underlying StreamingReader encountered an error.
    ReaderError(E),
}

impl<E> From<E> for DecodeFingerprintError<E> {
    fn from(e: E) -> Self {
        DecodeFingerprintError::ReaderError(e)
    }
}

/// The errors that can occur when decoding a value in the body
/// of an LCM message.
#[derive(Debug)]
pub enum DecodeValueError<E> {
    /// The user attempted to read more or fewer items
    /// out of an array than the array contained.
    ArrayLengthMismatch(&'static str),
    /// The value attempted to be decoded was invalid
    /// in some way.
    InvalidValue(&'static str),
    /// The underlying StreamingReader encountered an error.
    ReaderError(E),
}

impl<E> From<E> for DecodeValueError<E> {
    fn from(e: E) -> Self {
        DecodeValueError::ReaderError(e)
    }
}

/// The errors that can occur when encoding a value in the body
/// of an LCM message.
#[derive(Debug, PartialEq, Eq)]
pub enum EncodeValueError<E> {
    /// The user attempted to write more or fewer items
    /// into an array than the array contained.
    ArrayLengthMismatch(&'static str),
    /// The value attempted to be encoded was invalid
    /// in some way.
    InvalidValue(&'static str),
    /// The underlying StreamingWriter encountered an error.
    WriterError(E),
}

impl<E> From<E> for EncodeValueError<E> {
    fn from(e: E) -> Self {
        EncodeValueError::WriterError(e)
    }
}

/// Reader backend trait
pub trait StreamingReader {
    /// The kind of Error the implementation produces
    type Error;
    /// Read bytes from the underlying data source into the provided `buf`.
    /// Should return an error if insufficient bytes are available to
    /// fully fill `buf`.
    fn read_bytes(&mut self, buf: &mut [u8]) -> Result<(), Self::Error>;

    /// Expose an aliased view of a subset of the underlying data as
    /// immutable bytes.
    ///
    /// The implementer must ensure that the view of bytes returned
    /// does not overlap with the region of bytes that it allows itself
    /// to mutate at any further point.
    fn share_bytes(&mut self, len: usize) -> Result<&[u8], Self::Error>;
}

/// The BufferReader had a problem. The only problem worth mentioning
/// is that it did not have enough bytes to complete a requested
/// operations.
#[derive(Debug)]
pub struct BufferReaderError;

/// StreamingReader backend for a byte slice
pub struct BufferReader<'a> {
    buffer: &'a [u8],
    cursor: usize,
}

impl<'a> BufferReader<'a> {
    /// Make a new BufferReader around a byte slice
    pub fn new(buffer: &'a [u8]) -> BufferReader<'a> {
        BufferReader { buffer, cursor: 0 }
    }

    /// How many bytes have been read thus far
    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

impl<'a> From<&'a [u8]> for BufferReader<'a> {
    fn from(buffer: &'a [u8]) -> Self {
        BufferReader::new(buffer)
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
            // This is unsafe because we are providing a shared a shareable immutable reference
            // to a part of a slice we currently holding a mutable (read: unshareable, solitary)
            // reference to via the lifetime from `&mut self`.
            //
            // We know that this type will not in fact be able to mutate the byte slice underneath
            // that shared immutable reference because the BufferRead operates solely in a forward
            // fashion and the cursor is moved past the immutable region. It helps that we also
            // never mutate the underlying buffer anyhow.
            let s =
                unsafe { core::slice::from_raw_parts(self.buffer.as_ptr().add(self.cursor), len) };
            self.cursor += len;
            Ok(s)
        } else {
            Err(BufferReaderError)
        }
    }
}

/// Writer backend trait
pub trait StreamingWriter {
    /// The kind of errors that the implementation emits during encoding
    type Error;
    /// Write all of the bytes from the provided buffer into the underlying
    /// encoding stream.
    ///
    /// Ought to produce an error if not all of the bytes could be written.
    ///
    /// N.B. for possible enhancement: We could return size written here
    /// rather than leaving that tracking and manner of exposure
    /// to the implementing type.
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;

    /// Ensure that all bytes are fully written in a maximally durable fashion.
    fn flush() -> Result<(), Self::Error>;
}

/// The BufferWriter had a problem. The only problem worth mentioning
/// is that it did not have enough bytes to complete a requested
/// operations.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct BufferWriterError;

/// Writer backend for a byte slice
pub struct BufferWriter<'a> {
    buffer: &'a mut [u8],
    cursor: usize,
}

impl<'a> BufferWriter<'a> {
    /// Create a new BufferWriter
    pub fn new(buffer: &'a mut [u8]) -> BufferWriter<'a> {
        BufferWriter { buffer, cursor: 0 }
    }

    /// How many bytes have been written thus far
    pub fn cursor(&self) -> usize {
        self.cursor
    }
}

impl<'a> StreamingWriter for BufferWriter<'a> {
    type Error = BufferWriterError;

    #[inline(always)]
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let len = bytes.len();
        let end = self.cursor + len;
        if end <= self.buffer.len() {
            self.buffer[self.cursor..end].copy_from_slice(bytes);
            self.cursor += len;
            Ok(())
        } else {
            Err(BufferWriterError)
        }
    }

    #[inline(always)]
    fn flush() -> Result<(), Self::Error> {
        Ok(())
    }
}

/// Value serialization helper trait, oriented towards primitives.
pub trait SerializeValue: Sized {
    /// Use a StreamingReader to produce an instance of the implementing type
    /// from an encoded stream of LCM data
    fn read_value<R: StreamingReader>(reader: &mut R) -> Result<Self, DecodeValueError<R::Error>>;
    /// Use a StreamingWriter to write an instance of the implementing type
    /// to an encoded stream of LCM data
    fn write_value<W: StreamingWriter>(val: Self, writer: &mut W) -> Result<(), W::Error>;
}

macro_rules! primitive_serialize_impl {
    ($ty:ty) => {
        impl SerializeValue for $ty {
            #[inline(always)]
            fn read_value<R: StreamingReader>(
                reader: &mut R,
            ) -> Result<Self, DecodeValueError<R::Error>> {
                let mut bytes = Self::default().to_ne_bytes();
                reader.read_bytes(&mut bytes)?;
                Ok(Self::from_be_bytes(bytes))
            }

            #[inline(always)]
            fn write_value<W: StreamingWriter>(val: Self, writer: &mut W) -> Result<(), W::Error> {
                writer.write_bytes(&val.to_be_bytes())
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

/// Write a string to a StreamingWriter using LCM's convention of encoding strings.
pub fn write_str_value<W: StreamingWriter>(string: &str, writer: &mut W) -> Result<(), W::Error> {
    writer.write_bytes(&(&(string.len() as i32 + 1)).to_be_bytes())?;
    writer.write_bytes(&string.as_bytes())?;
    writer.write_bytes(&[0])
}

/// Read a view of a string from a StreamingReader using LCM's convention of encoding strings.
pub fn read_str_value<R: StreamingReader>(
    reader: &mut R,
) -> Result<&str, DecodeValueError<<R as StreamingReader>::Error>> {
    let len: i32 = i32::read_value(reader)?;
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
        reader: &mut R,
    ) -> Result<Self, DecodeValueError<<R as StreamingReader>::Error>> {
        let mut buffer = [0u8; 1];
        reader.read_bytes(&mut buffer)?;
        match buffer[0] {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err(DecodeValueError::InvalidValue("invalid bool value")),
        }
    }

    fn write_value<W: StreamingWriter>(val: Self, writer: &mut W) -> Result<(), W::Error> {
        SerializeValue::write_value(if val { 1i8 } else { 0i8 }, writer)
    }
}
