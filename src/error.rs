//! Errors and their conversions relevant to the LCM
//! codec and the generated code which uses it.

/// Catch-all error for anything that can go wrong encoding
/// or decoding an LCM message.
#[derive(Debug, PartialEq)]
pub enum CodecError<RE, WE> {
    /// Error decoding (reading) an LCM message
    DecodeError(DecodeError<RE>),
    /// Error encoding (writing) an LCM message
    EncodeError(EncodeError<WE>),
}

impl<RE, WE> From<DecodeError<RE>> for CodecError<RE, WE> {
    #[inline]
    fn from(e: DecodeError<RE>) -> Self {
        CodecError::DecodeError(e)
    }
}

impl<RE, WE> From<DecodeFingerprintError<RE>> for CodecError<RE, WE> {
    #[inline]
    fn from(e: DecodeFingerprintError<RE>) -> Self {
        CodecError::DecodeError(DecodeError::DecodeFingerprintError(e))
    }
}

impl<RE, WE> From<DecodeValueError<RE>> for CodecError<RE, WE> {
    #[inline]
    fn from(e: DecodeValueError<RE>) -> Self {
        CodecError::DecodeError(DecodeError::DecodeValueError(e))
    }
}

impl<RE, WE> From<EncodeError<WE>> for CodecError<RE, WE> {
    #[inline]
    fn from(e: EncodeError<WE>) -> Self {
        CodecError::EncodeError(e)
    }
}

impl<RE, WE> From<EncodeFingerprintError<WE>> for CodecError<RE, WE> {
    #[inline]
    fn from(e: EncodeFingerprintError<WE>) -> Self {
        CodecError::EncodeError(EncodeError::EncodeFingerprintError(e))
    }
}

impl<RE, WE> From<EncodeValueError<WE>> for CodecError<RE, WE> {
    #[inline]
    fn from(e: EncodeValueError<WE>) -> Self {
        CodecError::EncodeError(EncodeError::EncodeValueError(e))
    }
}

/// The errors that can occur when decoding an LCM message.
#[derive(Debug, PartialEq)]
pub enum DecodeError<E> {
    /// Error decoding fingerprint prior to message body
    DecodeFingerprintError(DecodeFingerprintError<E>),
    /// Error decoding a value in the message body
    DecodeValueError(DecodeValueError<E>),
}

impl<E> From<DecodeFingerprintError<E>> for DecodeError<E> {
    #[inline]
    fn from(e: DecodeFingerprintError<E>) -> Self {
        DecodeError::DecodeFingerprintError(e)
    }
}

impl<E> From<DecodeValueError<E>> for DecodeError<E> {
    #[inline]
    fn from(e: DecodeValueError<E>) -> Self {
        DecodeError::DecodeValueError(e)
    }
}
/// The errors that can occur when decoding the LCM type hash / fingerprint
/// for a message.
#[derive(Debug, PartialEq)]
pub enum DecodeFingerprintError<E> {
    /// The fingerprint value found did not match the expected value for the
    /// message type of interest.
    InvalidFingerprint(u64),
    /// The underlying StreamingReader encountered an error.
    ReaderError(E),
}

impl<E> From<E> for DecodeFingerprintError<E> {
    #[inline]
    fn from(e: E) -> Self {
        DecodeFingerprintError::ReaderError(e)
    }
}

/// The errors that can occur when decoding a value in the body
/// of an LCM message.
#[derive(Debug, PartialEq)]
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
    #[inline]
    fn from(e: E) -> Self {
        DecodeValueError::ReaderError(e)
    }
}

/// The errors that can occur when encoding an LCM message.
#[derive(Debug, PartialEq)]
pub enum EncodeError<E> {
    /// Error encoding fingerprint prior to message body
    EncodeFingerprintError(EncodeFingerprintError<E>),
    /// Error encoding a value in the message body
    EncodeValueError(EncodeValueError<E>),
}

impl<E> From<EncodeFingerprintError<E>> for EncodeError<E> {
    #[inline]
    fn from(e: EncodeFingerprintError<E>) -> Self {
        EncodeError::EncodeFingerprintError(e)
    }
}

impl<E> From<EncodeValueError<E>> for EncodeError<E> {
    #[inline]
    fn from(e: EncodeValueError<E>) -> Self {
        EncodeError::EncodeValueError(e)
    }
}

/// The errors that can occur when encoding the LCM type hash / fingerprint
/// for a message.
#[derive(Debug, PartialEq)]
pub enum EncodeFingerprintError<E> {
    /// The underlying StreamingWriter encountered an error.
    WriterError(E),
}

impl<E> From<E> for EncodeFingerprintError<E> {
    #[inline]
    fn from(e: E) -> Self {
        EncodeFingerprintError::WriterError(e)
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
    #[inline]
    fn from(e: E) -> Self {
        EncodeValueError::WriterError(e)
    }
}
