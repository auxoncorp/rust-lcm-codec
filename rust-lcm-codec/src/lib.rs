
pub trait StreamingWriter {
    type Error;
    // TODO: return size written?
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error>;
    fn flush() -> Result<(), Self::Error>;
}

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
    type Error = ();
    fn write_bytes(&mut self, bytes: &[u8]) -> Result<(), Self::Error> {
        let len = bytes.len();
        self.buffer[self.cursor..self.cursor + len].copy_from_slice(bytes);
        self.cursor += len;
        Ok(())
    }

    fn flush() -> Result<(), Self::Error> {
        Ok(())
    }
}
