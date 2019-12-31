#![allow(non_camel_case_types)]
#![allow(dead_code)]

extern crate rust_lcm_codec;
use rust_lcm_codec::*;

// struct point2d {
//     double x;
//     double y;
// }
const POINT2D_SCHEMA_HASH: u64 = 0xff00ff00ff00ff00;

struct Point2d();
struct Point2d_Write_READY<'a, W: StreamingWriter> {
    writer: &'a mut W,
}
struct Point2d_Write_y<'a, W: StreamingWriter> {
    writer: &'a mut W,
}
struct Point2d_Write_DONE<'a, W: StreamingWriter> {
    writer: &'a mut W,
}

impl Point2d {
    fn begin_write<'a, W: StreamingWriter>(
        writer: &'a mut W,
    ) -> Result<Point2d_Write_READY<W>, W::Error> {
        writer.write_bytes(&POINT2D_SCHEMA_HASH.to_be_bytes())?;
        Ok(Point2d_Write_READY { writer })
    }
}

impl<'a, W: StreamingWriter> Point2d_Write_READY<'a, W> {
    fn write_x(self, x: f64) -> Result<Point2d_Write_y<'a, W>, W::Error> {
        self.writer.write_bytes(&x.to_be_bytes())?;
        Ok(Point2d_Write_y {
            writer: self.writer,
        })
    }
}

impl<'a, W: StreamingWriter> Point2d_Write_y<'a, W> {
    fn write_y(self, y: f64) -> Result<Point2d_Write_DONE<'a, W>, W::Error> {
        self.writer.write_bytes(&y.to_be_bytes())?;
        Ok(Point2d_Write_DONE {
            writer: self.writer,
        })
    }
}

#[test]
fn test_point2d_write() {
    let mut buf = [0u8; 256];
    let mut w = BufferWriter::new(&mut buf);

    let pw = Point2d::begin_write(&mut w).unwrap();
    let pw = pw.write_x(2.1).unwrap();
    let _pw = pw.write_y(3.2).unwrap();
}

// struct with_array {
//     int32_t count;
//     int32_t vals[count][2];
//     int32_t post;
// }

// Simplified single dimensional case
// struct with_array {
//     int32_t count;
//     int32_t vals[count];
//     int32_t post;
// }
const WITHARRAY_SCHEMA_HASH: u64 = 0xaa00aa00aa00aa00;

struct WithArray();
struct WithArray_Write_READY<'a, W: StreamingWriter> {
    writer: &'a mut W,
}
struct WithArray_Write_vals<'a, W: StreamingWriter> {
    writer: &'a mut W,
    expected_count: usize,
    current_count: usize,
}
struct WithArray_Write_post<'a, W: StreamingWriter> {
    writer: &'a mut W,
}
struct WithArray_Write_DONE<'a, W: StreamingWriter> {
    writer: &'a mut W,
}

impl WithArray {
    fn begin_write<'a, W: StreamingWriter>(
        writer: &'a mut W,
    ) -> Result<WithArray_Write_READY<W>, W::Error> {
        writer.write_bytes(&WITHARRAY_SCHEMA_HASH.to_be_bytes())?;
        Ok(WithArray_Write_READY { writer })
    }
}

impl<'a, W: StreamingWriter> WithArray_Write_READY<'a, W> {
    fn write_count(self, count: i32) -> Result<WithArray_Write_vals<'a, W>, W::Error> {
        self.writer.write_bytes(&count.to_be_bytes())?;
        Ok(WithArray_Write_vals {
            writer: self.writer,
            expected_count: count as _,
            current_count: 0,
        })
    }
}

impl<'a, W: StreamingWriter> Iterator for WithArray_Write_vals<'a, W> {
    type Item = WithArray_Write_vals_Item<'a, W>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.current_count < self.expected_count {
            unsafe {
                Some(WithArray_Write_vals_Item {
                    parent: core::mem::transmute(self),
                })
            }
        } else {
            None
        }
    }
}

impl<'a, W: StreamingWriter> WithArray_Write_vals<'a, W> {
    fn done(self) -> Result<WithArray_Write_post<'a, W>, EncodeValueError<W::Error>> {
        if self.current_count < self.expected_count {
            return Err(EncodeValueError::ArrayLengthMismatch(
                "vals len mismatch when done called",
            ));
        }
        Ok(WithArray_Write_post {
            writer: self.writer,
        })
    }
}

pub struct WithArray_Write_vals_Item<'a, W: StreamingWriter> {
    parent: &'a mut WithArray_Write_vals<'a, W>,
}

impl<'a, W: StreamingWriter> WithArray_Write_vals_Item<'a, W> {
    // for the primitive case, directly write
    fn write_item(self, val: &u8) -> Result<(), EncodeValueError<W::Error>> {
        if self.parent.current_count >= self.parent.expected_count {
            return Err(EncodeValueError::ArrayLengthMismatch(
                "vals item len mismatch",
            ));
        }
        self.parent.writer.write_bytes(&(*val).to_be_bytes())?;
        self.parent.current_count += 1;
        Ok(())
    }
}

impl<'a, W: StreamingWriter> WithArray_Write_post<'a, W> {
    fn write_post(self, post: i32) -> Result<WithArray_Write_DONE<'a, W>, W::Error> {
        self.writer.write_bytes(&post.to_be_bytes())?;
        Ok(WithArray_Write_DONE {
            writer: self.writer,
        })
    }
}

#[test]
fn test_witharray_write() {
    let mut buf = [0u8; 256];
    let mut w = BufferWriter::new(&mut buf);

    let aw = WithArray::begin_write(&mut w).unwrap();
    let mut aw = aw.write_count(10).unwrap();
    for (i, item_writer) in (&mut aw).enumerate() {
        let i = i as u8;
        item_writer.write_item(&i).unwrap();
    }
    let aw = aw.done().unwrap();
    let _aw = aw.write_post(2).unwrap();
}

//#[test]
//fn test_witharray_write() {
//    let mut buf = [0u8; 256];
//    let mut w = BufferWriter::new(&mut buf);
//
//    let aw = WithArray::begin_write(&mut w).unwrap();
//    let mut aw = aw.write_count(10).unwrap();
//    for i in 0..10 {
//        for j in 0..2 {
//            aw = aw.write_vals_item(i + j).unwrap();
//        }
//    }
//    let aw = aw.write_vals_done().unwrap();
//    let _aw = aw.write_post(2).unwrap();
//}

// struct nested {
//     int32_t a;
//     point2d point;
//     int32_t b;
// }

const NESTED_SCHEMA_HASH: u64 = 0xbb00bb00bb00bb00;

struct Nested();

struct Nested_Write_READY<'a, W: StreamingWriter> {
    writer: &'a mut W,
}

struct Nested_Write_point<'a, W: StreamingWriter> {
    writer: &'a mut W,
}

struct Nested_Write_b<'a, W: StreamingWriter> {
    writer: &'a mut W,
}

struct Nested_Write_DONE<'a, W: StreamingWriter> {
    writer: &'a mut W,
}

impl Nested {
    fn begin_write<'a, W: StreamingWriter>(
        writer: &'a mut W,
    ) -> Result<Nested_Write_READY<W>, W::Error> {
        writer.write_bytes(&NESTED_SCHEMA_HASH.to_be_bytes())?;
        Ok(Nested_Write_READY { writer })
    }
}

impl<'a, W: StreamingWriter> Nested_Write_READY<'a, W> {
    fn write_a(self, a: i32) -> Result<Nested_Write_point<'a, W>, W::Error> {
        self.writer.write_bytes(&a.to_be_bytes())?;
        Ok(Nested_Write_point {
            writer: self.writer,
        })
    }
}

impl<'a, W: StreamingWriter> Nested_Write_point<'a, W> {
    fn write_point<
        F: Fn(Point2d_Write_READY<'a, W>) -> Result<Point2d_Write_DONE<'a, W>, W::Error>,
    >(
        self,
        f: F,
    ) -> Result<Nested_Write_b<'a, W>, W::Error> {
        let ready = Point2d_Write_READY {
            writer: self.writer,
        };
        let done = f(ready)?;
        Ok(Nested_Write_b {
            writer: done.writer,
        })
    }
}

impl<'a, W: StreamingWriter> Nested_Write_b<'a, W> {
    fn write_b(self, b: i32) -> Result<Nested_Write_DONE<'a, W>, W::Error> {
        self.writer.write_bytes(&b.to_be_bytes())?;
        Ok(Nested_Write_DONE {
            writer: self.writer,
        })
    }
}

#[test]
fn test_nested_write() {
    let mut buf = [0u8; 256];
    let mut w = BufferWriter::new(&mut buf);

    let nw = Nested::begin_write(&mut w).unwrap();
    let nw = nw.write_a(1).unwrap();
    let nw = nw
        .write_point(|pw| Ok(pw.write_x(10.0)?.write_y(11.0)?))
        .unwrap();
    let _nw = nw.write_b(2).unwrap();
}
