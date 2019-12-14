#[repr(u8)]
enum Tag {
    String,
    Integer,
    Double,
}

impl From<u8> for Tag {
    fn from(tag: u8) -> Self {
        match tag {
            Tag::String as u8 => Tag::String,
        }
    }
}

#[derive(Debug, PartialEq)]
pub(crate) enum Value {
    String(String),
    Integer(i64),
    Double(f64),
}

impl Value {
    fn tag(&self) -> Tag {
        match self {
            Value::String(_) => Tag::String,
            Value::Integer(_) => Tag::Integer,
            Value::Double(_) => Tag::Double,
        }
    }
}

impl Into<Vec<u8>> for Value {
    fn into(self) -> Vec<u8> {
        let tag = self.tag();

        let mut bytes = match self {
            Value::String(s) => s.into_bytes(),
            Value::Integer(i) => i.to_le_bytes().to_vec(),
            Value::Double(d) => d.to_bits().to_le_bytes().to_vec(),
        };

        bytes.insert(0, tag as u8);

        bytes
    }
}

impl<'a> From<std::vec::Drain<'a, u8>> for Value {
    fn from(bytes: std::vec::Drain<'a, u8>) -> Self {
        let tag = bytes.next().unwrap();

        match tag {

        }
    }
}
