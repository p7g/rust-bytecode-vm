#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Noop,
    ConstInt,
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        self as u8
    }
}

impl std::convert::TryFrom<u8> for OpCode {
    type Error = String;

    fn try_from(intval: u8) -> Result<OpCode, Self::Error> {
        Ok(match intval {
            0 => OpCode::Noop,
            1 => OpCode::ConstInt,
            _ => {
                return Err(format!("{} is out of bounds for OpCode", intval))
            },
        })
    }
}
