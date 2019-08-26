// When adding to this enum, make sure to add to TryFrom<u8>
#[derive(Debug)]
#[repr(u8)]
pub enum OpCode {
    Noop = 0,
    ConstInt,
    ConstDouble,
    ConstString,
    ConstTrue,
    ConstFalse,
    ConstNull,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Exp,
    Jump,
    JumpIfTrue,
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
            2 => OpCode::ConstDouble,
            3 => OpCode::ConstString,
            4 => OpCode::ConstTrue,
            5 => OpCode::ConstFalse,
            6 => OpCode::ConstNull,
            7 => OpCode::Add,
            8 => OpCode::Sub,
            9 => OpCode::Mul,
            10 => OpCode::Div,
            11 => OpCode::Mod,
            12 => OpCode::Exp,
            13 => OpCode::Jump,
            14 => OpCode::JumpIfTrue,
            _ => return Err(format!("{} is out of bounds for OpCode", intval)),
        })
    }
}

impl std::convert::TryFrom<&u8> for OpCode {
    type Error = String;

    fn try_from(intval: &u8) -> Result<OpCode, Self::Error> {
        OpCode::try_from(*intval)
    }
}
