// When adding to this enum, make sure to add to TryFrom<u8>
#[derive(Debug, PartialEq, Clone, Copy)]
#[repr(u8)]
pub enum OpCode {
    Halt = 0,
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
    JumpIfFalse,
    Call,
    Return,
    Pop,
    LoadLocal,
    StoreLocal,
    LoadGlobal,
    DeclareGlobal,
    StoreGlobal,
    NewFunction,
    BindLocal,
    BindUpvalue,
    BindArgument,
    LoadUpvalue,
    StoreUpvalue,
    LoadArgument,
    StoreArgument,
    NewArray,
    NewArrayWithValues,
    ArrayGet,
    ArraySet,
    Equal,
    NotEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseNot,
    Not,
    LeftShift,
    RightShift,
    Neg,
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
            0 => OpCode::Halt,
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
            15 => OpCode::JumpIfFalse,
            16 => OpCode::Call,
            17 => OpCode::Return,
            18 => OpCode::Pop,
            19 => OpCode::LoadLocal,
            20 => OpCode::StoreLocal,
            21 => OpCode::LoadGlobal,
            22 => OpCode::DeclareGlobal,
            23 => OpCode::StoreGlobal,
            24 => OpCode::NewFunction,
            25 => OpCode::BindLocal,
            26 => OpCode::BindUpvalue,
            27 => OpCode::BindArgument,
            28 => OpCode::LoadUpvalue,
            29 => OpCode::StoreUpvalue,
            30 => OpCode::LoadArgument,
            31 => OpCode::StoreArgument,
            32 => OpCode::NewArray,
            33 => OpCode::NewArrayWithValues,
            34 => OpCode::ArrayGet,
            35 => OpCode::ArraySet,
            36 => OpCode::Equal,
            37 => OpCode::NotEqual,
            38 => OpCode::LessThan,
            39 => OpCode::LessThanEqual,
            40 => OpCode::GreaterThan,
            41 => OpCode::GreaterThanEqual,
            42 => OpCode::BitwiseAnd,
            43 => OpCode::BitwiseOr,
            44 => OpCode::BitwiseXor,
            45 => OpCode::BitwiseNot,
            46 => OpCode::Not,
            47 => OpCode::LeftShift,
            48 => OpCode::RightShift,
            49 => OpCode::Neg,
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

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::convert::TryFrom;

    #[test]
    fn test_out_of_bounds() {
        let n: u8 = 255;

        let op: Result<OpCode, String> = OpCode::try_from(n);

        assert_eq!(op, Err("255 is out of bounds for OpCode".to_string()));
    }
}
