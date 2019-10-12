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

impl std::convert::From<u8> for OpCode {
    fn from(val: u8) -> Self {
        unsafe { std::mem::transmute(val) }
    }
}
