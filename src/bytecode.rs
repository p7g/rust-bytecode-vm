use crate::opcode::OpCode;

pub struct Bytecode(Vec<u8>);

impl Bytecode {
    pub fn new() -> Bytecode {
        Bytecode(Vec::new())
    }

    pub fn op(mut self, opcode: OpCode) -> Bytecode {
        self.0.push(opcode.into());
        self
    }

    pub fn i64(mut self, n: i64) -> Bytecode {
        self.0.append(&mut n.to_le_bytes().to_vec());
        self
    }

    pub fn f64(mut self, n: f64) -> Bytecode {
        self.0.append(&mut n.to_bits().to_le_bytes().to_vec());
        self
    }

    pub fn usize(mut self, n: usize) -> Bytecode {
        self.0.append(&mut n.to_le_bytes().to_vec());
        self
    }

    pub fn halt(mut self) -> Bytecode {
        self.op(OpCode::Halt)
    }

    pub fn const_int(mut self, n: i64) -> Bytecode {
        self.op(OpCode::ConstInt).i64(n)
    }

    pub fn const_double(mut self, n: f64) -> Bytecode {
        self.op(OpCode::ConstDouble).f64(n)
    }

    pub fn const_null(mut self) -> Bytecode {
        self.op(OpCode::ConstNull)
    }

    pub fn const_true(mut self) -> Bytecode {
        self.op(OpCode::ConstTrue)
    }

    pub fn const_false(mut self) -> Bytecode {
        self.op(OpCode::ConstFalse)
    }

    pub fn const_string(mut self, id: usize) -> Bytecode {
        self.op(OpCode::ConstString).usize(id)
    }

    pub fn add(mut self) -> Bytecode {
        self.op(OpCode::Add)
    }

    pub fn sub(mut self) -> Bytecode {
        self.op(OpCode::Sub)
    }

    pub fn mul(mut self) -> Bytecode {
        self.op(OpCode::Mul)
    }

    pub fn div(mut self) -> Bytecode {
        self.op(OpCode::Div)
    }

    pub fn rem(mut self) -> Bytecode {
        self.op(OpCode::Mod)
    }

    pub fn exp(mut self) -> Bytecode {
        self.op(OpCode::Exp)
    }

    pub fn jump(mut self, ip: usize) -> Bytecode {
        self.op(OpCode::Jump).usize(ip)
    }

    pub fn jump_if_true(mut self, ip: usize) -> Bytecode {
        self.op(OpCode::JumpIfTrue).usize(ip)
    }

    pub fn jump_if_false(mut self, ip: usize) -> Bytecode {
        self.op(OpCode::JumpIfFalse).usize(ip)
    }

    pub fn into<T>(self) -> T
    where
        T: std::convert::From<std::vec::Vec<u8>>,
    {
        self.0.into()
    }
}
