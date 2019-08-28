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

macro_rules! bytecode {
    (($builder:expr) add $($tt:tt)*) => {{
        bytecode! {
            ($builder.add())
            $($tt)*
        }
    }};
    (($builder:expr) sub $($tt:tt)*) => {{
        bytecode! {
            ($builder.sub())
            $($tt)*
        }
    }};
    (($builder:expr) mul $($tt:tt)*) => {{
        bytecode! {
            ($builder.mul())
            $($tt)*
        }
    }};
    (($builder:expr) div $($tt:tt)*) => {{
        bytecode! {
            ($builder.div())
            $($tt)*
        }
    }};
    (($builder:expr) exp $($tt:tt)*) => {{
        bytecode! {
            ($builder.exp())
            $($tt)*
        }
    }};
    (($builder:expr) mod $($tt:tt)*) => {{
        bytecode! {
            ($builder.rem())
            $($tt)*
        }
    }};
    (($builder:expr) const_int $expr:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_int($expr))
            $($tt)*
        }
    }};
    (($builder:expr) const_double $expr:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_double($expr))
            $($tt)*
        }
    }};
    (($builder:expr) const_string ($agent:tt) $str:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_string($agent.intern_string($str)))
            $($tt)*
        }
    }};
    (($builder:expr) const_string $expr:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_string($expr))
            $($tt)*
        }
    }};
    (($builder:expr) const_null $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_null())
            $($tt)*
        }
    }};
    (($builder:expr) const_true $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_true())
            $($tt)*
        }
    }};
    (($builder:expr) const_false $($tt:tt)*) => {{
        bytecode! {
            ($builder.const_false())
            $($tt)*
        }
    }};
    (($builder:expr) halt $($tt:tt)*) => {{
        bytecode! {
            ($builder.halt())
            $($tt)*
        }
    }};
    (($builder:expr) jump $dest:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.jump($dest))
            $($tt)*
        }
    }};
    (($builder:expr) jump_if_false $dest:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.jump_if_false($dest))
            $($tt)*
        }
    }};
    (($builder:expr) jump_if_true $dest:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.jump_if_true($dest))
            $($tt)*
        }
    }};
    (($builder:expr) $bad_thing:tt) => {{
        compile_error!(stringify!(Unexpected: $bad_thing))
    }};
    (($builder:expr)) => {{
        $builder
    }};
    ($($tt:tt)*) => {{
        bytecode! {
            (Bytecode::new())
            $($tt)*
        }
    }};
}
