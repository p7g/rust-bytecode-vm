use crate::opcode::OpCode;
use std::collections::HashMap;

pub struct Bytecode {
    instructions: Vec<u8>,
    label_addresses: HashMap<&'static str, usize>,
    pending_addresses: HashMap<&'static str, Vec<usize>>,
}

impl Bytecode {
    pub fn new() -> Bytecode {
        Bytecode {
            instructions: Vec::new(),
            label_addresses: HashMap::new(),
            pending_addresses: HashMap::new(),
        }
    }

    pub fn label(mut self, name: &'static str) -> Bytecode {
        let address = self.instructions.len();
        self.label_addresses.insert(name, address);

        if let Some(pending) = self.pending_addresses.get(name) {
            for p in pending.iter() {
                for (i, b) in address.to_le_bytes().iter().enumerate() {
                    self.instructions[p + i] = *b;
                }
            }
        }

        self
    }

    pub fn address_of(mut self, name: &'static str) -> Bytecode {
        if self.label_addresses.contains_key(name) {
            let address = { self.label_addresses.get(name).unwrap().clone() };
            self.usize(address)
        } else {
            let current_address = self.instructions.len();
            if !self.pending_addresses.contains_key(name) {
                self.pending_addresses.insert(name, vec![current_address]);
            } else {
                self.pending_addresses
                    .get_mut(name)
                    .unwrap()
                    .push(current_address);
            }

            self.usize(0)
        }
    }

    pub fn op(mut self, opcode: OpCode) -> Bytecode {
        self.instructions.push(opcode.into());
        self
    }

    pub fn i64(mut self, n: i64) -> Bytecode {
        self.instructions.append(&mut n.to_le_bytes().to_vec());
        self
    }

    pub fn f64(mut self, n: f64) -> Bytecode {
        self.instructions
            .append(&mut n.to_bits().to_le_bytes().to_vec());
        self
    }

    pub fn usize(mut self, n: usize) -> Bytecode {
        self.instructions.append(&mut n.to_le_bytes().to_vec());
        self
    }

    pub fn halt(self) -> Bytecode {
        self.op(OpCode::Halt)
    }

    pub fn const_int(self, n: i64) -> Bytecode {
        self.op(OpCode::ConstInt).i64(n)
    }

    pub fn const_double(self, n: f64) -> Bytecode {
        self.op(OpCode::ConstDouble).f64(n)
    }

    pub fn const_null(self) -> Bytecode {
        self.op(OpCode::ConstNull)
    }

    pub fn const_true(self) -> Bytecode {
        self.op(OpCode::ConstTrue)
    }

    pub fn const_false(self) -> Bytecode {
        self.op(OpCode::ConstFalse)
    }

    pub fn const_string(self, id: usize) -> Bytecode {
        self.op(OpCode::ConstString).usize(id)
    }

    pub fn add(self) -> Bytecode {
        self.op(OpCode::Add)
    }

    pub fn sub(self) -> Bytecode {
        self.op(OpCode::Sub)
    }

    pub fn mul(self) -> Bytecode {
        self.op(OpCode::Mul)
    }

    pub fn div(self) -> Bytecode {
        self.op(OpCode::Div)
    }

    pub fn rem(self) -> Bytecode {
        self.op(OpCode::Mod)
    }

    pub fn exp(self) -> Bytecode {
        self.op(OpCode::Exp)
    }

    pub fn jump(self, ip: usize) -> Bytecode {
        self.op(OpCode::Jump).usize(ip)
    }

    pub fn jump_if_true(self, ip: usize) -> Bytecode {
        self.op(OpCode::JumpIfTrue).usize(ip)
    }

    pub fn jump_if_false(self, ip: usize) -> Bytecode {
        self.op(OpCode::JumpIfFalse).usize(ip)
    }

    pub fn call(self, num_args: usize) -> Bytecode {
        self.op(OpCode::Call).usize(num_args)
    }

    pub fn ret(self) -> Bytecode {
        self.op(OpCode::Return)
    }

    pub fn pop(self) -> Bytecode {
        self.op(OpCode::Pop)
    }

    pub fn load_local(self, id: usize) -> Bytecode {
        self.op(OpCode::LoadLocal).usize(id)
    }

    pub fn store_local(self, id: usize) -> Bytecode {
        self.op(OpCode::StoreLocal).usize(id)
    }

    pub fn into<T>(self) -> T
    where
        T: std::convert::From<std::vec::Vec<u8>>,
    {
        self.instructions.into()
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
    (($builder:expr) jump $dest:ident $($tt:tt)*) => {{
        bytecode! {
            ($builder.op(OpCode::Jump).address_of(stringify!($dest)))
            $($tt)*
        }
    }};
    (($builder:expr) jump $dest:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.jump($dest))
            $($tt)*
        }
    }};
    (($builder:expr) jump_if_false $dest:ident $($tt:tt)*) => {{
        bytecode! {
            ($builder.op(OpCode::JumpIfFalse).address_of(stringify!($dest)))
            $($tt)*
        }
    }};
    (($builder:expr) jump_if_false $dest:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.jump_if_false($dest))
            $($tt)*
        }
    }};
    (($builder:expr) jump_if_true $dest:ident $($tt:tt)*) => {{
        bytecode! {
            ($builder.op(OpCode::JumpIfTrue).address_of(stringify!($dest)))
            $($tt)*
        }
    }};
    (($builder:expr) jump_if_true $dest:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.jump_if_true($dest))
            $($tt)*
        }
    }};
    (($builder:expr) call $num_args:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.call($num_args))
            $($tt)*
        }
    }};
    (($builder:expr) return $($tt:tt)*) => {{
        bytecode! {
            ($builder.ret())
            $($tt)*
        }
    }};
    (($builder:expr) pop $($tt:tt)*) => {{
        bytecode! {
            ($builder.pop())
            $($tt)*
        }
    }};
    (($builder:expr) load_local $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.load_local($id))
            $($tt)*
        }
    }};
    (($builder:expr) store_local $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.store_local($id))
            $($tt)*
        }
    }};
    (($builder:expr) $label:ident : $($tt:tt)*) => {{
        bytecode! {
            ($builder.label(stringify!($label)))
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
