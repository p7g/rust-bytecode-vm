use crate::opcode::OpCode;
use std::collections::HashMap;

pub struct Bytecode {
    pub instructions: Vec<u8>,
    label_addresses: HashMap<&'static str, usize>,
    pending_addresses: HashMap<&'static str, Vec<usize>>,
    // FIXME: dedupe this stuff
    label_addresses_auto: Vec<Option<usize>>,
    pending_addresses_auto: HashMap<usize, Vec<usize>>,
}

impl Bytecode {
    pub fn new() -> Bytecode {
        Bytecode {
            instructions: Vec::new(),
            label_addresses: HashMap::new(),
            pending_addresses: HashMap::new(),
            label_addresses_auto: Vec::new(),
            pending_addresses_auto: HashMap::new(),
        }
    }

    pub fn new_label(&mut self) -> usize {
        self.label_addresses_auto.push(None);
        self.label_addresses_auto.len() - 1
    }

    pub fn mark_label(&mut self, idx: usize) {
        let address = self.instructions.len();
        self.label_addresses_auto[idx] = Some(address);

        if let Some(pending) = self.pending_addresses_auto.get(&idx) {
            for p in pending.iter() {
                for (i, b) in address.to_le_bytes().iter().enumerate() {
                    self.instructions[p + i] = *b;
                }
            }
        }
    }

    pub fn address_of_auto(&mut self, idx: usize) -> &mut Bytecode {
        let maybe_addr = self.label_addresses_auto[idx];
        if let Some(address) = maybe_addr {
            self.usize(address)
        } else {
            let current_address = self.instructions.len();
            self.pending_addresses_auto
                .entry(idx)
                .and_modify(|v| v.push(current_address))
                .or_insert_with(|| vec![current_address]);

            self.usize(0)
        }
    }

    pub fn label(&mut self, name: &'static str) -> &mut Bytecode {
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

    pub fn address_of(&mut self, name: &'static str) -> &mut Bytecode {
        if self.label_addresses.contains_key(name) {
            let address = { *self.label_addresses.get(name).unwrap() };
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

    pub fn op(&mut self, opcode: OpCode) -> &mut Bytecode {
        self.instructions.push(opcode.into());
        self
    }

    pub fn i64(&mut self, n: i64) -> &mut Bytecode {
        self.instructions.append(&mut n.to_le_bytes().to_vec());
        self
    }

    pub fn f64(&mut self, n: f64) -> &mut Bytecode {
        self.instructions
            .append(&mut n.to_bits().to_le_bytes().to_vec());
        self
    }

    pub fn usize(&mut self, n: usize) -> &mut Bytecode {
        self.instructions.append(&mut n.to_le_bytes().to_vec());
        self
    }

    pub fn halt(&mut self) -> &mut Bytecode {
        self.op(OpCode::Halt)
    }

    pub fn const_int(&mut self, n: i64) -> &mut Bytecode {
        self.op(OpCode::ConstInt).i64(n)
    }

    pub fn const_double(&mut self, n: f64) -> &mut Bytecode {
        self.op(OpCode::ConstDouble).f64(n)
    }

    pub fn const_null(&mut self) -> &mut Bytecode {
        self.op(OpCode::ConstNull)
    }

    pub fn const_true(&mut self) -> &mut Bytecode {
        self.op(OpCode::ConstTrue)
    }

    pub fn const_false(&mut self) -> &mut Bytecode {
        self.op(OpCode::ConstFalse)
    }

    pub fn const_string(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::ConstString).usize(id)
    }

    pub fn add(&mut self) -> &mut Bytecode {
        self.op(OpCode::Add)
    }

    pub fn sub(&mut self) -> &mut Bytecode {
        self.op(OpCode::Sub)
    }

    pub fn mul(&mut self) -> &mut Bytecode {
        self.op(OpCode::Mul)
    }

    pub fn div(&mut self) -> &mut Bytecode {
        self.op(OpCode::Div)
    }

    pub fn rem(&mut self) -> &mut Bytecode {
        self.op(OpCode::Mod)
    }

    pub fn exp(&mut self) -> &mut Bytecode {
        self.op(OpCode::Exp)
    }

    pub fn jump(&mut self, ip: usize) -> &mut Bytecode {
        self.op(OpCode::Jump).usize(ip)
    }

    pub fn jump_if_true(&mut self, ip: usize) -> &mut Bytecode {
        self.op(OpCode::JumpIfTrue).usize(ip)
    }

    pub fn jump_if_false(&mut self, ip: usize) -> &mut Bytecode {
        self.op(OpCode::JumpIfFalse).usize(ip)
    }

    pub fn call(&mut self, num_args: usize) -> &mut Bytecode {
        self.op(OpCode::Call).usize(num_args)
    }

    pub fn ret(&mut self) -> &mut Bytecode {
        self.op(OpCode::Return)
    }

    pub fn pop(&mut self) -> &mut Bytecode {
        self.op(OpCode::Pop)
    }

    pub fn load_local(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::LoadLocal).usize(id)
    }

    pub fn store_local(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::StoreLocal).usize(id)
    }

    pub fn load_global(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::LoadGlobal).usize(id)
    }

    pub fn declare_global(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::DeclareGlobal).usize(id)
    }

    pub fn store_global(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::StoreGlobal).usize(id)
    }

    pub fn new_function(&mut self, arity: usize, address: usize) -> &mut Bytecode {
        self.op(OpCode::NewFunction).usize(arity).usize(address)
    }

    pub fn bind_local(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::BindLocal).usize(id)
    }

    pub fn bind_upvalue(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::BindUpvalue).usize(id)
    }

    pub fn bind_argument(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::BindArgument).usize(id)
    }

    pub fn load_upvalue(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::LoadUpvalue).usize(id)
    }

    pub fn store_upvalue(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::StoreUpvalue).usize(id)
    }

    pub fn load_argument(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::LoadArgument).usize(id)
    }

    pub fn store_argument(&mut self, id: usize) -> &mut Bytecode {
        self.op(OpCode::StoreArgument).usize(id)
    }

    pub fn new_array(&mut self, len: usize) -> &mut Bytecode {
        self.op(OpCode::NewArray).usize(len)
    }

    pub fn new_array_with_values(&mut self, len: usize) -> &mut Bytecode {
        self.op(OpCode::NewArrayWithValues).usize(len)
    }

    pub fn array_get(&mut self) -> &mut Bytecode {
        self.op(OpCode::ArrayGet)
    }

    pub fn array_set(&mut self) -> &mut Bytecode {
        self.op(OpCode::ArraySet)
    }

    pub fn equal(&mut self) -> &mut Bytecode {
        self.op(OpCode::Equal)
    }

    pub fn not_equal(&mut self) -> &mut Bytecode {
        self.op(OpCode::NotEqual)
    }

    pub fn less_than(&mut self) -> &mut Bytecode {
        self.op(OpCode::LessThan)
    }

    pub fn less_than_equal(&mut self) -> &mut Bytecode {
        self.op(OpCode::LessThanEqual)
    }

    pub fn greater_than(&mut self) -> &mut Bytecode {
        self.op(OpCode::GreaterThan)
    }

    pub fn greater_than_equal(&mut self) -> &mut Bytecode {
        self.op(OpCode::GreaterThanEqual)
    }

    pub fn bitwise_and(&mut self) -> &mut Bytecode {
        self.op(OpCode::BitwiseAnd)
    }

    pub fn bitwise_or(&mut self) -> &mut Bytecode {
        self.op(OpCode::BitwiseOr)
    }

    pub fn bitwise_xor(&mut self) -> &mut Bytecode {
        self.op(OpCode::BitwiseXor)
    }

    pub fn bitwise_not(&mut self) -> &mut Bytecode {
        self.op(OpCode::BitwiseNot)
    }

    pub fn not(&mut self) -> &mut Bytecode {
        self.op(OpCode::Not)
    }

    pub fn shift_left(&mut self) -> &mut Bytecode {
        self.op(OpCode::LeftShift)
    }

    pub fn shift_right(&mut self) -> &mut Bytecode {
        self.op(OpCode::RightShift)
    }

    pub fn neg(&mut self) -> &mut Bytecode {
        self.op(OpCode::Neg)
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
    (($builder:expr) load_global ($id:expr) $($tt:tt)*) => {{
        bytecode! {
            ($builder.load_global($id))
            $($tt)*
        }
    }};
    (($builder:expr) declare_global ($id:expr) $($tt:tt)*) => {{
        bytecode! {
            ($builder.declare_global($id))
            $($tt)*
        }
    }};
    (($builder:expr) store_global ($id:expr) $($tt:tt)*) => {{
        bytecode! {
            ($builder.store_global($id))
            $($tt)*
        }
    }};
    (($builder:expr) load_global $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.load_global($id))
            $($tt)*
        }
    }};
    (($builder:expr) declare_global $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.declare_global($id))
            $($tt)*
        }
    }};
    (($builder:expr) store_global $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.store_global($id))
            $($tt)*
        }
    }};
    (($builder:expr) new_function $arity:tt $label:ident $($tt:tt)*) => {{
        bytecode! {
            ($builder.op(OpCode::NewFunction).usize($arity).address_of(stringify!($label)))
            $($tt)*
        }
    }};
    (($builder:expr) new_function $arity:tt $address:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.new_function($arity, $address))
            $($tt)*
        }
    }};
    (($builder:expr) bind_local $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.bind_local($id))
            $($tt)*
        }
    }};
    (($builder:expr) bind_upvalue $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.bind_upvalue($id))
            $($tt)*
        }
    }};
    (($builder:expr) bind_argument $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.bind_argument($id))
            $($tt)*
        }
    }};
    (($builder:expr) load_upvalue $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.load_upvalue($id))
            $($tt)*
        }
    }};
    (($builder:expr) store_upvalue $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.store_upvalue($id))
            $($tt)*
        }
    }};
    (($builder:expr) load_argument $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.load_argument($id))
            $($tt)*
        }
    }};
    (($builder:expr) store_argument $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.store_argument($id))
            $($tt)*
        }
    }};
    (($builder:expr) new_array $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.new_array($id))
            $($tt)*
        }
    }};
    (($builder:expr) new_array_with_values $id:tt $($tt:tt)*) => {{
        bytecode! {
            ($builder.new_array_with_values($id))
            $($tt)*
        }
    }};
    (($builder:expr) array_get $($tt:tt)*) => {{
        bytecode! {
            ($builder.array_get())
            $($tt)*
        }
    }};
    (($builder:expr) array_set $($tt:tt)*) => {{
        bytecode! {
            ($builder.array_set())
            $($tt)*
        }
    }};
    (($builder:expr) equal $($tt:tt)*) => {{
        bytecode! {
            ($builder.equal())
            $($tt)*
        }
    }};
    (($builder:expr) not_equal $($tt:tt)*) => {{
        bytecode! {
            ($builder.not_equal())
            $($tt)*
        }
    }};
    (($builder:expr) less_than $($tt:tt)*) => {{
        bytecode! {
            ($builder.less_than())
            $($tt)*
        }
    }};
    (($builder:expr) less_than_equal $($tt:tt)*) => {{
        bytecode! {
            ($builder.less_than_equal())
            $($tt)*
        }
    }};
    (($builder:expr) greater_than $($tt:tt)*) => {{
        bytecode! {
            ($builder.greater_than())
            $($tt)*
        }
    }};
    (($builder:expr) greater_than_equal $($tt:tt)*) => {{
        bytecode! {
            ($builder.greater_than_equal())
            $($tt)*
        }
    }};
    (($builder:expr) bitwise_and $($tt:tt)*) => {{
        bytecode! {
            ($builder.bitwise_and())
            $($tt)*
        }
    }};
    (($builder:expr) bitwise_or $($tt:tt)*) => {{
        bytecode! {
            ($builder.bitwise_or())
            $($tt)*
        }
    }};
    (($builder:expr) bitwise_xor $($tt:tt)*) => {{
        bytecode! {
            ($builder.bitwise_xor())
            $($tt)*
        }
    }};
    (($builder:expr) bitwise_not $($tt:tt)*) => {{
        bytecode! {
            ($builder.bitwise_not())
            $($tt)*
        }
    }};
    (($builder:expr) not $($tt:tt)*) => {{
        bytecode! {
            ($builder.not())
            $($tt)*
        }
    }};
    (($builder:expr) shift_left $($tt:tt)*) => {{
        bytecode! {
            ($builder.shift_left())
            $($tt)*
        }
    }};
    (($builder:expr) shift_right $($tt:tt)*) => {{
        bytecode! {
            ($builder.shift_right())
            $($tt)*
        }
    }};
    (($builder:expr) neg $($tt:tt)*) => {{
        bytecode! {
            ($builder.neg())
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
