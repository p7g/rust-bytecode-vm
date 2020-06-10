use crate::opcode::OpCode;
use std::collections::HashMap;

enum Argument {
    Usize(usize),
    I64(i64),
    F64(f64),
    AddressOf(usize),
}

enum Instruction {
    OpCode(OpCode, Vec<Argument>),
    Label(usize),
}

struct Bytecode2 {
    instructions: Vec<Instruction>,
    label_count: usize,
    label_indices: HashMap<&'static str, usize>,
}

impl Bytecode2 {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            label_count: 0,
            label_indices: HashMap::new(),
        }
    }

    fn current_arguments_mut(&mut self) -> &mut Vec<Argument> {
        if let Some(Instruction::OpCode(_, arguments)) = self.instructions.last_mut() {
            arguments
        } else {
            unreachable!("Trying to get arguments of non-opcode instruction");
        }
    }

    fn named_label_index(&mut self, name: &'static str) -> usize {
        let map_len = self.label_count;
        self.label_count += 1;
        *self.label_indices.entry(name).or_insert(map_len)
    }

    fn next_label_index(&mut self) -> usize {
        let idx = self.label_count;
        self.label_count += 1;
        idx
    }

    pub fn new_label(&mut self) -> usize {
        self.next_label_index()
    }

    pub fn label(&mut self, name: &'static str) -> &mut Self {
        let index = self.named_label_index(name);
        self.instructions.push(Instruction::Label(index));
        self
    }

    pub fn address_of(&mut self, name: &'static str) -> &mut Self {
        let index = self.named_label_index(name);
        self.current_arguments_mut()
            .push(Argument::AddressOf(index));
        self
    }

    pub fn op(&mut self, opcode: OpCode) -> &mut Self {
        self.instructions
            .push(Instruction::OpCode(opcode, Vec::new()));
        self
    }

    pub fn usize(&mut self, value: usize) -> &mut Self {
        self.current_arguments_mut().push(Argument::Usize(value));
        self
    }

    pub fn i64(&mut self, n: i64) -> &mut Self {
        self.current_arguments_mut().push(Argument::I64(n));
        self
    }

    pub fn f64(&mut self, n: f64) -> &mut Self {
        self.current_arguments_mut().push(Argument::F64(n));
        self
    }

    pub fn halt(&mut self) -> &mut Self {
        self.op(OpCode::Halt)
    }

    pub fn const_int(&mut self, n: i64) -> &mut Self {
        self.op(OpCode::ConstInt).i64(n)
    }

    pub fn const_double(&mut self, n: f64) -> &mut Self {
        self.op(OpCode::ConstDouble).f64(n)
    }

    pub fn const_null(&mut self) -> &mut Self {
        self.op(OpCode::ConstNull)
    }

    pub fn const_true(&mut self) -> &mut Self {
        self.op(OpCode::ConstTrue)
    }

    pub fn const_false(&mut self) -> &mut Self {
        self.op(OpCode::ConstFalse)
    }

    pub fn const_string(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::ConstString).usize(id)
    }

    pub fn add(&mut self) -> &mut Self {
        self.op(OpCode::Add)
    }

    pub fn sub(&mut self) -> &mut Self {
        self.op(OpCode::Sub)
    }

    pub fn mul(&mut self) -> &mut Self {
        self.op(OpCode::Mul)
    }

    pub fn div(&mut self) -> &mut Self {
        self.op(OpCode::Div)
    }

    pub fn rem(&mut self) -> &mut Self {
        self.op(OpCode::Mod)
    }

    pub fn exp(&mut self) -> &mut Self {
        self.op(OpCode::Exp)
    }

    pub fn jump(&mut self, ip: usize) -> &mut Self {
        self.op(OpCode::Jump).usize(ip)
    }

    pub fn jump_if_true(&mut self, ip: usize) -> &mut Self {
        self.op(OpCode::JumpIfTrue).usize(ip)
    }

    pub fn jump_if_false(&mut self, ip: usize) -> &mut Self {
        self.op(OpCode::JumpIfFalse).usize(ip)
    }

    pub fn call(&mut self, num_args: usize) -> &mut Self {
        self.op(OpCode::Call).usize(num_args)
    }

    pub fn ret(&mut self) -> &mut Self {
        self.op(OpCode::Return)
    }

    pub fn pop(&mut self) -> &mut Self {
        self.op(OpCode::Pop)
    }

    pub fn load_local(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::LoadLocal).usize(id)
    }

    pub fn store_local(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::StoreLocal).usize(id)
    }

    pub fn load_global(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::LoadGlobal).usize(id)
    }

    pub fn declare_global(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::DeclareGlobal).usize(id)
    }

    pub fn store_global(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::StoreGlobal).usize(id)
    }

    pub fn new_function(&mut self, arity: usize, address: usize) -> &mut Self {
        self.op(OpCode::NewFunction).usize(arity).usize(address)
    }

    pub fn bind_local(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::BindLocal).usize(id)
    }

    pub fn bind_upvalue(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::BindUpvalue).usize(id)
    }

    pub fn bind_argument(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::BindArgument).usize(id)
    }

    pub fn load_upvalue(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::LoadUpvalue).usize(id)
    }

    pub fn store_upvalue(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::StoreUpvalue).usize(id)
    }

    pub fn load_argument(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::LoadArgument).usize(id)
    }

    pub fn store_argument(&mut self, id: usize) -> &mut Self {
        self.op(OpCode::StoreArgument).usize(id)
    }

    pub fn load_from_module(&mut self, module_name: usize, export_name: usize) -> &mut Self {
        self.op(OpCode::LoadFromModule)
            .usize(module_name)
            .usize(export_name)
    }

    pub fn new_array(&mut self, len: usize) -> &mut Self {
        self.op(OpCode::NewArray).usize(len)
    }

    pub fn new_array_with_values(&mut self, len: usize) -> &mut Self {
        self.op(OpCode::NewArrayWithValues).usize(len)
    }

    pub fn array_get(&mut self) -> &mut Self {
        self.op(OpCode::ArrayGet)
    }

    pub fn array_set(&mut self) -> &mut Self {
        self.op(OpCode::ArraySet)
    }

    pub fn equal(&mut self) -> &mut Self {
        self.op(OpCode::Equal)
    }

    pub fn not_equal(&mut self) -> &mut Self {
        self.op(OpCode::NotEqual)
    }

    pub fn less_than(&mut self) -> &mut Self {
        self.op(OpCode::LessThan)
    }

    pub fn less_than_equal(&mut self) -> &mut Self {
        self.op(OpCode::LessThanEqual)
    }

    pub fn greater_than(&mut self) -> &mut Self {
        self.op(OpCode::GreaterThan)
    }

    pub fn greater_than_equal(&mut self) -> &mut Self {
        self.op(OpCode::GreaterThanEqual)
    }

    pub fn bitwise_and(&mut self) -> &mut Self {
        self.op(OpCode::BitwiseAnd)
    }

    pub fn bitwise_or(&mut self) -> &mut Self {
        self.op(OpCode::BitwiseOr)
    }

    pub fn bitwise_xor(&mut self) -> &mut Self {
        self.op(OpCode::BitwiseXor)
    }

    pub fn bitwise_not(&mut self) -> &mut Self {
        self.op(OpCode::BitwiseNot)
    }

    pub fn not(&mut self) -> &mut Self {
        self.op(OpCode::Not)
    }

    pub fn shift_left(&mut self) -> &mut Self {
        self.op(OpCode::LeftShift)
    }

    pub fn shift_right(&mut self) -> &mut Self {
        self.op(OpCode::RightShift)
    }

    pub fn neg(&mut self) -> &mut Self {
        self.op(OpCode::Neg)
    }

    pub fn init_module(&mut self, name: usize) -> &mut Self {
        self.op(OpCode::InitModule).usize(name)
    }

    pub fn end_module(&mut self) -> &mut Self {
        self.op(OpCode::EndModule)
    }

    pub fn dup(&mut self) -> &mut Self {
        self.op(OpCode::Dup)
    }

    pub fn allocate_locals(&mut self, count: usize) -> &mut Self {
        self.op(OpCode::AllocateLocals).usize(count)
    }
}

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

    pub fn position(&self) -> usize {
        self.instructions.len() - 1
    }

    pub fn update_usize(&mut self, position: usize, new_value: usize) {
        let bytes = usize::to_le_bytes(new_value);

        self.instructions[position..(position + std::mem::size_of::<usize>())]
            .clone_from_slice(&bytes);
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

    pub fn into<T>(self) -> T
    where
        T: std::convert::From<std::vec::Vec<u8>>,
    {
        self.instructions.into()
    }
}
