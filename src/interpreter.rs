use std::cell::RefCell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::{Add, Deref, Div, Mul, Rem, Sub};
use std::rc::Rc;

use crate::agent::Agent;
use crate::compiler::disassemble::disassemble;
use crate::debuginfo::DebugInfo;
use crate::module::Module;
use crate::opcode::OpCode;
use crate::value::{FunctionValue, Upvalue, Value};

macro_rules! print_stack {
    ($stack:expr) => {{
        print!("[");
        for (i, v) in $stack.iter().enumerate() {
            print!("{:?}", v);
            if i < $stack.len() - 1 {
                print!(", ");
            }
        }
        println!("]");
    }};
}

#[derive(Debug)]
struct Frame {
    prev_ip: usize,
    prev_bp: usize,
    num_args: usize,
    module_id: usize,
    current_function: Option<usize>,
}

pub struct Interpreter<'a> {
    pub agent: &'a mut Agent,
    intrinsics: HashMap<usize, Value>,
    modules: Vec<Module>,
    current_module: Option<Module>,
    call_stack: Vec<Frame>,
    stack: Vec<Value>,
    ip: usize,
    bp: usize,
    sp: usize,
    debuginfo: Option<&'a DebugInfo>,
}

impl<'a> Interpreter<'a> {
    pub fn new(agent: &'a mut Agent) -> Interpreter<'a> {
        Interpreter::with_intrinsics(agent, HashMap::new())
    }

    pub fn with_intrinsics(
        agent: &'a mut Agent,
        intrinsics: HashMap<usize, Value>,
    ) -> Interpreter<'a> {
        Interpreter {
            intrinsics,
            modules: Vec::with_capacity(agent.num_modules()),
            agent,
            current_module: None,
            call_stack: Vec::new(),
            stack: Vec::new(),
            ip: 0,
            bp: 0,
            sp: 0,
            debuginfo: None,
        }
    }

    pub(crate) fn set_debuginfo(&mut self, debuginfo: &'a DebugInfo) {
        self.debuginfo.replace(debuginfo);
    }

    fn push(&mut self, expr: Value) {
        self.sp += 1;
        self.stack.push(expr);
    }

    fn pop(&mut self) -> Result<Value, &'static str> {
        self.sp -= 1;
        self.stack.pop().ok_or("Stack underflow")
    }

    fn drop_n(&mut self, count: usize) {
        self.sp -= count;
        self.stack.truncate(self.sp);
    }

    fn top_n(&self, count: usize) -> &[Value] {
        &self.stack[self.sp - count..]
    }

    #[inline]
    fn next_instruction(&mut self, code: &[u8]) -> u8 {
        let inst = code[self.ip];
        self.ip += 1;
        inst
    }

    fn next_usize_bytes(&mut self, code: &[u8]) -> [u8; std::mem::size_of::<usize>()] {
        const USIZE_SIZE: usize = std::mem::size_of::<usize>();

        let array: [u8; USIZE_SIZE] = code[self.ip..self.ip + USIZE_SIZE]
            .try_into()
            .expect("Unexpected end of bytecode");
        self.ip += USIZE_SIZE;

        array
    }

    #[inline]
    fn top(&self) -> &Value {
        &self.stack[self.sp - 1]
    }

    #[inline]
    fn set_top(&mut self, value: Value) {
        self.stack[self.sp - 1] = value;
    }

    // in any scope except the global scope, the base pointer points after the arguments
    #[inline]
    fn arguments_index(&self) -> usize {
        debug_assert!(
            !self.call_stack.is_empty(),
            "Accessing arguments when not in function"
        );
        self.bp - 1
    }

    #[inline]
    fn argument(&self, at: usize) -> &Value {
        &self.stack[self.arguments_index() - at]
    }

    #[inline]
    fn set_argument(&mut self, idx: usize, value: Value) {
        let idx = self.arguments_index() + idx;
        self.stack[idx] = value;
    }

    // in any scope except the global scope, the base pointer points to the executing function
    #[inline]
    fn locals_index(&self) -> usize {
        debug_assert!(
            !self.call_stack.is_empty(),
            "Accessing locals in global scope"
        );
        self.bp + 1
    }

    #[inline]
    fn local(&self, at: usize) -> &Value {
        &self.stack[self.locals_index() + at]
    }

    #[inline]
    fn set_local(&mut self, idx: usize, value: Value) {
        let idx = self.locals_index() + idx;
        self.stack[idx] = value;
    }

    fn executing_function(&self) -> Result<&Value, String> {
        debug_assert!(
            !self.call_stack.is_empty(),
            "Tried to get executing function in global scope"
        );
        debug_assert!(self.bp < self.stack.len(), "Base pointer is out of bounds");
        let func = &self.stack[self.bp];
        if let func @ Value::Function(_) = func {
            Ok(func)
        } else {
            Err(self.error(
                "Tried to get executing function but bp didn't point to function".to_string(),
            ))
        }
    }

    fn print_stacktrace(&self) -> String {
        let mut buf = "Stack trace:\n".to_string();
        for frame in self.call_stack.iter().rev() {
            if let Some(current_function) = frame.current_function {
                if let Value::Function(function_value) = &self.stack[current_function] {
                    match function_value.deref() {
                        FunctionValue::User { name, .. } => {
                            let module_name = self.agent.string_table[self
                                .modules
                                .get(frame.module_id)
                                .or_else(|| self.current_module.as_ref())
                                .unwrap()
                                .name()]
                            .clone();
                            let function_name =
                                name.map(|name| self.agent.string_table[name].clone());
                            buf += format!(
                                "	{}.{}\n",
                                module_name,
                                function_name.unwrap_or_else(|| "<anonymous>".into())
                            )
                            .as_ref();
                        }

                        FunctionValue::Builtin { name, .. } => {
                            buf += format!(
                                "	builtin {}\n",
                                name.map(|name| self.agent.string_table[name].clone())
                                    .unwrap_or_else(|| "<anonymous>".into())
                            )
                            .as_ref();
                        }
                    }
                } else {
                    unreachable!()
                }
            } else {
                buf += "	toplevel\n";
            }
        }
        buf
    }

    fn current_module(&self) -> Option<&Module> {
        if !self.call_stack.is_empty()
            && self.call_stack[self.call_stack.len() - 1].module_id
                != self.current_module.as_ref().unwrap().id()
        {
            self.modules
                .get(self.call_stack[self.call_stack.len() - 1].module_id)
        } else {
            self.current_module.as_ref()
        }
    }

    fn current_module_mut(&mut self) -> Option<&mut Module> {
        if !self.call_stack.is_empty()
            && self.call_stack[self.call_stack.len() - 1].module_id
                != self.current_module.as_ref().unwrap().id()
        {
            self.modules
                .get_mut(self.call_stack[self.call_stack.len() - 1].module_id)
        } else {
            self.current_module.as_mut()
        }
    }

    fn error(&self, msg: String) -> String {
        let mod_name = self.current_module().unwrap().name();
        format!(
            "Error in {}: {}\n{:#?}",
            self.agent.string_table[mod_name].clone(),
            msg,
            self.debuginfo.map(|d| d.get(self.ip)),
        )
    }

    pub fn evaluate(&mut self, code: Vec<u8>) -> Value {
        match self._evaluate(code) {
            Ok(value) => value,
            Err(e) => {
                eprintln!("{}\n{}", e, self.print_stacktrace());
                Value::Null
            }
        }
    }

    #[allow(clippy::cognitive_complexity)]
    fn _evaluate(&mut self, code: Vec<u8>) -> Result<Value, String> {
        if cfg!(disasm) {
            disassemble(self.agent, &code)?;
        }

        macro_rules! number_binop {
            ($name:expr, $intop:expr, $doubleop:expr) => {
                number_binop!($name, $intop, $doubleop, |a: i64| -> Result<i64, String> {
                    Ok(a)
                })
            };
            ($name:expr, $intop:expr, $doubleop:expr, $bconvert:expr) => {{
                let b = self.pop()?;
                let a = self.pop()?;

                self.push(if let Value::Integer(a) = a {
                    if let Value::Integer(b) = b {
                        let converter = $bconvert;
                        Value::from($intop(a, converter(b)?))
                    } else if let Value::Double(b) = b {
                        Value::from($doubleop(a as f64, b))
                    } else {
                        return Err(
                            self.error(format!("Got unexpected value {:?} in {}", b, $name))
                        );
                    }
                } else if let Value::Double(a) = a {
                    if let Value::Integer(b) = b {
                        Value::from($doubleop(a, b as f64))
                    } else if let Value::Double(b) = b {
                        Value::from($doubleop(a, b))
                    } else {
                        return Err(
                            self.error(format!("Got unexpected value {:?} in {}", b, $name))
                        );
                    }
                } else {
                    return Err(self.error(format!("Got unexpected value {:?} in {}", a, $name)));
                })
            }};
        }

        let code_len = code.len();
        while self.ip < code_len {
            let instruction = self.next_instruction(&code);
            if cfg!(vm_debug) {
                println!("--------------");
                print_stack!(&self.stack);
                println!("{:?}", OpCode::from(instruction));
                println!("ip: {} sp: {} bp: {}", self.ip, self.sp, self.bp);
                println!(
                    "{} {:?}",
                    if let Some(m) = self.current_module() {
                        self.agent.string_table[m.name()].clone()
                    } else {
                        "No module".to_string()
                    },
                    self.debuginfo
                        .and_then(|d| d.get(self.ip))
                        .map(|d| d.position)
                );
            }

            match OpCode::from(instruction) {
                OpCode::Halt => break,
                OpCode::ConstInt => self.const_int(&code),
                OpCode::ConstDouble => self.const_double(&code),
                OpCode::ConstNull => self.const_null(),
                OpCode::ConstTrue => self.const_true(),
                OpCode::ConstFalse => self.const_false(),
                OpCode::ConstString => self.const_string(&code),
                OpCode::ConstChar => self.const_char(&code),

                OpCode::Add => number_binop!("addition", i64::wrapping_add, f64::add),
                OpCode::Sub => number_binop!("subtraction", i64::wrapping_sub, f64::sub),
                OpCode::Mul => number_binop!("multiplication", i64::wrapping_mul, f64::mul),
                OpCode::Div => number_binop!("division", i64::wrapping_div, f64::div),
                OpCode::Mod => number_binop!("modulus", i64::wrapping_rem, f64::rem),
                OpCode::Exp => number_binop!(
                    "exponentiation",
                    i64::wrapping_pow,
                    f64::powf,
                    |b: i64| -> Result<u32, String> {
                        b.try_into().map_err(|_| "Integer overflow".to_string())
                    }
                ),

                OpCode::Jump => self.jump(&code),
                OpCode::JumpIfTrue => self.jump_if_true(&code)?,
                OpCode::JumpIfFalse => self.jump_if_false(&code)?,
                OpCode::Call => self.call(&code)?,
                OpCode::Return => self.return_()?,
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::LoadLocal => self.load_local(&code),
                OpCode::StoreLocal => self.store_local(&code),
                OpCode::LoadGlobal => self.load_global(&code)?,
                OpCode::DeclareGlobal => self.declare_global(&code),
                OpCode::StoreGlobal => self.store_global(&code)?,
                OpCode::NewFunction => self.new_function(&code),
                OpCode::BindLocal => self.bind_local(&code)?,
                OpCode::BindUpvalue => self.bind_upvalue(&code)?,
                OpCode::BindArgument => self.bind_argument(&code)?,
                OpCode::LoadUpvalue => self.load_upvalue(&code)?,
                OpCode::StoreUpvalue => self.store_upvalue(&code)?,
                OpCode::LoadArgument => self.load_argument(&code),
                OpCode::StoreArgument => self.store_argument(&code),
                OpCode::LoadFromModule => self.load_from_module(&code),
                OpCode::Export => self.export(&code)?,
                OpCode::NewArray => self.new_array(&code),
                OpCode::NewArrayWithValues => self.new_array_with_values(&code)?,
                OpCode::ArrayGet => self.array_get()?,
                OpCode::ArraySet => self.array_set()?,
                OpCode::Equal => self.equal()?,
                OpCode::NotEqual => self.not_equal()?,
                OpCode::LessThan => self.less_than()?,
                OpCode::LessThanEqual => self.less_than_equal()?,
                OpCode::GreaterThan => self.greater_than()?,
                OpCode::GreaterThanEqual => self.greater_than_equal()?,
                OpCode::BitwiseAnd => self.bitwise_and()?,
                OpCode::BitwiseOr => self.bitwise_or()?,
                OpCode::BitwiseXor => self.bitwise_xor()?,
                OpCode::BitwiseNot => self.bitwise_not()?,
                OpCode::Not => self.not()?,
                OpCode::LeftShift => self.left_shift()?,
                OpCode::RightShift => self.right_shift()?,
                OpCode::Neg => self.neg()?,
                OpCode::InitModule => self.init_module(&code),
                OpCode::EndModule => self.end_module(),
                OpCode::Dup => self.dup(),
                OpCode::AllocateLocals => self.allocate_locals(&code),
            }
        }

        Ok(if let Some(value) = self.stack.pop() {
            value
        } else {
            Value::Null
        })
    }

    #[inline]
    fn const_int(&mut self, code: &[u8]) {
        let usize_bytes = self.next_usize_bytes(&code);
        self.push(Value::from(i64::from_le_bytes(usize_bytes)));
    }

    #[inline]
    fn const_double(&mut self, code: &[u8]) {
        let usize_bytes = self.next_usize_bytes(&code);
        self.push(Value::from(f64::from_bits(u64::from_le_bytes(usize_bytes))));
    }

    #[inline]
    fn const_null(&mut self) {
        self.push(Value::Null);
    }

    #[inline]
    fn const_true(&mut self) {
        self.push(Value::from(true));
    }

    #[inline]
    fn const_false(&mut self) {
        self.push(Value::from(false));
    }

    #[inline]
    fn const_string(&mut self, code: &[u8]) {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        self.push(Value::InternedString(idx));
    }

    #[inline]
    fn const_char(&mut self, code: &[u8]) {
        let c: char = (usize::from_le_bytes(self.next_usize_bytes(&code)) as u32)
            .try_into()
            .unwrap();
        self.push(Value::from(c));
    }

    #[inline]
    fn jump(&mut self, code: &[u8]) {
        self.ip = usize::from_le_bytes(self.next_usize_bytes(&code));
    }

    #[inline]
    fn jump_if_true(&mut self, code: &[u8]) -> Result<(), String> {
        let to = usize::from_le_bytes(self.next_usize_bytes(&code));
        let cond = self.pop()?;
        if cond.is_truthy(&self.agent) {
            self.ip = to;
        }
        Ok(())
    }

    #[inline]
    fn jump_if_false(&mut self, code: &[u8]) -> Result<(), String> {
        let to = usize::from_le_bytes(self.next_usize_bytes(&code));
        let cond = self.pop()?;
        if !cond.is_truthy(&self.agent) {
            self.ip = to;
        }
        Ok(())
    }

    fn call(&mut self, code: &[u8]) -> Result<(), String> {
        let function = self.pop()?;
        let num_args = usize::from_le_bytes(self.next_usize_bytes(&code));
        if let Value::Function(f) = &function {
            macro_rules! ensure_arity {
                ($arity:expr, $name:expr) => {{
                    if num_args < $arity {
                        let name = if let Some(name) = $name {
                            self.agent.string_table[*name].as_ref()
                        } else {
                            "<anonymous>"
                        };
                        return Err(self.error(format!(
                            "Function {} expected {} args, got {}",
                            name, $arity, num_args
                        )));
                    }
                }};
            }
            match f.deref() {
                FunctionValue::Builtin {
                    arity,
                    function,
                    name,
                    ..
                } => {
                    ensure_arity!(*arity, name);
                    let args = self.top_n(num_args);
                    let result = function(self.agent, args)?;
                    self.drop_n(num_args);
                    self.push(result);
                }
                FunctionValue::User {
                    arity,
                    address,
                    name,
                    module,
                    ..
                } => {
                    ensure_arity!(*arity, name);
                    self.call_stack.push(Frame {
                        prev_ip: self.ip,
                        prev_bp: self.bp,
                        num_args,           // for cleanup
                        module_id: *module, // for accessing correct global scope
                        current_function: if self.call_stack.is_empty() {
                            None
                        } else {
                            Some(self.bp)
                        },
                    });
                    self.bp = self.sp; // new base is at current stack index
                    self.ip = *address; // jump into function
                    self.push(function);
                }
            }
            Ok(())
        } else {
            Err(self.error(format!(
                "Value {} is not callable",
                function.to_string(self.agent)
            )))
        }
    }

    fn return_(&mut self) -> Result<(), String> {
        let retval = self.pop()?;
        let frame = self
            .call_stack
            .pop()
            .ok_or("Trying to return outside of function")?;

        while let Some(uv) = self.agent.upvalues.pop() {
            debug_assert!(
                uv.borrow().is_open(),
                "Had closed upvalue in agent.upvalues"
            );
            let i = uv.borrow().stack_index();
            if i < self.bp - frame.num_args {
                self.agent.upvalues.push(uv);
                break;
            }
            uv.borrow_mut().close(self.stack[i].clone());
        }

        self.drop_n(frame.num_args + self.sp - self.bp);

        self.bp = frame.prev_bp;
        self.ip = frame.prev_ip;
        self.push(retval);

        Ok(())
    }

    #[inline]
    fn load_local(&mut self, code: &[u8]) {
        let usize_bytes = self.next_usize_bytes(&code);
        self.push(self.local(usize::from_le_bytes(usize_bytes)).clone());
    }

    #[inline]
    fn store_local(&mut self, code: &[u8]) {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        self.set_local(idx, self.top().clone());
    }

    #[inline]
    fn load_global(&mut self, code: &[u8]) -> Result<(), String> {
        let usize_bytes = self.next_usize_bytes(&code);
        let id = usize::from_le_bytes(usize_bytes);

        if let Some(module) = self.current_module_mut() {
            if let Some(val) = module.global_scope.get(&id).cloned() {
                self.push(val);
                Ok(())
            } else {
                Err(self.error(format!(
                    "ReferenceError: {} is not defined",
                    self.agent.string_table[id]
                )))
            }
        } else {
            unreachable!("Missing current_module");
        }
    }

    #[inline]
    fn declare_global(&mut self, code: &[u8]) {
        let id = usize::from_le_bytes(self.next_usize_bytes(&code));

        if let Some(module) = self.current_module_mut() {
            module.global_scope.insert(id, Value::Null);
        } else {
            unreachable!();
        }
    }

    fn store_global(&mut self, code: &[u8]) -> Result<(), String> {
        let id = usize::from_le_bytes(self.next_usize_bytes(&code));
        let top = self.top().clone();

        if let Some(module) = self.current_module_mut() {
            if module.global_scope.contains_key(&id) {
                module.global_scope.insert(id, top);
                Ok(())
            } else {
                Err(self.error(format!(
                    "ReferenceError: {} is not defined",
                    self.agent.string_table[id]
                )))
            }
        } else {
            unreachable!();
        }
    }

    fn new_function(&mut self, code: &[u8]) {
        let name = usize::from_le_bytes(self.next_usize_bytes(&code));
        let arity = usize::from_le_bytes(self.next_usize_bytes(&code));
        let address = usize::from_le_bytes(self.next_usize_bytes(&code));
        let module = if let Some(module) = self.current_module_mut() {
            module.id()
        } else {
            unreachable!();
        };

        self.push(Value::from(FunctionValue::User {
            name: if name == std::usize::MAX {
                None
            } else {
                Some(name)
            },
            address,
            arity,
            module,
            upvalues: Vec::new(),
        }));
    }

    fn bind_local(&mut self, code: &[u8]) -> Result<(), String> {
        let idx = self.locals_index() + usize::from_le_bytes(self.next_usize_bytes(&code));
        let mut func = self.pop()?;

        if let Value::Function(function_value) = &mut func {
            if let Some(FunctionValue::User { upvalues, .. }) = Rc::get_mut(function_value) {
                let upvalue = if let Some(upvalue) = self
                    .agent
                    .upvalues
                    .iter()
                    .find(|uv| uv.borrow().is_open() && uv.borrow().stack_index() == idx)
                {
                    upvalue
                } else {
                    self.agent
                        .upvalues
                        .push(Rc::new(RefCell::new(Upvalue::new(idx))));
                    self.agent.upvalues.last().unwrap()
                };
                upvalues.push(upvalue.clone());
                self.push(func);

                Ok(())
            } else {
                unreachable!();
            }
        } else {
            Err(self.error("Cannot bind local to non-user function".to_string()))
        }
    }

    fn bind_upvalue(&mut self, code: &[u8]) -> Result<(), String> {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        let mut func = self.pop()?;

        if let Value::Function(function_value) = &mut func {
            if let Some(FunctionValue::User { upvalues, .. }) = Rc::get_mut(function_value) {
                if let Value::Function(function_value) = self.executing_function()? {
                    if let FunctionValue::User {
                        upvalues: efn_upvalues,
                        ..
                    } = function_value.deref()
                    {
                        upvalues.push(efn_upvalues[idx].clone());
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
                self.push(func);
                Ok(())
            } else {
                unreachable!();
            }
        } else {
            Err(self.error("Cannot bind upvalue to non-user function".to_string()))
        }
    }

    fn bind_argument(&mut self, code: &[u8]) -> Result<(), String> {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        let mut func = self.pop()?;

        if let Value::Function(function_value) = &mut func {
            if let Some(FunctionValue::User { upvalues, .. }) = Rc::get_mut(function_value) {
                if let Value::Function(_) = self.executing_function()? {
                    let idx = self.arguments_index() - idx;
                    let upvalue = if let Some(upvalue) = self
                        .agent
                        .upvalues
                        .iter()
                        .find(|uv| uv.borrow().is_open() && uv.borrow().stack_index() == idx)
                    {
                        upvalue
                    } else {
                        self.agent
                            .upvalues
                            .push(Rc::new(RefCell::new(Upvalue::new(idx))));
                        self.agent.upvalues.last().unwrap()
                    };
                    upvalues.push(upvalue.clone());
                    self.push(func);
                    Ok(())
                } else {
                    unreachable!();
                }
            } else {
                unreachable!();
            }
        } else {
            Err(self.error("Cannot bind argument to non-user function".to_string()))
        }
    }

    fn load_upvalue(&mut self, code: &[u8]) -> Result<(), String> {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        let idx_or_value = if let Value::Function(function_value) = self.executing_function()? {
            if let FunctionValue::User { upvalues, .. } = function_value.deref() {
                let upvalue = (*upvalues[idx]).borrow();
                if upvalue.is_open() {
                    Ok(upvalue.stack_index())
                } else {
                    Err(upvalue.get_value())
                }
            } else {
                unreachable!();
            }
        } else {
            unreachable!()
        };

        if let Ok(idx) = idx_or_value {
            let value = self.stack[idx].clone();
            self.push(value);
        } else if let Err(value) = idx_or_value {
            self.push(value);
        }
        Ok(())
    }

    fn store_upvalue(&mut self, code: &[u8]) -> Result<(), String> {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        if let Value::Function(function_value) = self.executing_function()? {
            if let FunctionValue::User { upvalues, .. } = function_value.deref() {
                let upvalue = &upvalues[idx];
                if upvalue.borrow().is_open() {
                    let idx = upvalue.borrow().stack_index();
                    self.stack[idx] = self.top().clone();
                } else {
                    upvalue.borrow_mut().set_value(self.top().clone());
                }
            } else {
                unreachable!();
            }
            Ok(())
        } else {
            unreachable!();
        }
    }

    #[inline]
    fn load_argument(&mut self, code: &[u8]) {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        self.push(self.argument(idx).clone());
    }

    #[inline]
    fn store_argument(&mut self, code: &[u8]) {
        let idx = usize::from_le_bytes(self.next_usize_bytes(&code));
        self.set_argument(idx, self.top().clone());
    }

    fn load_from_module(&mut self, code: &[u8]) {
        let module_id = usize::from_le_bytes(self.next_usize_bytes(&code));
        let export_id = usize::from_le_bytes(self.next_usize_bytes(&code));

        self.push(self.modules[module_id].get_export(export_id));
    }

    fn export(&mut self, code: &[u8]) -> Result<(), String> {
        let export_id = usize::from_le_bytes(self.next_usize_bytes(&code));
        let exp = self.pop()?;
        if let Some(module) = self.current_module_mut() {
            module.set_export(export_id, exp);
            Ok(())
        } else {
            Err("Exports only allowed within a module".to_string())
        }
    }

    #[inline]
    fn new_array(&mut self, code: &[u8]) {
        let len = usize::from_le_bytes(self.next_usize_bytes(&code));
        self.push(Value::from(vec![Value::Null; len]));
    }

    #[inline]
    fn new_array_with_values(&mut self, code: &[u8]) -> Result<(), String> {
        let num_values = usize::from_le_bytes(self.next_usize_bytes(&code));
        let mut values = Vec::with_capacity(num_values);
        for _ in 0..num_values {
            values.push(self.pop()?);
        }
        values.reverse();
        self.push(Value::from(values));
        Ok(())
    }

    fn array_get(&mut self) -> Result<(), String> {
        let idx = self.pop()?;
        let array = self.top();

        if let Value::Integer(idx) = idx {
            if let Value::Array(array) = array {
                let idx = idx as usize;
                if array.borrow().len() > idx {
                    let value = array.borrow()[idx].clone();
                    self.set_top(value);
                    Ok(())
                } else {
                    Err(self.error(format!("Index {} is out of bounds", idx)))
                }
            } else {
                Err(self.error("Trying to access index of non-array".to_string()))
            }
        } else {
            Err(self.error("Array index must be an integer".to_string()))
        }
    }

    fn array_set(&mut self) -> Result<(), String> {
        let idx = self.pop()?;
        let array = self.pop()?;

        if let Value::Integer(idx) = idx {
            if let Value::Array(array) = array {
                let idx = idx as usize;
                if array.borrow().len() > idx {
                    array.borrow_mut()[idx] = self.top().clone();
                    Ok(())
                } else {
                    Err(self.error(format!("Index {} is out of bounds", idx)))
                }
            } else {
                Err(self.error("Trying to set index of non-array".to_string()))
            }
        } else {
            Err(self.error("Array index must be an integer".to_string()))
        }
    }

    #[inline]
    fn equal(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();
        let result = Value::from(left.eq(&right, self.agent));

        self.set_top(result);
        Ok(())
    }

    #[inline]
    fn not_equal(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();
        let result = Value::from(!left.eq(&right, self.agent));

        self.set_top(result);
        Ok(())
    }

    #[inline]
    fn less_than(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();
        let result = Value::from(left.cmp(&right, self.agent) == Some(Ordering::Less));

        self.set_top(result);
        Ok(())
    }

    #[inline]
    fn less_than_equal(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();
        let cmp = left.cmp(&right, self.agent);
        let result = Value::from(cmp == Some(Ordering::Less) || cmp == Some(Ordering::Equal));

        self.set_top(result);
        Ok(())
    }

    #[inline]
    fn greater_than(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();
        let result = Value::from(left.cmp(&right, self.agent) == Some(Ordering::Greater));

        self.set_top(result);
        Ok(())
    }

    #[inline]
    fn greater_than_equal(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();
        let cmp = left.cmp(&right, self.agent);
        let result = Value::from(cmp == Some(Ordering::Greater) || cmp == Some(Ordering::Equal));

        self.set_top(result);
        Ok(())
    }

    fn bitwise_and(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();

        if let Value::Integer(left) = left {
            if let Value::Integer(right) = right {
                let result = Value::from(*left & right);
                self.set_top(result);
                Ok(())
            } else {
                Err(self.error("Bitwise operations only support integers".to_string()))
            }
        } else {
            Err(self.error("Bitwise operations only support integers".to_string()))
        }
    }

    fn bitwise_or(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();

        if let Value::Integer(left) = left {
            if let Value::Integer(right) = right {
                let result = Value::from(*left | right);
                self.set_top(result);
                Ok(())
            } else {
                Err(self.error("Bitwise operations only support integers".to_string()))
            }
        } else {
            Err(self.error("Bitwise operations only support integers".to_string()))
        }
    }

    fn bitwise_xor(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();

        if let Value::Integer(left) = left {
            if let Value::Integer(right) = right {
                let result = Value::from(*left ^ right);
                self.set_top(result);
                Ok(())
            } else {
                Err(self.error("Bitwise operations only support integers".to_string()))
            }
        } else {
            Err(self.error("Bitwise operations only support integers".to_string()))
        }
    }

    fn bitwise_not(&mut self) -> Result<(), String> {
        let right = self.top();

        if let Value::Integer(right) = right {
            let result = Value::from(!*right);
            self.set_top(result);
            Ok(())
        } else {
            Err(self.error("Bitwise operations only support integers".to_string()))
        }
    }

    #[inline]
    fn not(&mut self) -> Result<(), String> {
        let result = Value::from(!self.top().is_truthy(&self.agent));

        self.set_top(result);
        Ok(())
    }

    fn left_shift(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();

        if let Value::Integer(left) = left {
            if let Value::Integer(right) = right {
                let result = Value::from(left.wrapping_shl(right as u32));
                self.set_top(result);
                Ok(())
            } else {
                Err(self.error("Bitwise operations only support integers".to_string()))
            }
        } else {
            Err(self.error("Bitwise operations only support integers".to_string()))
        }
    }

    fn right_shift(&mut self) -> Result<(), String> {
        let right = self.pop()?;
        let left = self.top();

        if let Value::Integer(left) = left {
            if let Value::Integer(right) = right {
                let result = Value::from(left.wrapping_shr(right as u32));
                self.set_top(result);
                Ok(())
            } else {
                Err(self.error("Bitwise operations only support integers".to_string()))
            }
        } else {
            Err(self.error("Bitwise operations only support integers".to_string()))
        }
    }

    fn neg(&mut self) -> Result<(), String> {
        let right = self.top();

        if let Value::Integer(right) = right {
            let result = Value::from(-*right);
            self.set_top(result);
            Ok(())
        } else {
            Err(self.error("Expected integer in negation expression".to_string()))
        }
    }

    #[inline]
    fn init_module(&mut self, code: &[u8]) {
        let id = usize::from_le_bytes(self.next_usize_bytes(&code));

        debug_assert!(self.current_module.is_none());
        self.current_module = Some(Module::new(
            self.agent.get_module(id).clone(),
            self.intrinsics.clone(),
        ));
    }

    #[inline]
    fn end_module(&mut self) {
        let module = self.current_module.take();
        debug_assert!(module.is_some());

        let mut module = module.unwrap();
        module.finalize();
        self.modules.insert(module.spec.id(), module);
    }

    #[inline]
    fn dup(&mut self) {
        let value = self.top().clone();
        self.push(value);
    }

    #[inline]
    fn allocate_locals(&mut self, code: &[u8]) {
        let count = usize::from_le_bytes(self.next_usize_bytes(&code));

        self.stack.reserve(count);
        for _ in 0..=count {
            self.push(Value::Null);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::bytecode::Bytecode;
    use crate::intrinsics::make_intrinsics;
    use crate::module::ModuleSpec;
    use pretty_assertions::assert_eq;

    macro_rules! get_agent {
        () => {{
            let mut agent = Agent::new();
            let name = agent.intern_string("test");
            let spec = ModuleSpec::new(name);

            agent.add_module(name, spec);

            agent
        }};
    }

    #[test]
    fn test_halt() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).halt().const_true().end_module();

        let result = interpreter._evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_const_int() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_int(123).end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_const_double() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_double(1.23).end_module();

        let code = bytecode.into();

        let result = interpreter._evaluate(code);
        assert_eq!(result, Ok(Value::from(1.23)));
    }

    #[test]
    fn test_const_true() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_true().end_module();

        let result = interpreter._evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::from(true)));
    }

    #[test]
    fn test_const_false() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_false().end_module();

        let result = interpreter._evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::from(false)));
    }

    #[test]
    fn test_const_null() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_null().end_module();

        let result = interpreter._evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_const_string() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_string(agent.intern_string("hello world"))
            .end_module();

        let mut interpreter = Interpreter::new(&mut agent);
        let code = bytecode.into();

        let result = interpreter._evaluate(code);
        assert_eq!(
            result,
            Ok(Value::InternedString(agent.intern_string("hello world")))
        );
    }

    #[test]
    fn test_add() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(123)
            .const_double(1.23)
            .add()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(124.23)));
    }

    #[test]
    fn test_sub() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(123)
            .const_double(1.23)
            .sub()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(121.77)));
    }

    #[test]
    fn test_mul() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(123)
            .const_double(2.0)
            .mul()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(246f64)));
    }

    #[test]
    fn test_div() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(124)
            .const_double(2.0)
            .div()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(62f64)));
    }

    #[test]
    fn test_mod() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(124)
            .const_double(2.0)
            .rem()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(0f64)));
    }

    #[test]
    fn test_exp() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(4)
            .const_int(2)
            .exp()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(16)));
    }

    #[test]
    fn test_jump() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(4)
            .op(OpCode::Jump)
            .address_of("jump_point")
            .const_int(8)
            .add()
            .halt()
            .label("jump_point")
            .const_int(12)
            .mul()
            .halt()
            .end_module();

        let code = bytecode.into();
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(48)));
    }

    #[test]
    fn test_jump_if_true() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(123)
            .const_int(234)
            .const_int(1)
            .op(OpCode::JumpIfTrue)
            .address_of("one")
            .mul()
            .halt()
            .label("one")
            .const_false()
            .op(OpCode::JumpIfTrue)
            .address_of("two")
            .add()
            .halt()
            .label("two")
            .sub()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(357)));
    }

    #[test]
    fn test_jump_if_false() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_true()
            .op(OpCode::JumpIfFalse)
            .address_of("one")
            .const_int(10)
            .const_int(2)
            .const_string(agent.intern_string(""))
            .op(OpCode::JumpIfFalse)
            .address_of("two")
            .label("one")
            .add()
            .halt()
            .label("two")
            .mul()
            .halt()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(20)));
    }

    #[test]
    fn test_user_function() {
        let mut agent = get_agent!();

        let name = agent.intern_string("ret123");
        let ret123 = Value::from(FunctionValue::User {
            name: Some(name),
            address: 18,
            arity: 0,
            module: 0,
            upvalues: Vec::new(),
        });

        let mut global = HashMap::new();
        global.insert(name, ret123);

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .const_int(123)
            .ret()
            .label("main")
            .load_global(name)
            .call(0)
            .end_module();

        let code: Vec<u8> = bytecode.into();
        crate::compiler::disassemble::disassemble(&agent, &code).unwrap();
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_pop() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_int(123).pop().end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_load_local() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::NewFunction)
            .usize(0)
            .usize(0)
            .address_of("fn")
            .op(OpCode::Jump)
            .address_of("end")
            .label("fn")
            .const_int(123)
            .const_double(432.0)
            .load_local(0)
            .ret()
            .label("end")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_store_local() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::NewFunction)
            .usize(0)
            .usize(0)
            .address_of("fn")
            .op(OpCode::Jump)
            .address_of("end")
            .label("fn")
            .const_int(123)
            .const_int(234)
            .store_local(0)
            .pop()
            .load_local(0)
            .ret()
            .label("end")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(234)));
    }

    #[test]
    fn test_load_global() {
        let mut agent = get_agent!();
        let mut global = HashMap::new();

        global.insert(agent.intern_string("test"), Value::from("test"));

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .load_global(agent.intern_string("test"))
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from("test")));
    }

    #[test]
    fn test_declare_global() {
        let mut agent = get_agent!();
        let ident_hello = agent.intern_string("hello");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .declare_global(ident_hello)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_store_global() {
        let mut agent = get_agent!();
        let mut global = HashMap::new();

        global.insert(agent.intern_string("test"), Value::from(3));

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .load_global(agent.intern_string("test"))
            .const_int(3)
            .exp()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(27)));
    }

    #[test]
    fn test_new_function() {
        let mut agent = get_agent!();
        let ident_func = agent.intern_string("func");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("func")
            .const_int(999)
            .ret()
            .label("main")
            .op(OpCode::NewFunction)
            .usize(ident_func)
            .usize(0)
            .address_of("func")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(999)));
    }

    #[test]
    fn test_bind_local() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::NewFunction)
            .usize(0)
            .usize(0)
            .address_of("outer")
            .op(OpCode::Jump)
            .address_of("end")
            .label("outer")
            .op(OpCode::Jump)
            .address_of("main")
            .label("func")
            .load_upvalue(0)
            .ret()
            .label("main")
            .const_int(123)
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("func")
            .bind_local(0)
            .call(0)
            .ret()
            .label("end")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_bind_upvalue() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::NewFunction)
            .usize(0)
            .usize(0)
            .address_of("outer")
            .op(OpCode::Jump)
            .address_of("end")
            .label("outer")
            .op(OpCode::Jump)
            .address_of("main")
            .label("func1")
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("func2")
            .bind_upvalue(0)
            .ret()
            .label("func2")
            .load_upvalue(0)
            .ret()
            .label("main")
            .const_int(2334)
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("func1")
            .bind_local(0)
            .call(0)
            .call(0)
            .ret()
            .label("end")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(2334)));
    }

    #[test]
    fn test_bind_argument() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("func1")
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("func2")
            .bind_argument(0)
            .ret()
            .label("func2")
            .load_upvalue(0)
            .ret()
            .label("main")
            .const_int(2334)
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(1)
            .address_of("func1")
            .call(1)
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(2334)));
    }

    #[test]
    fn test_load_upvalue() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::NewFunction)
            .usize(0)
            .usize(0)
            .address_of("fn")
            .op(OpCode::Jump)
            .address_of("end")
            .label("fn")
            .op(OpCode::Jump)
            .address_of("main")
            .label("test")
            .load_upvalue(0)
            .ret()
            .label("main")
            .const_string(agent.intern_string("hello"))
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("test")
            .bind_local(0)
            .call(0)
            .ret()
            .label("end")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::InternedString(agent.intern_string("hello")))
        );
    }

    #[test]
    fn test_store_upvalue() {
        let mut agent = get_agent!();

        let a = agent.intern_string("a");
        let b = agent.intern_string("b");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("test")
            .const_int(0)
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("func_a")
            .bind_local(0)
            .store_global(a)
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("func_b")
            .bind_local(0)
            .store_global(b)
            .const_null()
            .ret()
            .label("func_a")
            .load_upvalue(0)
            .ret()
            .label("func_b")
            .load_upvalue(0)
            .const_int(1)
            .add()
            .store_upvalue(0)
            .pop()
            .const_null()
            .ret()
            .label("main")
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(0)
            .address_of("test")
            .call(0)
            .pop()
            .load_global(b)
            .call(0)
            .pop()
            .load_global(b)
            .call(0)
            .load_global(a)
            .call(0)
            .end_module();

        let code = bytecode.into();

        let mut global = HashMap::new();
        global.insert(a, Value::Null);
        global.insert(b, Value::Null);

        let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(2)));
    }

    #[test]
    fn test_load_argument() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("func")
            .load_argument(0)
            .ret()
            .label("main")
            .const_string(agent.intern_string("hullo"))
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(1)
            .address_of("func")
            .call(1)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::InternedString(agent.intern_string("hullo")))
        );
    }

    #[test]
    fn test_store_argument() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("func")
            .load_argument(0)
            .const_int(2)
            .add()
            .store_argument(0)
            .load_argument(0)
            .ret()
            .label("main")
            .const_int(1)
            .op(OpCode::NewFunction)
            .usize(std::usize::MAX)
            .usize(1)
            .address_of("func")
            .call(1)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(3)));
    }

    #[test]
    fn test_new_array() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).new_array(10).end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(vec![Value::Null; 10])));
    }

    #[test]
    fn test_array_get() {
        let mut agent = get_agent!();

        let array = agent.intern_string("array");

        let mut global = HashMap::new();
        global.insert(array, Value::from(vec![Value::Null, Value::from(123)]));

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .load_global(array)
            .const_int(1)
            .array_get()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_array_set() {
        let mut agent = get_agent!();

        let array = agent.intern_string("array");

        let mut global = HashMap::new();
        global.insert(array, Value::from(vec![Value::Null, Value::from(123)]));

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .load_global(array)
            .const_int(1)
            .array_set()
            .pop()
            .load_global(array)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![Value::Null, Value::from(9229)]))
        );
    }

    #[test]
    fn test_equal() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .const_int(9229)
            .equal()
            .const_int(9229)
            .const_int(9230)
            .equal()
            .new_array_with_values(2)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![Value::from(true), Value::from(false)]))
        );
    }

    #[test]
    fn test_not_equal() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .const_int(9228)
            .not_equal()
            .const_int(9229)
            .const_int(9229)
            .not_equal()
            .new_array_with_values(2)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![Value::from(true), Value::from(false)]))
        );
    }

    #[test]
    fn test_less_than() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .const_int(9228)
            .less_than()
            .const_int(9229)
            .const_int(9229)
            .less_than()
            .const_int(9229)
            .const_int(9230)
            .less_than()
            .new_array_with_values(3)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(false),
                Value::from(false),
                Value::from(true),
            ]))
        );
    }

    #[test]
    fn test_less_than_equal() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .const_int(9229)
            .less_than_equal()
            .const_int(9228)
            .const_int(9229)
            .less_than_equal()
            .const_int(9230)
            .const_int(9229)
            .less_than_equal()
            .new_array_with_values(3)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(true),
                Value::from(true),
                Value::from(false),
            ]))
        );
    }

    #[test]
    fn test_greater_than() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .const_int(9229)
            .greater_than()
            .const_int(9229)
            .const_int(9228)
            .greater_than()
            .const_int(9229)
            .const_int(9230)
            .greater_than()
            .new_array_with_values(3)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(false),
                Value::from(true),
                Value::from(false),
            ]))
        );
    }

    #[test]
    fn test_greater_than_equal() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(9229)
            .const_int(9229)
            .greater_than_equal()
            .const_int(9228)
            .const_int(9229)
            .greater_than_equal()
            .const_int(9230)
            .const_int(9229)
            .greater_than_equal()
            .new_array_with_values(3)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(true),
                Value::from(false),
                Value::from(true),
            ]))
        );
    }

    #[test]
    fn test_bitwise_and() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(6)
            .const_int(1)
            .bitwise_and()
            .const_int(6)
            .const_int(2)
            .bitwise_and()
            .const_int(1)
            .const_int(6)
            .bitwise_and()
            .const_int(2)
            .const_int(6)
            .bitwise_and()
            .new_array_with_values(4)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(0),
                Value::from(2),
                Value::from(0),
                Value::from(2),
            ]))
        );
    }

    #[test]
    fn test_bitwise_or() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(6)
            .const_int(1)
            .bitwise_or()
            .const_int(6)
            .const_int(2)
            .bitwise_or()
            .const_int(1)
            .const_int(6)
            .bitwise_or()
            .const_int(2)
            .const_int(6)
            .bitwise_or()
            .new_array_with_values(4)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(7),
                Value::from(6),
                Value::from(7),
                Value::from(6),
            ]))
        );
    }

    #[test]
    fn test_bitwise_xor() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(6)
            .const_int(1)
            .bitwise_xor()
            .const_int(6)
            .const_int(2)
            .bitwise_xor()
            .const_int(1)
            .const_int(6)
            .bitwise_xor()
            .const_int(2)
            .const_int(6)
            .bitwise_xor()
            .new_array_with_values(4)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(
            result,
            Ok(Value::from(vec![
                Value::from(7),
                Value::from(4),
                Value::from(7),
                Value::from(4),
            ]))
        );
    }

    #[test]
    fn test_bitwise_not() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(0)
            .bitwise_not()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(-1)));
    }

    #[test]
    fn test_not() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_true().not().end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(false)));
    }

    #[test]
    fn test_shift_left() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(2)
            .const_int(3)
            .shift_left()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(16)));
    }

    #[test]
    fn test_shift_right() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(16)
            .const_int(3)
            .shift_right()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(2)));
    }

    #[test]
    fn test_calling_builtin_function() {
        let mut agent = get_agent!();
        let ident_tostring = agent.intern_string("tostring");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(3)
            .load_global(ident_tostring)
            .call(1)
            .end_module();

        let code = bytecode.into();
        let intrinsics = make_intrinsics(&mut agent);
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, intrinsics);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from("3")));
    }

    #[test]
    fn test_load_from_module() {
        let mut agent = Agent::new();
        let ident_testmodule = agent.intern_string("TestModule");
        let ident_test = agent.intern_string("test");

        let mut modspec = ModuleSpec::new(ident_testmodule);
        modspec.add_export(ident_test);
        agent.add_module(ident_testmodule, modspec);
        agent.add_module(ident_test, ModuleSpec::new(ident_test));

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("after_test")
            .label("test")
            .const_int(3)
            .ret()
            .label("after_test")
            .op(OpCode::NewFunction)
            .usize(ident_test)
            .usize(0)
            .address_of("test")
            .declare_global(ident_test)
            .store_global(ident_test)
            .export(0)
            .end_module()
            .init_module(1)
            .load_from_module(0, 0)
            .call(0)
            .end_module();

        let code = bytecode.into();
        let intrinsics = make_intrinsics(&mut agent);
        let mut interpreter = Interpreter::with_intrinsics(&mut agent, intrinsics);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(3)));
    }

    #[test]
    fn test_neg() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_int(1).neg().end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(-1)));
    }

    #[test]
    fn test_dup() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_int(1).dup().end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert_eq!(result, Ok(Value::from(1)));
    }

    #[test]
    fn test_allocate_locals() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).allocate_locals(2).end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_ok());
        assert_eq!(interpreter.stack.len(), 2);
    }

    #[test]
    fn test_call_not_callable() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(0)
            .call(0)
            .pop()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_global() {
        let mut agent = get_agent!();
        let ident_test = agent.intern_string("test");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .load_global(ident_test)
            .pop()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_get_out_of_bounds() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .new_array(0)
            .const_int(1)
            .array_get()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_get_non_array() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(1)
            .dup()
            .array_get()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_get_non_int() {
        let mut agent = get_agent!();
        let str_test = agent.intern_string("test");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .new_array(1)
            .const_string(str_test)
            .array_get()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_set_out_of_bounds() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .new_array(0)
            .const_int(1)
            .dup()
            .array_set()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_set_non_array() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(1)
            .dup()
            .dup()
            .array_get()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }

    #[test]
    fn test_array_set_non_int() {
        let mut agent = get_agent!();
        let str_test = agent.intern_string("test");

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .new_array(1)
            .const_string(str_test)
            .dup()
            .array_get()
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter._evaluate(code);

        assert!(result.is_err());
    }
}
