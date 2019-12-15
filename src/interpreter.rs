use std::cell::RefCell;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::Rc;

use crate::agent::Agent;
use crate::compiler::disassemble::disassemble;
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

pub struct Interpreter<'a> {
    pub agent: &'a mut Agent,
    intrinsics: HashMap<usize, Value>,
    modules: HashMap<usize, Module>,
    current_module: Option<Module>,
    call_stack: Vec<usize>,
    stack: Vec<Value>,
    ip: usize,
    bp: usize,
    sp: usize,
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
            agent,
            intrinsics,
            modules: HashMap::new(),
            current_module: None,
            call_stack: Vec::new(),
            stack: Vec::new(),
            ip: 0,
            bp: 0,
            sp: 0,
        }
    }

    #[allow(clippy::cognitive_complexity)]
    pub fn evaluate(&mut self, code: Vec<u8>) -> Result<Value, String> {
        if cfg!(vm_debug) {
            disassemble(self.agent, &code)?;
        }

        macro_rules! push {
            ($expr:expr) => {{
                self.sp += 1;
                self.stack.push($expr)
            }};
        }
        macro_rules! pop {
            () => {{
                let val = self.stack.pop().ok_or("Stack underflow")?;
                self.sp -= 1;
                val
            }};
            ($num:expr) => {{
                self.sp = self.sp.saturating_sub($num);
                self.stack.truncate(self.sp);
            }};
        }
        macro_rules! pop_and_get {
            ($num:expr) => {{
                let vals = self
                    .stack
                    .split_off(self.sp - $num)
                    .into_iter()
                    .rev()
                    .collect::<Vec<_>>();
                self.sp -= $num;
                vals
            }};
        }
        macro_rules! next {
            () => {{
                let inst = code.get(self.ip).cloned();
                self.ip += 1;
                inst
            }};
            ($expr:expr) => {{
                let mut array = [0u8; $expr];

                for i in 0..$expr {
                    array[i] = code[self.ip + i];
                }
                self.ip += $expr;

                array
            }};
        }

        macro_rules! top {
            () => {
                self.stack[self.sp - 1]
            };
        }

        // in any scope except the global scope, the base pointer points after the arguments
        macro_rules! arguments_index {
            () => {
                if self.call_stack.is_empty() {
                    return error!("Trying to access arguments when not in function".to_string());
                } else {
                    self.bp - 1
                }
            };
        }

        macro_rules! arguments {
            ($idx:expr) => {
                self.stack[arguments_index!() - $idx]
            };
        }

        // in any scope except the global scope, the base pointer points to the executing function
        macro_rules! locals_index {
            () => {
                if self.call_stack.is_empty() {
                    0
                } else {
                    self.bp + 1
                }
            };
        }

        macro_rules! locals {
            ($index:expr) => {
                self.stack[$index + locals_index!()]
            };
        }

        macro_rules! executing_function {
            () => {
                if self.call_stack.is_empty() {
                    return error!("Tried to get executing function in global scope".to_string());
                } else if let Some(func) = self.stack.get(self.bp) {
                    if let func @ Value::Function(_) = func {
                        func.clone()
                    } else {
                        return error!(
                            "Tried to get executing function but bp didn't point to function"
                                .to_string()
                        );
                    }
                } else {
                    return error!(format!(
                        "Base pointer is not within stack: bp={} stack length={}",
                        self.bp,
                        self.stack.len()
                    ));
                }
            };
        }

        macro_rules! current_module {
            () => {{
                if !self.call_stack.is_empty()
                    && *self.call_stack.last().unwrap()
                        != self.current_module.as_ref().unwrap().name()
                {
                    self.modules.get_mut(self.call_stack.last().unwrap())
                } else {
                    self.current_module.as_mut()
                }
            }};
        }

        macro_rules! number_binop {
            ($name:expr, $intop:expr, $doubleop:expr) => {
                number_binop!($name, $intop, $doubleop, |a: i64| -> Result<i64, String> {
                    Ok(a)
                })
            };
            ($name:expr, $intop:expr, $doubleop:expr, $bconvert:expr) => {{
                let b = pop!();
                let a = pop!();

                push!(if let Value::Integer(a) = a {
                    if let Value::Integer(b) = b {
                        let converter = $bconvert;
                        Value::from($intop(a, converter(b)?))
                    } else if let Value::Double(b) = b {
                        Value::from($doubleop(a as f64, b))
                    } else {
                        return error!(format!("Got unexpected value {:?} in {}", b, $name));
                    }
                } else if let Value::Double(a) = a {
                    if let Value::Integer(b) = b {
                        Value::from($doubleop(a, b as f64))
                    } else if let Value::Double(b) = b {
                        Value::from($doubleop(a, b))
                    } else {
                        return error!(format!("Got unexpected value {:?} in {}", b, $name));
                    }
                } else {
                    return error!(format!("Got unexpected value {:?} in {}", a, $name));
                })
            }};
        }

        macro_rules! error {
            ($msg:expr) => {{
                Err(format!(
                    "Error in {}: {}",
                    self.agent.string_table[current_module!().unwrap().name()].clone(),
                    $msg
                ))
            }};
        }

        while let Some(instruction) = next!() {
            if cfg!(vm_debug) {
                println!("--------------");
                print_stack!(&self.stack);
                println!("{:?}", OpCode::from(instruction));
                println!("ip: {} sp: {} bp: {}", self.ip, self.sp, self.bp);
            }

            match OpCode::from(instruction) {
                OpCode::Halt => break,

                OpCode::ConstInt => {
                    push!(Value::from(i64::from_le_bytes(next!(8))));
                }

                OpCode::ConstDouble => {
                    push!(Value::from(f64::from_bits(u64::from_le_bytes(next!(8))),));
                }

                OpCode::ConstNull => {
                    push!(Value::Null);
                }

                OpCode::ConstTrue => {
                    push!(Value::from(true));
                }

                OpCode::ConstFalse => {
                    push!(Value::from(false));
                }

                OpCode::ConstString => {
                    let idx = usize::from_le_bytes(next!(8));
                    push!(Value::from(self.agent.string_table[idx].as_ref()));
                }

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

                OpCode::Jump => {
                    self.ip = usize::from_le_bytes(next!(8));
                }

                OpCode::JumpIfTrue => {
                    let to = usize::from_le_bytes(next!(8));
                    let cond = pop!();
                    if cond.is_truthy() {
                        self.ip = to;
                    }
                }

                OpCode::JumpIfFalse => {
                    let to = usize::from_le_bytes(next!(8));
                    let cond = pop!();
                    if !cond.is_truthy() {
                        self.ip = to;
                    }
                }

                OpCode::Call => {
                    let function = pop!();
                    let num_args = usize::from_le_bytes(next!(8));
                    if let Value::Function(f) = &function {
                        macro_rules! ensure_arity {
                            ($arity:expr, $name:expr) => {{
                                if num_args < $arity {
                                    let name = if let Some(name) = $name {
                                        self.agent.string_table[*name].as_ref()
                                    } else {
                                        "<anonymous>"
                                    };
                                    return error!(format!(
                                        "Function {} expected {} args, got {}",
                                        name, $arity, num_args
                                    ));
                                }
                            }};
                        }
                        match f {
                            FunctionValue::Builtin {
                                arity,
                                function,
                                name,
                                ..
                            } => {
                                ensure_arity!(*arity, name);
                                let args = pop_and_get!(num_args);
                                let result = function(self, args);
                                push!(result);
                            }
                            FunctionValue::User {
                                arity,
                                address,
                                name,
                                module,
                                ..
                            } => {
                                ensure_arity!(*arity, name);
                                self.call_stack.push(self.ip); // return address
                                self.call_stack.push(self.bp); // current base pointer
                                self.call_stack.push(num_args); // for cleanup
                                self.call_stack.push(*module); // for accessing correct global scope
                                self.bp = self.sp; // new base is at current stack index
                                self.ip = *address; // jump into function
                                push!(function);
                            }
                        }
                    } else {
                        return error!(format!("Value {} is not callable", function));
                    }
                }

                OpCode::Return => {
                    let retval = pop!();
                    self.call_stack
                        .pop()
                        .ok_or("Missing module id on call_stack")?;
                    let num_args = self
                        .call_stack
                        .pop()
                        .ok_or("Missing num_args on call_stack")?;

                    while let Some(uv) = self.agent.upvalues.pop() {
                        if uv.borrow().is_open() {
                            let i = uv.borrow().stack_index();
                            if i < self.bp - num_args {
                                self.agent.upvalues.push(uv);
                                break;
                            }
                            uv.borrow_mut().close(self.stack[i].clone());
                        } else {
                            return error!("Had closed upvalue in agent.upvalues".to_string());
                        }
                    }

                    pop!(num_args + self.sp - self.bp);

                    self.bp = self.call_stack.pop().ok_or("Missing bp on call_stack")?;
                    self.ip = self.call_stack.pop().ok_or("Missing ip on call_stack")?;
                    push!(retval);
                }

                OpCode::Pop => {
                    pop!();
                }

                OpCode::LoadLocal => {
                    push!(locals![usize::from_le_bytes(next!(8))].clone());
                }

                OpCode::StoreLocal => {
                    let idx = usize::from_le_bytes(next!(8));
                    locals![idx] = top!().clone();
                }

                OpCode::LoadGlobal => {
                    let id = usize::from_le_bytes(next!(8));

                    if let Some(module) = current_module!() {
                        if let Some(val) = module.global_scope.get(&id) {
                            push!(val.clone());
                        } else {
                            return error!(format!(
                                "ReferenceError: {} is not defined",
                                self.agent.string_table[id]
                            ));
                        }
                    } else {
                        unreachable!();
                    }
                }

                OpCode::DeclareGlobal => {
                    let id = usize::from_le_bytes(next!(8));

                    if let Some(module) = current_module!() {
                        module.global_scope.insert(id, Value::Null);
                    } else {
                        unreachable!();
                    }
                }

                OpCode::StoreGlobal => {
                    let id = usize::from_le_bytes(next!(8));

                    if let Some(module) = current_module!() {
                        if module.global_scope.contains_key(&id) {
                            module.global_scope.insert(id, top!().clone());
                        } else {
                            return error!(format!(
                                "ReferenceError: {} is not defined",
                                self.agent.string_table[id]
                            ));
                        }
                    } else {
                        unreachable!();
                    }
                }

                OpCode::NewFunction => {
                    let arity = usize::from_le_bytes(next!(8));
                    let address = usize::from_le_bytes(next!(8));
                    let module = if let Some(module) = current_module!() {
                        module.name()
                    } else {
                        unreachable!();
                    };

                    push!(Value::from(FunctionValue::User {
                        name: None,
                        address,
                        arity,
                        module,
                        upvalues: Vec::new(),
                    }));
                }

                OpCode::BindLocal => {
                    let idx = locals_index!() + usize::from_le_bytes(next!(8));
                    let mut func = pop!();

                    if let Value::Function(FunctionValue::User { upvalues, .. }) = &mut func {
                        let upvalue = if let Some(upvalue) =
                            self.agent.upvalues.iter().find(|uv| {
                                uv.borrow().is_open() && uv.borrow().stack_index() == idx
                            }) {
                            upvalue
                        } else {
                            self.agent
                                .upvalues
                                .push(Rc::new(RefCell::new(Upvalue::new(idx))));
                            self.agent.upvalues.last().unwrap()
                        };
                        upvalues.push(upvalue.clone());
                        push!(func);
                    } else {
                        return error!("Cannot bind local to non-user function".to_string());
                    }
                }

                OpCode::BindUpvalue => {
                    let idx = usize::from_le_bytes(next!(8));
                    let mut func = pop!();

                    if let Value::Function(FunctionValue::User { upvalues, .. }) = &mut func {
                        if let Value::Function(FunctionValue::User {
                            upvalues: efn_upvalues,
                            ..
                        }) = &executing_function!()
                        {
                            upvalues.push(efn_upvalues[idx].clone());
                        } else {
                            unreachable!();
                        }
                        push!(func);
                    } else {
                        return error!("Cannot bind upvalue to non-user function".to_string());
                    }
                }

                OpCode::BindArgument => {
                    let idx = usize::from_le_bytes(next!(8));
                    let mut func = pop!();

                    if let Value::Function(FunctionValue::User { upvalues, .. }) = &mut func {
                        if let Value::Function(_) = &executing_function!() {
                            let idx = arguments_index!() - idx;
                            let upvalue = if let Some(upvalue) =
                                self.agent.upvalues.iter().find(|uv| {
                                    uv.borrow().is_open() && uv.borrow().stack_index() == idx
                                }) {
                                upvalue
                            } else {
                                self.agent
                                    .upvalues
                                    .push(Rc::new(RefCell::new(Upvalue::new(idx))));
                                self.agent.upvalues.last().unwrap()
                            };
                            upvalues.push(upvalue.clone());
                            push!(func);
                        } else {
                            unreachable!();
                        }
                    } else {
                        return error!("Cannot bind argument to non-user function".to_string());
                    }
                }

                OpCode::LoadUpvalue => {
                    let idx = usize::from_le_bytes(next!(8));
                    if let Value::Function(FunctionValue::User { upvalues, .. }) =
                        &executing_function!()
                    {
                        let upvalue = (*upvalues[idx]).borrow();
                        if upvalue.is_open() {
                            push!(self.stack[upvalue.stack_index()].clone());
                        } else {
                            push!(upvalue.get_value());
                        }
                    } else {
                        unreachable!();
                    }
                }

                OpCode::StoreUpvalue => {
                    let idx = usize::from_le_bytes(next!(8));
                    if let Value::Function(FunctionValue::User { upvalues, .. }) =
                        &executing_function!()
                    {
                        let upvalue = &upvalues[idx];
                        if upvalue.borrow().is_open() {
                            self.stack[upvalue.borrow().stack_index()] = top!().clone();
                        } else {
                            upvalue.borrow_mut().set_value(top!().clone());
                        }
                    } else {
                        unreachable!();
                    }
                }

                OpCode::LoadArgument => {
                    let idx = usize::from_le_bytes(next!(8));
                    push!(arguments![idx].clone());
                }

                OpCode::StoreArgument => {
                    let idx = usize::from_le_bytes(next!(8));
                    arguments![idx] = top!().clone();
                }

                OpCode::LoadFromModule => {
                    let module_name = usize::from_le_bytes(next!(8));
                    let export_name = usize::from_le_bytes(next!(8));

                    push!(self
                        .modules
                        .get(&module_name)
                        .ok_or_else(|| format!(
                            "Unknown module {}",
                            self.agent.string_table[module_name]
                        ))?
                        .resolve_export(self.agent, export_name)?);
                }

                OpCode::NewArray => {
                    let len = usize::from_le_bytes(next!(8));
                    push!(Value::from(vec![Value::Null; len]));
                }

                OpCode::NewArrayWithValues => {
                    let num_values = usize::from_le_bytes(next!(8));
                    let mut values = Vec::with_capacity(num_values);
                    for _ in 0..num_values {
                        values.push(pop!());
                    }
                    push!(Value::from(values.into_iter().rev().collect::<Vec<_>>()));
                }

                OpCode::ArrayGet => {
                    let idx = pop!();
                    let array = pop!();

                    if let Value::Integer(idx) = idx {
                        if let Value::Array(array) = array {
                            let idx = idx as usize;
                            if array.borrow().len() > idx {
                                push!(array.borrow()[idx].clone());
                            } else {
                                return error!(format!("Index {} is out of bounds", idx));
                            }
                        } else {
                            return error!("Trying to access index of non-array".to_string());
                        }
                    } else {
                        return error!("Array index must be an integer".to_string());
                    }
                }

                OpCode::ArraySet => {
                    let idx = pop!();
                    let array = pop!();

                    if let Value::Integer(idx) = idx {
                        if let Value::Array(array) = array {
                            let idx = idx as usize;
                            if array.borrow().len() > idx {
                                array.borrow_mut()[idx] = top!().clone();
                            } else {
                                return error!(format!("Index {} is out of bounds", idx));
                            }
                        } else {
                            return error!("Trying to set index of non-array".to_string());
                        }
                    } else {
                        return error!("Array index must be an integer".to_string());
                    }
                }

                OpCode::Equal => {
                    let right = pop!();
                    let left = pop!();

                    push!(Value::from(left == right));
                }

                OpCode::NotEqual => {
                    let right = pop!();
                    let left = pop!();

                    push!(Value::from(left != right));
                }

                OpCode::LessThan => {
                    let right = pop!();
                    let left = pop!();

                    push!(Value::from(left < right));
                }

                OpCode::LessThanEqual => {
                    let right = pop!();
                    let left = pop!();

                    push!(Value::from(left <= right));
                }

                OpCode::GreaterThan => {
                    let right = pop!();
                    let left = pop!();

                    push!(Value::from(left > right));
                }

                OpCode::GreaterThanEqual => {
                    let right = pop!();
                    let left = pop!();

                    push!(Value::from(left >= right));
                }

                OpCode::BitwiseAnd => {
                    let right = pop!();
                    let left = pop!();

                    if let Value::Integer(left) = left {
                        if let Value::Integer(right) = right {
                            push!(Value::from(left & right));
                        } else {
                            return error!("Bitwise operations only support integers".to_string());
                        }
                    } else {
                        return error!("Bitwise operations only support integers".to_string());
                    }
                }

                OpCode::BitwiseOr => {
                    let right = pop!();
                    let left = pop!();

                    if let Value::Integer(left) = left {
                        if let Value::Integer(right) = right {
                            push!(Value::from(left | right));
                        } else {
                            return error!("Bitwise operations only support integers".to_string());
                        }
                    } else {
                        return error!("Bitwise operations only support integers".to_string());
                    }
                }

                OpCode::BitwiseXor => {
                    let right = pop!();
                    let left = pop!();

                    if let Value::Integer(left) = left {
                        if let Value::Integer(right) = right {
                            push!(Value::from(left ^ right));
                        } else {
                            return error!("Bitwise operations only support integers".to_string());
                        }
                    } else {
                        return error!("Bitwise operations only support integers".to_string());
                    }
                }

                OpCode::BitwiseNot => {
                    let right = pop!();

                    if let Value::Integer(right) = right {
                        push!(Value::from(!right));
                    } else {
                        return error!("Bitwise operations only support integers".to_string());
                    }
                }

                OpCode::Not => {
                    let right = pop!();

                    push!(Value::from(!right.is_truthy()));
                }

                OpCode::LeftShift => {
                    let right = pop!();
                    let left = pop!();

                    if let Value::Integer(left) = left {
                        if let Value::Integer(right) = right {
                            push!(Value::from(left.wrapping_shl(right as u32)));
                        } else {
                            return error!("Bitwise operations only support integers".to_string());
                        }
                    } else {
                        return error!("Bitwise operations only support integers".to_string());
                    }
                }

                OpCode::RightShift => {
                    let right = pop!();
                    let left = pop!();

                    if let Value::Integer(left) = left {
                        if let Value::Integer(right) = right {
                            push!(Value::from(left.wrapping_shr(right as u32)));
                        } else {
                            return error!("Bitwise operations only support integers".to_string());
                        }
                    } else {
                        return error!("Bitwise operations only support integers".to_string());
                    }
                }

                OpCode::Neg => {
                    let right = pop!();

                    if let Value::Integer(right) = right {
                        push!(Value::from(-right));
                    } else {
                        return error!("Expected integer in negation expression".to_string());
                    }
                }

                OpCode::InitModule => {
                    let name = usize::from_le_bytes(next!(8));

                    debug_assert!(self.current_module.is_none());
                    self.current_module = Some(Module::new(
                        self.agent.modules[&name].clone(),
                        self.intrinsics.clone(),
                    ));
                }

                OpCode::EndModule => {
                    let module = self.current_module.take();
                    debug_assert!(module.is_some());

                    let module = module.unwrap();
                    self.modules.insert(module.name(), module);
                }

                OpCode::Dup => {
                    let value = top!().clone();
                    push!(value);
                }
            }
        }

        Ok(if let Some(value) = self.stack.pop() {
            value
        } else {
            Value::Null
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::bytecode::Bytecode;
    use crate::module::ModuleSpec;
    use pretty_assertions::assert_eq;

    macro_rules! get_agent {
        () => {{
            let mut agent = Agent::new();
            let name = agent.intern_string("test");
            let spec = ModuleSpec::new(name);

            agent.modules.insert(name, spec);

            agent
        }};
    }

    #[test]
    fn test_halt() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).halt().const_true().end_module();

        let result = interpreter.evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_const_int() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_int(123).end_module();

        let code = bytecode.into();
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_const_double() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_double(1.23).end_module();

        let code = bytecode.into();

        let result = interpreter.evaluate(code);
        assert_eq!(result, Ok(Value::from(1.23)));
    }

    #[test]
    fn test_const_true() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_true().end_module();

        let result = interpreter.evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::from(true)));
    }

    #[test]
    fn test_const_false() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_false().end_module();

        let result = interpreter.evaluate(bytecode.into());
        assert_eq!(result, Ok(Value::from(false)));
    }

    #[test]
    fn test_const_null() {
        let mut agent = get_agent!();
        let mut interpreter = Interpreter::new(&mut agent);

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_null().end_module();

        let result = interpreter.evaluate(bytecode.into());
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

        let result = interpreter.evaluate(code);
        assert_eq!(result, Ok(Value::from("hello world")));
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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_pop() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_int(123).pop().end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_load_local() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(123)
            .const_double(432.0)
            .load_local(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_store_local() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .const_int(123)
            .const_int(234)
            .store_local(0)
            .pop()
            .load_local(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(27)));
    }

    #[test]
    fn test_new_function() {
        let mut agent = get_agent!();

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
            .usize(0)
            .address_of("func")
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(999)));
    }

    #[test]
    fn test_bind_local() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("func")
            .load_upvalue(0)
            .ret()
            .label("main")
            .const_int(123)
            .op(OpCode::NewFunction)
            .usize(0)
            .address_of("func")
            .bind_local(0)
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_bind_upvalue() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("func1")
            .op(OpCode::NewFunction)
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
            .usize(0)
            .address_of("func1")
            .bind_local(0)
            .call(0)
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

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
            .usize(1)
            .address_of("func1")
            .call(1)
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(2334)));
    }

    #[test]
    fn test_load_upvalue() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode
            .init_module(0)
            .op(OpCode::Jump)
            .address_of("main")
            .label("test")
            .load_upvalue(0)
            .ret()
            .label("main")
            .const_string(agent.intern_string("hello"))
            .op(OpCode::NewFunction)
            .usize(0)
            .address_of("test")
            .bind_local(0)
            .call(0)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from("hello")));
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
            .usize(0)
            .address_of("func_a")
            .bind_local(0)
            .store_global(a)
            .op(OpCode::NewFunction)
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
        let result = interpreter.evaluate(code);

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
            .usize(1)
            .address_of("func")
            .call(1)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from("hullo")));
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
            .usize(1)
            .address_of("func")
            .call(1)
            .end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(3)));
    }

    #[test]
    fn test_new_array() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).new_array(10).end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(-1)));
    }

    #[test]
    fn test_not() {
        let mut agent = get_agent!();

        let mut bytecode = Bytecode::new();
        bytecode.init_module(0).const_true().not().end_module();

        let code = bytecode.into();
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

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
        let result = interpreter.evaluate(code);

        assert_eq!(result, Ok(Value::from(2)));
    }
}
