use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::agent::Agent;
use crate::bytecode::Bytecode;
use crate::code_object::CodeObject;
use crate::opcode::OpCode;
use crate::value::{FunctionValue, Value};

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
    agent: &'a mut Agent<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new(agent: &'a mut Agent<'a>) -> Interpreter<'a> {
        Interpreter { agent }
    }

    pub fn evaluate(
        &mut self,
        mut stack: Vec<Value>,
        code_object: CodeObject,
    ) -> Result<Value, String> {
        let mut call_stack: Vec<usize> = Vec::new();
        let mut ip = 0;
        let mut bp = 0;
        let mut sp = 0;

        macro_rules! push {
            ($expr:expr) => {{
                sp += 1;
                stack.push($expr)
            }};
        }
        macro_rules! pop {
            () => {{
                sp -= 1;
                stack.pop().ok_or("Stack underflow")?
            }};
        }
        macro_rules! next {
            () => {{
                let inst = code_object.instructions.get(ip);
                ip += 1;
                inst
            }};
            ($expr:expr) => {{
                let mut array = [0u8; $expr];

                for i in 0..$expr {
                    let result: Result<&u8, String> =
                        next!().ok_or("Unexpected end of bytecode".into());
                    let n: u8 = *result?;
                    array[i] = n;
                }

                array
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
                        Value::from($intop(a, ($bconvert)(b)?))
                    } else if let Value::Double(b) = b {
                        Value::from($doubleop(a as f64, b))
                    } else {
                        panic!("Got unexpected value {:?} in {}", b, $name);
                    }
                } else if let Value::Double(a) = a {
                    if let Value::Integer(b) = b {
                        Value::from($doubleop(a, b as f64))
                    } else if let Value::Double(b) = b {
                        Value::from($doubleop(a, b))
                    } else {
                        panic!("Got unexpected value {:?} in {}", b, $name);
                    }
                } else {
                    panic!("Got unexpected value {:?} in {}", a, $name);
                })
            }};
        }

        while let Some(instruction) = next!() {
            if cfg!(vm_debug) {
                println!("--------------");
                print_stack!(&stack);
                println!("{:?}", OpCode::try_from(instruction)?);
                println!("ip: {}", ip);
            }

            match OpCode::try_from(instruction)? {
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
                    push!(Value::from(self.agent.string_table[idx]));
                }

                OpCode::Add => number_binop!("addition", i64::add, f64::add),
                OpCode::Sub => number_binop!("subtraction", i64::sub, f64::sub),
                OpCode::Mul => number_binop!("multiplication", i64::mul, f64::mul),
                OpCode::Div => number_binop!("division", i64::div, f64::div),
                OpCode::Mod => number_binop!("modulus", i64::rem, f64::rem),
                OpCode::Exp => number_binop!(
                    "exponentiation",
                    i64::pow,
                    f64::powf,
                    |b: i64| -> Result<u32, String> {
                        b.try_into().map_err(|_| "Integer overflow".to_string())
                    }
                ),

                OpCode::Jump => {
                    ip = usize::from_le_bytes(next!(8));
                }

                OpCode::JumpIfTrue => {
                    let to = usize::from_le_bytes(next!(8));
                    let cond = pop!();
                    if cond.is_truthy() {
                        ip = to;
                    }
                }

                OpCode::JumpIfFalse => {
                    let to = usize::from_le_bytes(next!(8));
                    let cond = pop!();
                    if !cond.is_truthy() {
                        ip = to;
                    }
                }

                OpCode::Call => {
                    let function = pop!();
                    let num_args = usize::from_le_bytes(next!(8));
                    if let Value::Function(f) = function {
                        macro_rules! ensure_arity {
                            ($arity:expr, $name:expr) => {{
                                if num_args < $arity {
                                    let name = if let Some(name) = $name {
                                        self.agent.string_table[name]
                                    } else {
                                        "<anonymous>"
                                    };
                                    return Err(format!(
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
                                ensure_arity!(arity, name);
                                let mut args = Vec::new();
                                for _ in 0..num_args {
                                    args.push(pop!());
                                }
                                push!(function(self, args));
                            }
                            FunctionValue::User {
                                arity,
                                address,
                                name,
                                ..
                            } => {
                                ensure_arity!(arity, name);
                                unimplemented!();
                            }
                        }
                    } else {
                        return Err(format!("Value {:?} is not callable", function));
                    }
                }

                OpCode::Return => {
                    unimplemented!();
                }
            }
        }

        Ok(if let Some(value) = stack.pop() {
            value
        } else {
            Value::Null
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_halt() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = Bytecode::new().halt().const_true().into();

        let result = interpreter.evaluate(Vec::new(), CodeObject::new(bytecode));
        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_const_int() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = Bytecode::new().const_int(123).into();

        let code = CodeObject::new(bytecode);
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(123)));
    }

    #[test]
    fn test_const_double() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = Bytecode::new().const_double(1.23).into();

        let code = CodeObject::new(bytecode);

        let result = interpreter.evaluate(Vec::new(), code);
        assert_eq!(result, Ok(Value::from(1.23)));
    }

    #[test]
    fn test_const_true() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = Bytecode::new().const_true().into();

        let result = interpreter.evaluate(Vec::new(), CodeObject::new(bytecode));
        assert_eq!(result, Ok(Value::from(true)));
    }

    #[test]
    fn test_const_false() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = Bytecode::new().const_false().into();

        let result = interpreter.evaluate(Vec::new(), CodeObject::new(bytecode));
        assert_eq!(result, Ok(Value::from(false)));
    }

    #[test]
    fn test_const_null() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = Bytecode::new().const_null().into();

        let result = interpreter.evaluate(Vec::new(), CodeObject::new(bytecode));
        assert_eq!(result, Ok(Value::Null));
    }

    #[test]
    fn test_const_string() {
        let mut agent = Agent::new();

        let bytecode = bytecode! {
            const_string (agent) "hello world"
        };

        let mut interpreter = Interpreter::new(&mut agent);
        let code = CodeObject::new(bytecode.into());

        let result = interpreter.evaluate(Vec::new(), code);
        assert_eq!(result, Ok(Value::from("hello world")));
    }

    #[test]
    fn test_add() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 123
            const_double 1.23
            add
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(124.23)));
    }

    #[test]
    fn test_sub() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 123
            const_double 1.23
            sub
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(121.77)));
    }

    #[test]
    fn test_mul() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 123
            const_double 2.0
            mul
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(246f64)));
    }

    #[test]
    fn test_div() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 124
            const_double 2.0
            div
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(62f64)));
    }

    #[test]
    fn test_mod() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 124
            const_double 2.0
            mod
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(0f64)));
    }

    #[test]
    fn test_exp() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 4
            const_int 2
            exp
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(16)));
    }

    #[test]
    fn test_jump() {
        let mut agent = Agent::new();
        let mut interpreter = Interpreter::new(&mut agent);

        let bytecode = bytecode! {
            const_int 4
            jump 29
            const_int 8
            add
            halt
            const_int 12
            mul
            halt
        };

        let code = CodeObject::new(bytecode.into());
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(48)));
    }

    #[test]
    fn test_jump_if_true() {
        let mut agent = Agent::new();

        let bytecode = bytecode! {
            const_int 123
            const_int 234
            const_int 1
            jump_if_true one
            mul
            halt
        one:
            const_false
            jump_if_true two
            add
            halt
        two:
            sub
        };

        let code = CodeObject::new(bytecode.into());
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(357)));
    }

    #[test]
    fn test_jump_if_false() {
        let mut agent = Agent::new();

        let bytecode = bytecode! {
            const_true
            jump_if_false one
            const_int 10
            const_int 2
            const_string (agent) ""
            jump_if_false two
        one:
            add
            halt
        two:
            mul
            halt
        };

        let code = CodeObject::new(bytecode.into());
        let mut interpreter = Interpreter::new(&mut agent);
        let result = interpreter.evaluate(Vec::new(), code);

        assert_eq!(result, Ok(Value::from(20)));
    }
}
