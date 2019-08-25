use std::convert::{TryFrom, TryInto};
use std::ops::{Add, Div, Mul, Rem, Sub};

use crate::agent::Agent;
use crate::code_object::CodeObject;
use crate::opcode::OpCode;
use crate::value::Value;

pub struct Interpreter<'a> {
    agent: &'a mut Agent<'a>,
    stack: Vec<Value<'a>>,
}

impl<'a> Interpreter<'a> {
    pub fn new(agent: &'a mut Agent<'a>) -> Interpreter<'a> {
        Interpreter {
            agent,
            stack: Vec::new(),
        }
    }

    pub fn evaluate(&mut self, code_object: CodeObject) -> Result<Value<'a>, String> {
        let mut ip = 0;

        macro_rules! push {
            ($expr:expr) => {
                self.stack.push($expr)
            };
        }
        macro_rules! pop {
            () => {
                self.stack.pop().ok_or("Stack underflow")?
            };
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
                number_binop!($name, $intop, $doubleop, i64)
            };
            ($name:expr, $intop:expr, $doubleop:expr, $btype:ty) => {{
                let b = pop!();
                let a = pop!();

                push!(if let Value::Integer(a) = a {
                    if let Value::Integer(b) = b {
                        let b: $btype = b.try_into().map_err(|_| "Integer overflow".to_string())?;
                        Value::from($intop(a, b))
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
            match OpCode::try_from(instruction)? {
                OpCode::Noop => {}

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
                OpCode::Exp => number_binop!("exponentiation", i64::pow, f64::powf, u32),
            }
        }

        Ok(if let Some(value) = self.stack.pop() {
            value
        } else {
            Value::Null
        })
    }
}
