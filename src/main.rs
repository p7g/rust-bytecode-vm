#![allow(dead_code)] // FIXME: enable this again once things are stable

mod agent;
#[macro_use]
mod bytecode;
mod code_object;
mod disassemble;
mod interpreter;
mod module;
mod opcode;
mod parser;
mod value;

use std::collections::HashMap;

use agent::Agent;
#[macro_use]
use bytecode::Bytecode;
use code_object::CodeObject;
use disassemble::disassemble;
use interpreter::Interpreter;
use value::{FunctionValue, Value};

fn to_string(_: &mut Interpreter, args: Vec<Value>) -> Value {
    Value::from(format!("{}", args[0]))
}

fn type_of(_: &mut Interpreter, args: Vec<Value>) -> Value {
    Value::from(args[0].type_of())
}

fn print(_: &mut Interpreter, args: Vec<Value>) -> Value {
    let mut s = String::new();

    for (i, v) in args.iter().map(|v| format!("{}", v)).enumerate() {
        s.push_str(&v);
        if i < args.len() - 1 {
            s.push(' ');
        }
    }

    println!("{}", s);

    Value::Null
}

fn main() -> Result<(), String> {
    let mut agent = Agent::new();

    let bytecode = bytecode! {
        const_int 3
        const_int 4
        load_global (agent.intern_string("print"))
        call 2
        jump_if_true 0
        const_int 2
        load_global (agent.intern_string("to_string"))
        call 1
        jump_if_false 0
        const_int 1
        load_global (agent.intern_string("type_of"))
        call 1
    };

    let code_object = CodeObject::new(bytecode.into());

    disassemble(&agent, &code_object)?;

    let mut global = HashMap::new();

    global.insert(
        agent.intern_string("print"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("print")),
            arity: 1,
            function: print,
        }),
    );

    global.insert(
        agent.intern_string("to_string"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("to_string")),
            arity: 1,
            function: to_string,
        }),
    );

    global.insert(
        agent.intern_string("type_of"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("type_of")),
            arity: 1,
            function: type_of,
        }),
    );

    let mut interpreter = Interpreter::with_global(&mut agent, global);
    let result = interpreter.evaluate(code_object)?;

    println!("\n{:?}", result);

    Ok(())
}
