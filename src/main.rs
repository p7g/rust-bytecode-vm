#![allow(dead_code)] // FIXME: enable this again once things are stable

mod agent;
#[macro_use]
mod compiler;
mod interpreter;
mod module;
mod opcode;
mod value;

use std::collections::HashMap;

use agent::Agent;
use compiler::Compiler;
use interpreter::Interpreter;
use value::{FunctionValue, Value};

fn tostring(_: &mut Interpreter, args: Vec<Value>) -> Value {
    Value::from(format!("{}", args[0]))
}

fn type_of(_: &mut Interpreter, args: Vec<Value>) -> Value {
    Value::from(args[0].type_of())
}

fn print(_: &mut Interpreter, args: Vec<Value>) -> Value {
    print!("{}", args[0]);
    Value::Null
}

fn println(_: &mut Interpreter, args: Vec<Value>) -> Value {
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

fn array_new(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Integer(n)) = args.get(0) {
        Value::from(vec![Value::Null; *n as usize])
    } else {
        panic!("Expected int");
    }
}

fn string_chars(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::String(s)) = args.get(0) {
        Value::from(
            s.chars()
                .map(|c| Value::from(c.to_string()))
                .collect::<Vec<_>>(),
        )
    } else {
        panic!("Expected string");
    }
}

fn string_bytes(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::String(s)) = args.get(0) {
        Value::from(
            s.bytes()
                .map(|b| Value::from(i64::from(b)))
                .collect::<Vec<_>>(),
        )
    } else {
        panic!("Expected string");
    }
}

fn ord(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::String(s)) = args.get(0) {
        if let Some(c) = s.chars().next() {
            Value::from(c as i64)
        } else {
            panic!("Expected string with length 1, got {:?}", s);
        }
    } else {
        panic!("Expected string with length 1");
    }
}

fn chr(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Integer(n)) = args.get(0) {
        Value::from((*n as u8 as char).to_string())
    } else {
        panic!("Expected integer");
    }
}

fn array_length(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Array(vs)) = args.get(0) {
        Value::from(vs.borrow().len() as i64)
    } else {
        panic!("Expected array");
    }
}

fn truncate32(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Integer(i)) = args.get(0) {
        Value::from(i64::from(*i as u32))
    } else {
        panic!("Expected integer");
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut agent = Agent::new();
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
        agent.intern_string("println"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("println")),
            arity: 1,
            function: println,
        }),
    );

    global.insert(
        agent.intern_string("tostring"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("tostring")),
            arity: 1,
            function: tostring,
        }),
    );

    global.insert(
        agent.intern_string("typeof"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("typeof")),
            arity: 1,
            function: type_of,
        }),
    );

    global.insert(
        agent.intern_string("array_new"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("array_new")),
            arity: 1,
            function: array_new,
        }),
    );

    global.insert(
        agent.intern_string("array_length"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("array_length")),
            arity: 1,
            function: array_length,
        }),
    );

    global.insert(
        agent.intern_string("string_chars"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("string_chars")),
            arity: 1,
            function: string_chars,
        }),
    );

    global.insert(
        agent.intern_string("string_bytes"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("string_bytes")),
            arity: 1,
            function: string_bytes,
        }),
    );

    global.insert(
        agent.intern_string("chr"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("chr")),
            arity: 1,
            function: chr,
        }),
    );

    global.insert(
        agent.intern_string("ord"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("ord")),
            arity: 1,
            function: ord,
        }),
    );

    global.insert(
        agent.intern_string("truncate32"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("truncate32")),
            arity: 1,
            function: truncate32,
        }),
    );

    let mut compiler = Compiler::new(&mut agent);

    let args = std::env::args().collect::<Vec<_>>();
    let filename = args.get(1).expect("Expected filename");

    compiler.compile_file(std::env::current_dir()?, filename)?;
    let code = compiler.code();

    let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
    interpreter.evaluate(code.unwrap())?;

    Ok(())
}
