#![allow(dead_code)] // FIXME: enable this again once things are stable

mod agent;
mod compiler;
mod debuginfo;
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
        panic!("array_new: Expected int");
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
        panic!("string_chars: Expected string");
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
        panic!("string_bytes: Expected string");
    }
}

fn string_concat(_: &mut Interpreter, args: Vec<Value>) -> Value {
    let mut buf = String::new();

    for arg in args {
        if let Value::String(s) = arg {
            buf += &s;
        } else {
            panic!("string_concat: Expected string");
        }
    }

    Value::String(buf)
}

fn ord(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::String(s)) = args.get(0) {
        if let Some(c) = s.chars().next() {
            Value::from(c as i64)
        } else {
            panic!("ord: Expected string with length 1, got {:?}", s);
        }
    } else {
        panic!("ord: Expected string with length 1");
    }
}

fn chr(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Integer(n)) = args.get(0) {
        Value::from((*n as u8 as char).to_string())
    } else {
        panic!("chr: Expected integer");
    }
}

fn array_length(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Array(vs)) = args.get(0) {
        Value::from(vs.borrow().len() as i64)
    } else {
        panic!("array_length: Expected array");
    }
}

fn truncate32(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Integer(i)) = args.get(0) {
        Value::from(i64::from(*i as u32))
    } else {
        panic!("truncate32: Expected integer");
    }
}

fn read_file(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::String(s)) = args.first() {
        Value::from(std::fs::read_to_string(s).expect("Failed to read file"))
    } else {
        panic!("read_file: Expected string");
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut agent = Agent::new();
    let mut global = HashMap::new();

    macro_rules! add_global {
        ($name:ident, $arity:expr) => {{
            global.insert(
                agent.intern_string(stringify!($name)),
                Value::Function(FunctionValue::Builtin {
                    name: Some(agent.intern_string(stringify!($name))),
                    arity: $arity,
                    function: $name,
                }),
            );
        }};
    }

    add_global!(print, 1);
    add_global!(println, 1);
    add_global!(tostring, 1);
    add_global!(type_of, 1);
    add_global!(array_new, 1);
    add_global!(array_length, 1);
    add_global!(string_chars, 1);
    add_global!(string_bytes, 1);
    add_global!(string_concat, 0);
    add_global!(chr, 1);
    add_global!(ord, 1);
    add_global!(truncate32, 1);
    add_global!(read_file, 1);

    let mut compiler = Compiler::new(&mut agent);

    let args = std::env::args().collect::<Vec<_>>();
    let filename = args.get(1).expect("Expected filename");

    compiler.compile_file(std::env::current_dir()?, filename)?;
    let (code, debuginfo) = compiler.end();

    let mut interpreter = Interpreter::with_intrinsics(&mut agent, global);
    interpreter.set_debuginfo(&debuginfo);
    interpreter.evaluate(code.unwrap())?;

    Ok(())
}
