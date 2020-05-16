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

fn tostring(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    Ok(Value::from(format!("{}", args[0])))
}

fn type_of(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    Ok(Value::from(args[0].type_of()))
}

fn print(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    print!("{}", args[0]);
    Ok(Value::Null)
}

fn println(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    let mut s = String::new();

    for (i, v) in args.iter().map(|v| format!("{}", v)).enumerate() {
        s.push_str(&v);
        if i < args.len() - 1 {
            s.push(' ');
        }
    }

    println!("{}", s);

    Ok(Value::Null)
}

fn array_new(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::Integer(n)) = args.get(0) {
        Ok(Value::from(vec![Value::Null; *n as usize]))
    } else {
        Err("array_new: Expected int".to_string())
    }
}

fn string_chars(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::String(s)) = args.get(0) {
        Ok(Value::from(
            s.chars()
                .map(|c| Value::from(c.to_string()))
                .collect::<Vec<_>>(),
        ))
    } else {
        Err("string_chars: Expected string".to_string())
    }
}

fn string_bytes(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::String(s)) = args.get(0) {
        Ok(Value::from(
            s.bytes()
                .map(|b| Value::from(i64::from(b)))
                .collect::<Vec<_>>(),
        ))
    } else {
        Err("string_bytes: Expected string".to_string())
    }
}

fn string_concat(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    let mut buf = String::new();

    for arg in args {
        if let Value::String(s) = arg {
            buf += &s;
        } else {
            return Err("string_concat: Expected string".to_string());
        }
    }

    Ok(Value::String(buf))
}

fn ord(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::String(s)) = args.get(0) {
        if let Some(c) = s.chars().next() {
            Ok(Value::from(c as i64))
        } else {
            Err(format!("ord: Expected string with length 1, got {:?}", s))
        }
    } else {
        Err("ord: Expected string with length 1".to_string())
    }
}

fn chr(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::Integer(n)) = args.get(0) {
        Ok(Value::from((*n as u8 as char).to_string()))
    } else {
        Err("chr: Expected integer".to_string())
    }
}

fn array_length(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::Array(vs)) = args.get(0) {
        Ok(Value::from(vs.borrow().len() as i64))
    } else {
        Err(format!("array_length: Expected array, get {:#?}", args))
    }
}

fn truncate32(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::Integer(i)) = args.get(0) {
        Ok(Value::from(i64::from(*i as u32)))
    } else {
        Err("truncate32: Expected integer".to_string())
    }
}

fn read_file(_: &mut Interpreter, args: Vec<Value>) -> Result<Value, String> {
    if let Some(Value::String(s)) = args.first() {
        Ok(Value::from(std::fs::read_to_string(s).map_err(|_| "Failed to read file".to_string())?))
    } else {
        Err("read_file: Expected string".to_string())
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
    interpreter.evaluate(code.unwrap());

    Ok(())
}
