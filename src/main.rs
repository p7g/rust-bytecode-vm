#![allow(dead_code)] // FIXME: enable this again once things are stable

mod agent;
#[macro_use]
mod bytecode;
mod code_object;
mod compiler;
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

fn array_new(_: &mut Interpreter, args: Vec<Value>) -> Value {
    if let Some(Value::Integer(n)) = args.get(0) {
        Value::from(vec![Value::Null; *n as usize])
    } else {
        panic!("Expected int");
    }
}

fn main() -> Result<(), String> {
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

    global.insert(
        agent.intern_string("array_new"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("array_new")),
            arity: 1,
            function: array_new,
        }),
    );

    let code = {
        let lexer = parser::Lexer::new(
            r#"
function fib_slow(n) {
    let seen = array_new(n + 1);
    seen[0] = 1;
    seen[1] = 1;

    function do_fib(n) {
        if seen[n] != null {
            return seen[n];
        }
        let less1 = do_fib(n - 1);
        let less2 = do_fib(n - 2);
        seen[n - 1] = less1;
        seen[n - 2] = less2;
        return less1 + less2;
    }

    return do_fib(n);
}

function fib_fast(n) {
    let a = 1;
    let b = 1;

    for let cursor = 0; cursor <= n; cursor = cursor + 1 {
        if cursor == 0 {
            continue;
        }
        if cursor == 1 {
            continue;
        }

        if cursor % 2 == 0 {
            b = a + b;
        } else {
            a = a + b;
        }
    }

    if a > b {
        return a;
    }
    return b;
}

print(fib_slow(46));
"#,
        );
        let parser = parser::Parser::new(&mut agent, lexer);
        let statements = parser.collect::<Result<Vec<_>, String>>()?;
        let compiler = compiler::Compiler::new();
        compiler.compile(statements.iter())?
    };

    let code_object = CodeObject::new(code);
    // disassemble(&agent, &code_object)?;
    let mut interpreter = Interpreter::with_global(&mut agent, global);
    interpreter.evaluate(code_object)?;

    Ok(())
}
