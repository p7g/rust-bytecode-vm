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

    let code = {
        let lexer = parser::Lexer::new(
            r#"
function factorial(n) {
    if n == 1 {
        return 1;
    }
    if n == 2 {
        return 2;
    }
    return n * factorial(n - 1);
}

print(factorial(10));
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
