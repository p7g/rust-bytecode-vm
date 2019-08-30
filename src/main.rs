mod agent;
#[macro_use]
mod bytecode;
mod code_object;
mod disassemble;
mod interpreter;
mod module;
mod opcode;
mod value;

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
        call 1
        jump_if_false 0
        call 1
        jump_if_false 0
        call 2
    };

    let code_object = CodeObject::new(bytecode.into());

    disassemble(&agent, &code_object)?;

    let builtin_print = Value::Function(FunctionValue::Builtin {
        name: Some(agent.intern_string("print")),
        arity: 1,
        function: print,
    });

    let builtin_to_string = Value::Function(FunctionValue::Builtin {
        name: Some(agent.intern_string("to_string")),
        arity: 1,
        function: to_string,
    });

    let builtin_type_of = Value::Function(FunctionValue::Builtin {
        name: Some(agent.intern_string("type_of")),
        arity: 1,
        function: type_of,
    });

    let stack = vec![
        Value::from(3),
        Value::from(4),
        builtin_print,
        Value::from(2),
        builtin_to_string,
        Value::from(1),
        builtin_type_of,
    ];
    let mut interpreter = Interpreter::new(&mut agent);
    let result = interpreter.evaluate(stack, code_object)?;

    println!("\n{:?}", result);

    Ok(())
}
