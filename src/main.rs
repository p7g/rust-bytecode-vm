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
        agent.intern_string("println"),
        Value::Function(FunctionValue::Builtin {
            name: Some(agent.intern_string("println")),
            arity: 1,
            function: println,
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

    let code = {
        let lexer = parser::Lexer::new(
            r#"
function box(value) {
    return [value];
}

function unbox(box) {
    return box[0];
}

function box_set(box, new_value) {
    box[0] = new_value;
}

function array_find(array, func) {
    let len = array_length(array);
    for let i = 0; i < len; i = i + 1 {
        if func(array[i], i, array) {
            return array[i];
        }
    }
    return null;
}

function array_contains(array, thing) {
    return null != array_find(array, function(i) {
        return i == thing;
    });
}

function linked_list_new() {
    return box(null);
}

function linked_list_length(list) {
    let current = unbox(list);
    let len = 0;

    while current {
        len = len + 1;
        current = current[1];
    }

    return len;
}

function linked_list_prepend(list, value) {
    let current = unbox(list);
    box_set(list, [value, current]);
}

function linked_list_append(list, value) {
    let current = unbox(list);
    let prev = null;
    if current == null {
        box_set(list, [value, null]);
        return;
    }

    while current {
        prev = current;
        current = current[1];
    }

    prev[1] = [value, null];
}

function linked_list_foreach(list, fn) {
    let current = unbox(list);

    let i = 0;
    while current {
        fn(current[0], i);
        current = current[1];
        i = i + 1;
    }
}

function linked_list_find(list, fn) {
    let current = unbox(list);

    while current {
        if fn(current[0]) {
            return current;
        }
    }

    return null;
}

function linked_list_reverse(list) {
    let new_list = linked_list_new();
    linked_list_foreach(list, function(i) {
        linked_list_prepend(new_list, i);
    });
    return new_list;
}

function linked_list_to_array(list) {
    let arr = array_new(linked_list_length(list));
    linked_list_foreach(list, function(elem, i) {
        arr[i] = elem;
    });
    return arr;
}

let DYNAMIC_ARRAY_INITIAL_SIZE = 16;

function dynamic_array_new() {
    return [0, array_new(DYNAMIC_ARRAY_INITIAL_SIZE)];
}

function dynamic_array_push(dynarray, value) {
    let next_idx = dynarray[0];
    let array = dynarray[1];
    let len = array_length(array);
    if next_idx >= len {
        let new_array = array_new(2 * len);
        for let i = 0; i < next_idx; i = i + 1 {
            new_array[i] = array[i];
        }
        dynarray[1] = array = new_array;
    }
    array[next_idx] = value;
    dynarray[0] = next_idx + 1;
}

function dynamic_array_pop(dynarray) {
    if dynarray[0] <= 0 {
        return null;
    }
    dynarray[0] = dynarray[0] - 1;
    return dynarray[1][dynarray[0]];
}

function dynamic_array_foreach(dynarray, func) {
    let next_idx = dynarray[0];
    let array = dynarray[1];

    for let i = 0; i < next_idx; i = i + 1 {
        func(array[i], i, array);
    }
}

function dynamic_array_find(dynarray, func) {
    let next_idx = dynarray[0];
    let array = dynarray[1];

    for let i = 0; i < next_idx; i = i + 1 {
        if func(array[i], i, array) {
            return array[i];
        }
    }

    return null;
}

function dynamic_array_set(dynarray, n, value) {
    dynarray[1][n] = value;
}

function dynamic_array_get(dynarray, n) {
    return dynarray[1][n];
}

function dynamic_array_length(dynarray) {
    return dynarray[0];
}

function dynamic_array_to_array(dynarray) {
    let len = dynarray[0];
    let array = array_new(len);
    for let i = 0; i < len; i = i + 1 {
        array[i] = dynarray[1][i];
    }
    return array;
}

function assoc_list_new() {
    return dynamic_array_new();
}

function assoc_list_set(list, key, value) {
    let found = dynamic_array_find(list, function(x, i) {
        if x[0] == key {
            return true;
        }
    });

    if found == null {
        dynamic_array_push(list, [key, value]);
    } else {
        found[1] = value;
    }
}

function assoc_list_get(list, key) {
    let found = dynamic_array_find(list, function(x) {
        if x[0] == key {
            return true;
        }
    });

    if found != null {
        return found[1];
    }
    return null;
}

let FNV_OFFSET_BASIS_32 = 2166136261;
let FNV_PRIME_32 = 16777619;

function fnv1a(s) {
    let bytes = string_bytes(s);
    let len = array_length(bytes);
    let hash = FNV_OFFSET_BASIS_32;

    for let i = 0; i < len; i = i + 1 {
        hash = hash ^ bytes[i];
        hash = hash * FNV_PRIME_32;
    }

    return hash;
}

let HASHTABLE_INITIAL_SIZE = 1024;

function hashtable_new() {
    return [
        0,
        array_new(HASHTABLE_INITIAL_SIZE),
    ];
}

function hashtable_insert(table, key, value) {}

let arr = dynamic_array_new();

for let i = 0; i < 10000; i = i + 1 {
    dynamic_array_push(arr, to_string(i));
}

print(dynamic_array_to_array(arr));
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
