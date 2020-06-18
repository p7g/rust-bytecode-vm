use crate::agent::Agent;
use crate::value::{FunctionValue, Value};
use std::collections::HashMap;
use std::io::{self, Write};
use std::rc::Rc;

fn tostring(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    Ok(Value::from(args[0].to_string(agent)))
}

fn type_of(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    Ok(Value::from(args[0].type_of()))
}

fn print(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    print!("{}", args[0].to_string(agent));
    io::stdout().flush().map_err(|_| "Failed to flush stdout")?;
    Ok(Value::Null)
}

fn println(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    let mut s = String::new();

    for (i, v) in args.iter().map(|v| v.to_string(agent)).enumerate() {
        s.push_str(&v);
        if i < args.len() - 1 {
            s.push(' ');
        }
    }

    println!("{}", s);

    Ok(Value::Null)
}

fn array_new(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Integer(n)) = args.get(0) {
        Ok(Value::from(vec![Value::Null; *n as usize]))
    } else {
        Err("array_new: Expected int".to_string())
    }
}

fn string_chars(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    let cs = match &args[0] {
        Value::String(s) => s.chars(),
        Value::InternedString(id) => agent.string_table[*id].chars(),
        _ => return Err("string_chars: Expected string".to_string()),
    };
    Ok(Value::from(cs.map(Value::from).collect::<Vec<_>>()))
}

fn string_from_chars(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Array(cs)) = args.get(0) {
        let s: String = cs
            .borrow()
            .iter()
            .filter_map(|c| match c {
                Value::Char(c) => Some(*c),
                _ => None,
            })
            .collect();

        Ok(Value::from(s))
    } else {
        Err("string_from_chars: Expected array of chars".to_string())
    }
}

fn string_bytes(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    let bs = match &args[0] {
        Value::String(s) => s.bytes(),
        Value::InternedString(id) => agent.string_table[*id].bytes(),
        _ => return Err("string_bytes: Expected string".to_string()),
    };
    Ok(Value::from(
        bs.map(|b| Value::from(i64::from(b))).collect::<Vec<_>>(),
    ))
}

fn string_concat(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    let buf: String = args
        .iter()
        .map(|arg| {
            if let Value::String(s) = arg {
                Ok((*s).as_str())
            } else if let Value::InternedString(id) = arg {
                Ok(agent.string_table[*id].as_str())
            } else {
                Err("string_concat: Expected string".to_string())
            }
        })
        .collect::<Result<_, _>>()?;

    Ok(Value::String(Rc::new(buf)))
}

fn ord(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Char(c)) = args.get(0) {
        Ok(Value::from(*c as i64))
    } else {
        Err("ord: Expected char".to_string())
    }
}

fn chr(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Integer(n)) = args.get(0) {
        Ok(Value::from(*n as u8 as char))
    } else {
        Err("chr: Expected integer".to_string())
    }
}

fn array_length(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Array(vs)) = args.get(0) {
        Ok(Value::from(vs.borrow().len() as i64))
    } else {
        Err(format!("array_length: Expected array, get {:#?}", args))
    }
}

fn truncate32(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Integer(i)) = args.get(0) {
        Ok(Value::from(i64::from(*i as u32)))
    } else {
        Err("truncate32: Expected integer".to_string())
    }
}

fn tofloat(_agent: &Agent, args: &[Value]) -> Result<Value, String> {
    if let Some(Value::Integer(i)) = args.first() {
        Ok(Value::Double(*i as f64))
    } else {
        Err("tofloat: Expected integer".to_string())
    }
}

fn read_file(agent: &Agent, args: &[Value]) -> Result<Value, String> {
    let s = match &args[0] {
        Value::String(s) => s.as_ref(),
        Value::InternedString(id) => &agent.string_table[*id],
        _ => return Err("read_file: Expected string".to_string()),
    };
    Ok(Value::from(
        std::fs::read_to_string(&**s).map_err(|_| "Failed to read file".to_string())?,
    ))
}

fn argv(_agent: &Agent, _args: &[Value]) -> Result<Value, String> {
    Ok(Value::from(std::env::args().collect::<Vec<_>>()))
}

pub(crate) fn initialize_global_scope(agent: &mut Agent, global: &mut HashMap<usize, Value>) {
    macro_rules! add_global {
        ($name:ident, $arity:expr) => {{
            global.insert(
                agent.intern_string(stringify!($name)),
                Value::Function(Rc::new(FunctionValue::Builtin {
                    name: Some(agent.intern_string(stringify!($name))),
                    arity: $arity,
                    function: $name,
                })),
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
    add_global!(string_from_chars, 1);
    add_global!(string_bytes, 1);
    add_global!(string_concat, 0);
    add_global!(chr, 1);
    add_global!(ord, 1);
    add_global!(truncate32, 1);
    add_global!(tofloat, 1);
    add_global!(read_file, 1);
}

pub(crate) fn make_intrinsics(agent: &mut Agent) -> HashMap<usize, Value> {
    let mut global = HashMap::new();
    initialize_global_scope(agent, &mut global);
    global
}
