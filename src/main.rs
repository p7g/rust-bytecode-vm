#![allow(dead_code)] // FIXME: enable this again once things are stable

mod agent;
mod compiler;
mod debuginfo;
mod interpreter;
mod intrinsics;
mod module;
mod opcode;
mod value;

use std::collections::HashMap;

use agent::Agent;
use compiler::Compiler;
use interpreter::Interpreter;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut agent = Agent::new();
    let mut global = HashMap::new();

    intrinsics::initialize_global_scope(&mut agent, &mut global);

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
