mod agent;
mod bytecode;
mod code_object;
mod disassemble;
mod interpreter;
mod module;
mod opcode;
mod value;

use agent::Agent;
use code_object::CodeObject;
use disassemble::disassemble;
use interpreter::Interpreter;
use opcode::OpCode;
use value::Value;

fn main() -> Result<(), String> {
    let mut agent = Agent::new();

    let mut instructions = Vec::new();
    instructions.push(OpCode::ConstInt.into());
    instructions.extend_from_slice(&7474i64.to_le_bytes());
    instructions.push(OpCode::ConstDouble.into());
    instructions.extend_from_slice(&1.23f64.to_bits().to_le_bytes());
    instructions.push(OpCode::Exp.into());

    let code_object = CodeObject::new(instructions);

    disassemble(&agent, &code_object)?;

    let mut interpreter = Interpreter::new(&mut agent);
    let result = interpreter.evaluate(code_object)?;

    println!("\n{:?}", result);

    Ok(())
}
