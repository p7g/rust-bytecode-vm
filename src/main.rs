mod agent;
mod code_object;
mod interpreter;
mod module;
mod opcode;
mod value;

use agent::Agent;
use code_object::CodeObject;
use interpreter::Interpreter;
use opcode::OpCode;
use value::Value;

fn main() -> Result<(), String> {
    let agent = Agent::new();
    let mut interpreter = Interpreter::new(agent);

    let mut instructions = vec![OpCode::ConstInt.into()];
    instructions.extend_from_slice(&1i64.to_le_bytes());
    let code_object = CodeObject::new(instructions);

    interpreter.evaluate(code_object)
}
