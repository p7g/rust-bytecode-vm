mod agent;
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

    let mut instructions = vec![OpCode::ConstString.into()];
    instructions.extend_from_slice(&agent.intern_string("hello world").to_le_bytes());
    instructions.push(OpCode::ConstNull.into());
    instructions.push(OpCode::ConstTrue.into());
    instructions.push(OpCode::ConstFalse.into());
    instructions.push(OpCode::ConstDouble.into());
    instructions.extend_from_slice(&1.23f64.to_bits().to_le_bytes());
    instructions.push(OpCode::ConstInt.into());
    instructions.extend_from_slice(&7474i64.to_le_bytes());
    instructions.push(OpCode::Noop.into());
    let code_object = CodeObject::new(instructions);

    disassemble(&agent, &code_object)?;

    let mut interpreter = Interpreter::new(&mut agent);
    interpreter.evaluate(code_object)
}
