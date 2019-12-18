use crate::agent::Agent;
use crate::opcode::OpCode;

pub fn disassemble(agent: &Agent, code: &[u8]) -> Result<(), String> {
    let mut ip = 0;

    macro_rules! next {
        () => {{
            let inst = code.get(ip).cloned();
            ip += 1;
            inst
        }};
        ($type:ty) => {
            next!(std::mem::size_of::<$type>())
        };
        ($count:expr) => {{
            let mut arr = [0u8; $count];

            for i in 0..$count {
                let result = next!().ok_or_else(|| "Unexpected end of bytecode".to_string());
                arr[i] = result?;
            }

            arr
        }};
    }

    while let Some(instruction) = next!() {
        print!("{}: ", ip - 1);
        let instruction = OpCode::from(instruction);
        match instruction {
            OpCode::ConstInt => {
                println!("{:?}({:?})", instruction, i64::from_le_bytes(next!(usize)));
            }

            OpCode::ConstDouble => {
                println!(
                    "{:?}({:?})",
                    instruction,
                    f64::from_bits(u64::from_le_bytes(next!(usize))),
                );
            }

            OpCode::Jump
            | OpCode::JumpIfTrue
            | OpCode::JumpIfFalse
            | OpCode::Call
            | OpCode::LoadLocal
            | OpCode::StoreLocal
            | OpCode::BindLocal
            | OpCode::BindUpvalue
            | OpCode::BindArgument
            | OpCode::LoadUpvalue
            | OpCode::StoreUpvalue
            | OpCode::LoadArgument
            | OpCode::StoreArgument
            | OpCode::NewArray
            | OpCode::NewArrayWithValues => {
                println!("{:?}({:?})", instruction, usize::from_le_bytes(next!(8)));
            }

            OpCode::LoadGlobal
            | OpCode::DeclareGlobal
            | OpCode::StoreGlobal
            | OpCode::ConstString
            | OpCode::InitModule => {
                let idx = usize::from_le_bytes(next!(usize));
                println!("{:?}({} ({}))", instruction, agent.string_table[idx], idx,);
            }

            OpCode::LoadFromModule => {
                println!(
                    "{:?}({}.{})",
                    instruction,
                    agent.string_table[usize::from_le_bytes(next!(usize))],
                    agent.string_table[usize::from_le_bytes(next!(usize))],
                );
            }

            OpCode::NewFunction => {
                println!(
                    "{:?}({:?}, {:?})",
                    instruction,
                    usize::from_le_bytes(next!(usize)),
                    usize::from_le_bytes(next!(usize)),
                );
            }

            OpCode::Halt
            | OpCode::ConstTrue
            | OpCode::ConstFalse
            | OpCode::ConstNull
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::Mod
            | OpCode::Exp
            | OpCode::Return
            | OpCode::Pop
            | OpCode::ArrayGet
            | OpCode::ArraySet
            | OpCode::Equal
            | OpCode::NotEqual
            | OpCode::LessThan
            | OpCode::LessThanEqual
            | OpCode::GreaterThan
            | OpCode::GreaterThanEqual
            | OpCode::BitwiseAnd
            | OpCode::BitwiseOr
            | OpCode::BitwiseXor
            | OpCode::BitwiseNot
            | OpCode::Not
            | OpCode::LeftShift
            | OpCode::RightShift
            | OpCode::Neg
            | OpCode::EndModule
            | OpCode::Dup => println!("{:?}", instruction),
        }
    }

    Ok(())
}
