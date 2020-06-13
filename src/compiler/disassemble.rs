use crate::agent::Agent;
use crate::opcode::OpCode;
use std::convert::TryFrom;

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
        eprint!("{}: ", ip - 1);
        let instruction = OpCode::from(instruction);
        match instruction {
            OpCode::ConstInt => {
                eprintln!("{:?}({:?})", instruction, i64::from_le_bytes(next!(usize)));
            }

            OpCode::ConstDouble => {
                eprintln!(
                    "{:?}({:?})",
                    instruction,
                    f64::from_bits(u64::from_le_bytes(next!(usize))),
                );
            }

            OpCode::ConstChar => {
                eprintln!(
                    "{:?}({:?})",
                    instruction,
                    char::try_from(usize::from_le_bytes(next!(usize)) as u32).unwrap()
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
            | OpCode::NewArrayWithValues
            | OpCode::AllocateLocals
            | OpCode::InitModule
            | OpCode::Export => {
                eprintln!(
                    "{:?}({:?})",
                    instruction,
                    usize::from_le_bytes(next!(usize))
                );
            }

            OpCode::LoadGlobal
            | OpCode::DeclareGlobal
            | OpCode::StoreGlobal
            | OpCode::ConstString => {
                let idx = usize::from_le_bytes(next!(usize));
                eprintln!("{:?}({:?} ({}))", instruction, agent.string_table[idx], idx,);
            }

            OpCode::LoadFromModule => {
                eprintln!(
                    "{:?}({}, {})",
                    instruction,
                    usize::from_le_bytes(next!(usize)),
                    usize::from_le_bytes(next!(usize)),
                );
            }

            OpCode::NewFunction => {
                eprintln!(
                    "{:?}({:?}, {:?}, {:?})",
                    instruction,
                    usize::from_le_bytes(next!(usize)),
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
            | OpCode::Dup => eprintln!("{:?}", instruction),
        }
    }

    Ok(())
}
