use crate::bytecode::Bytecode;
use crate::opcode::OpCode;
use crate::parser::{Expression, ExpressionKind, Statement, StatementKind};

pub type CompileResult<T> = Result<T, String>;

enum LoopState {
    While {
        start_label: usize,
        end_label: usize,
    },
    For {
        start_label: usize,
        end_label: usize,
        increment_label: usize,
    },
}

struct FunctionState {
    start_label: usize,
    end_label: usize,
    free_variables: Vec<usize>,
}

impl FunctionState {
    pub fn new(start_label: usize, end_label: usize) -> FunctionState {
        FunctionState {
            start_label,
            end_label,
            free_variables: Vec::new(),
        }
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
enum BindingType {
    Argument,
    Upvalue,
    Local,
}

struct Binding {
    typ: BindingType,
    name: usize,
    index: usize,
}

struct Scope<'a> {
    parent: Option<&'a Scope<'a>>,
    // index of vec is the location on the stack where the binding lives
    bindings: Vec<Binding>,
    binding_count: [usize; 3],
}

impl<'a> Scope<'a> {
    pub fn new(parent: Option<&'a Scope<'a>>) -> Scope<'a> {
        Scope {
            parent,
            bindings: Vec::new(),
            binding_count: [0; 3],
        }
    }

    pub fn push_binding(&mut self, typ: BindingType, name: usize) {
        self.bindings.push(Binding {
            name,
            typ,
            index: self.binding_count[typ as usize],
        });
        self.binding_count[typ as usize] += 1;
    }

    pub fn has_binding(&self, name: usize) -> bool {
        self.bindings.iter().rev().any(|b| b.name == name)
    }

    pub fn get_binding(&self, name: usize) -> Option<&Binding> {
        self.bindings.iter().rev().find(|b| b.name == name)
    }
}

struct CompilerState<'a> {
    is_global: bool,
    loop_state: Option<LoopState>,
    function_state: Option<FunctionState>,
    scope: Option<Scope<'a>>,
}

impl<'a> CompilerState<'a> {
    pub fn new(scope: Option<Scope<'a>>) -> CompilerState<'a> {
        CompilerState {
            is_global: true,
            loop_state: None,
            function_state: None,
            scope,
        }
    }
}

pub struct Compiler {
    bytecode: Bytecode,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            bytecode: Bytecode::new(),
        }
    }

    pub fn compile<'a, T>(mut self, statements: T) -> CompileResult<Vec<u8>>
    where
        T: Iterator<Item = &'a Statement>,
    {
        let mut state = CompilerState::new(None);

        for statement in statements {
            self.compile_statement(&mut state, statement)?;
        }

        Ok(self.bytecode.into())
    }

    fn compile_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        match statement.value {
            StatementKind::Let { .. } => self.compile_let_statement(state, statement)?,
            StatementKind::Function { .. } => self.compile_function_statement(state, statement)?,
            StatementKind::If { .. } => self.compile_if_statement(state, statement)?,
            StatementKind::For { .. } => self.compile_for_statement(state, statement)?,
            // StatementKind::While { .. } => self.compile_while_statement(state, statement),
            // StatementKind::Break => self.compile_break_statement(state, statement),
            // StatementKind::Continue => self.compile_continue_statement(state, statement),
            // StatementKind::Expression(_) => self.compile_expression_statement(state, statement),
            _ => unimplemented!(),
        };

        Ok(())
    }

    fn compile_let_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::Let {
            name:
                Expression {
                    value: ExpressionKind::Identifier(name),
                    ..
                },
            value,
        } = &statement.value
        {
            if let Some(expression) = value {
                self.compile_expression(state, &expression)?;
            } else {
                self.bytecode.const_null();
            }
            if state.is_global {
                self.bytecode.declare_global(*name);
                self.bytecode.store_global(*name);
            } else if let Some(scope) = &mut state.scope {
                scope.push_binding(BindingType::Local, *name);
            } else {
                return Err("Binding let value outside global scope with no scope".to_string());
            }
            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_function_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::Function {
            name:
                Expression {
                    value: ExpressionKind::Identifier(name),
                    ..
                },
            parameters,
            body,
        } = &statement.value
        {
            let start_label = self.bytecode.new_label();
            let end_label = self.bytecode.new_label();

            self.bytecode
                .op(OpCode::NewFunction)
                .usize(parameters.len()) // FIXME: This probably won't work with varargs
                .address_of_auto(start_label);

            if state.is_global {
                self.bytecode.declare_global(*name).store_global(*name);
            } else {
                state
                    .scope
                    .as_mut()
                    .ok_or_else(|| "Missing scope in local scope".to_string())?
                    .push_binding(BindingType::Local, *name);
            }

            let mut inner_scope = Scope::new(state.scope.as_ref());

            for (i, parameter) in parameters.iter().enumerate() {
                if let ExpressionKind::Identifier(id) = parameter.value {
                    inner_scope.bindings.push(Binding {
                        name: id,
                        index: i,
                        typ: BindingType::Argument,
                    });
                } else {
                    return Err("Invalid parameter".to_string());
                }
            }

            let mut inner_state = CompilerState::new(Some(inner_scope));
            inner_state.function_state = Some(FunctionState::new(start_label, end_label));

            self.bytecode.op(OpCode::Jump).address_of_auto(end_label);
            self.bytecode.mark_label(start_label);

            for statement in body {
                self.compile_statement(&mut inner_state, &statement)?;
            }

            let ret_code: u8 = OpCode::Return.into();
            if *self.bytecode.instructions.last().unwrap() != ret_code {
                self.bytecode.const_null().ret();
            }

            self.bytecode.mark_label(end_label);

            for free_variable in inner_state.function_state.unwrap().free_variables {
                if let Some(binding) = inner_state
                    .scope
                    .as_ref()
                    .unwrap()
                    .get_binding(free_variable)
                {
                    match binding.typ {
                        BindingType::Local => self.bytecode.bind_local(binding.index),
                        BindingType::Argument => self.bytecode.bind_argument(binding.index),
                        BindingType::Upvalue => self.bytecode.bind_upvalue(binding.index),
                    };
                } else {
                    unreachable!();
                }
            }

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_if_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::If {
            predicate,
            then_body,
            else_body,
        } = &statement.value
        {
            let else_label = self.bytecode.new_label();
            let end_label = self.bytecode.new_label();

            self.compile_expression(state, &predicate)?;

            self.bytecode.op(OpCode::JumpIfFalse);

            if else_body.is_some() {
                self.bytecode.address_of_auto(else_label);
            } else {
                self.bytecode.address_of_auto(end_label);
            }

            for statement in then_body {
                self.compile_statement(state, &statement)?;
            }

            if let Some(else_body) = else_body {
                self.bytecode.op(OpCode::Jump).address_of_auto(end_label);
                self.bytecode.mark_label(else_label);

                for statement in else_body {
                    self.compile_statement(state, &statement)?;
                }
            }

            self.bytecode.mark_label(end_label);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_for_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::For {
            initializer,
            predicate,
            increment,
            body,
        } = &statement.value
        {
            let start_label = self.bytecode.new_label();
            let end_label = self.bytecode.new_label();
            let increment_label = self.bytecode.new_label();

            let loop_state = LoopState::For {
                start_label,
                end_label,
                increment_label,
            };

            if let Some(initializer) = initializer {
                self.compile_statement(state, &initializer)?;
            }

            // Only enter the loop state after compiling the initializer to
            // prevent such monstrocities as:
            // for break;; {}
            let old_loop_state = state.loop_state.take();
            state.loop_state = Some(loop_state);

            self.bytecode.mark_label(start_label);

            if let Some(predicate) = predicate {
                self.compile_expression(state, &predicate)?;
                self.bytecode
                    .op(OpCode::JumpIfFalse)
                    .address_of_auto(end_label);
            }

            for statement in body {
                self.compile_statement(state, &statement)?;
            }

            self.bytecode.mark_label(increment_label);

            if let Some(increment) = increment {
                self.compile_expression(state, increment)?;
            }

            state.loop_state = old_loop_state;

            self.bytecode.op(OpCode::Jump).address_of_auto(start_label);
            self.bytecode.mark_label(end_label);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_expression(
        &mut self,
        _state: &mut CompilerState,
        _expression: &Expression,
    ) -> CompileResult<()> {
        self.bytecode.const_null();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::agent::Agent;
    use crate::code_object::CodeObject;
    use crate::disassemble::disassemble;
    use crate::parser::{Lexer, Parser};

    macro_rules! test_statement {
        ($input:expr, $expected:expr $(,)?) => {{
            let agent = Agent::new();
            test_statement!($input, $expected, agent)
        }};
        ($input:expr, $expected:expr, $agent:expr $(,)?) => {{
            let mut agent = $agent;
            let ast = {
                let input = $input;
                let lexer = Lexer::new(input);
                let parser = Parser::new(&mut agent, lexer);
                parser.collect::<Result<Vec<Statement>, String>>()?
            };

            let compiler = Compiler::new();
            let bytecode = CodeObject::new(compiler.compile(ast.iter())?);

            let expected = CodeObject::new($expected.into::<Vec<_>>());

            println!("Expected:");
            disassemble(&agent, &expected)?;
            println!("Actual:");
            disassemble(&agent, &bytecode)?;

            assert_eq!(bytecode, expected);

            Ok(())
        }};
    }

    macro_rules! bc {
        ($($tt:tt)*) => {{
            let mut bc = Bytecode::new();
            bytecode! { (&mut bc)
                $($tt)*
            };
            bc
        }};
    }

    #[test]
    fn test_let_declaration_without_value() -> Result<(), String> {
        let mut agent = Agent::new();
        let ident_test = agent.intern_string("test");

        test_statement!(
            "let test;",
            bc! {
                const_null
                declare_global (ident_test)
                store_global (ident_test)
            },
            agent,
        )
    }

    #[test]
    fn test_let_declaration_with_value() -> Result<(), String> {
        let mut agent = Agent::new();
        let ident_test = agent.intern_string("test");

        test_statement!(
            "let test = null;",
            bc! {
                const_null
                declare_global (ident_test)
                store_global (ident_test)
            },
            agent,
        )
    }

    #[test]
    fn test_function_declaration() -> Result<(), String> {
        let mut agent = Agent::new();
        let ident_test = agent.intern_string("test");

        test_statement!(
            "function test() {}",
            bc! {
                new_function 0 start
                declare_global (ident_test)
                store_global (ident_test)
                jump end
            start:
                const_null
                return
            end:
            },
            agent,
        )
    }

    #[test]
    fn test_if_statement_no_else() -> Result<(), String> {
        test_statement!(
            "if null {}",
            bc! {
                const_null
                jump_if_false end
            end:
            },
        )
    }

    #[test]
    fn test_if_statement() -> Result<(), String> {
        test_statement!(
            "if null {} else {}",
            bc! {
                const_null
                jump_if_false else_body
                jump end
            else_body:
            end:
            },
        )
    }

    #[test]
    fn test_if_statement_else_if() -> Result<(), String> {
        test_statement!(
            "if null {} else if null {}",
            bc! {
                const_null
                jump_if_false else_body
                jump end
            else_body:
                const_null
                jump_if_false end
            end:
            },
        )
    }

    #[test]
    fn test_if_statement_else_if_else() -> Result<(), String> {
        test_statement!(
            "if null {} else if null {} else {}",
            bc! {
                const_null
                jump_if_false else_body
                jump end
            else_body:
                const_null
                jump_if_false else_body2
                jump end
            else_body2:
            end:
            },
        )
    }

    #[test]
    fn test_for_statement_no_stuff() -> Result<(), String> {
        let mut bytecode = Bytecode::new();
        bytecode! { (&mut bytecode)
        start:
            jump start
        end:
        };

        test_statement!("for ;; {}", bytecode,)
    }
}
