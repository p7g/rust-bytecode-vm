use crate::bytecode::Bytecode;
use crate::opcode::OpCode;
use crate::parser::{Expression, ExpressionKind, Statement, StatementKind, TokenType};

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

#[derive(Clone)]
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

    pub fn parent_has_binding(&self, name: usize) -> bool {
        if let Some(parent) = self.parent {
            parent.has_binding(name) || parent.parent_has_binding(name)
        } else {
            false
        }
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
    pub fn new(is_global: bool, scope: Option<Scope<'a>>) -> CompilerState<'a> {
        CompilerState {
            is_global,
            loop_state: None,
            function_state: None,
            scope,
        }
    }

    pub fn resolve_binding(&mut self, id: usize) -> Option<Binding> {
        if let Some(scope) = &mut self.scope {
            if let Some(binding) = scope.get_binding(id) {
                Some((*binding).clone())
            } else if scope.parent_has_binding(id) {
                if let Some(function_state) = &mut self.function_state {
                    if let Some(idx) = function_state.free_variables.iter().position(|v| *v == id) {
                        Some(Binding {
                            typ: BindingType::Upvalue,
                            index: scope.binding_count[BindingType::Upvalue as usize] + idx,
                            name: id,
                        })
                    } else {
                        let idx = scope.binding_count[BindingType::Upvalue as usize]
                            + function_state.free_variables.len();
                        function_state.free_variables.push(id);
                        Some(Binding {
                            typ: BindingType::Upvalue,
                            index: idx,
                            name: id,
                        })
                    }
                } else {
                    unreachable!();
                }
            } else {
                None
            }
        } else {
            None
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
        let mut state = CompilerState::new(true, None);

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
            StatementKind::Let { .. } => self.compile_let_statement(state, statement),
            StatementKind::Function { .. } => self.compile_function_statement(state, statement),
            StatementKind::If { .. } => self.compile_if_statement(state, statement),
            StatementKind::For { .. } => self.compile_for_statement(state, statement),
            StatementKind::While { .. } => self.compile_while_statement(state, statement),
            StatementKind::Break => self.compile_break_statement(state, statement),
            StatementKind::Continue => self.compile_continue_statement(state, statement),
            StatementKind::Expression(_) => self.compile_expression_statement(state, statement),
            StatementKind::Return(_) => self.compile_return_statement(state, statement),
        }
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
                self.bytecode
                    .declare_global(*name)
                    .store_global(*name)
                    .pop();
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

    fn compile_function(
        &mut self,
        state: &mut CompilerState,
        name: Option<usize>,
        parameters: &[Expression],
        body: &[Statement],
    ) -> CompileResult<()> {
        let start_label = self.bytecode.new_label();
        let end_label = self.bytecode.new_label();

        self.bytecode
            .op(OpCode::NewFunction)
            .usize(parameters.len()) // FIXME: This probably won't work with varargs
            .address_of_auto(start_label);

        if let Some(name) = name {
            if state.is_global {
                self.bytecode.declare_global(name).store_global(name).pop();
            } else {
                state
                    .scope
                    .as_mut()
                    .ok_or_else(|| "Missing scope in local scope".to_string())?
                    .push_binding(BindingType::Local, name);
            }
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

        let mut inner_state = CompilerState::new(false, Some(inner_scope));
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

        for (i, free_variable) in inner_state
            .function_state
            .unwrap()
            .free_variables
            .iter()
            .enumerate()
        {
            if let Some(binding) = state.scope.as_ref().unwrap().get_binding(*free_variable) {
                match binding.typ {
                    BindingType::Local => self.bytecode.bind_local(binding.index),
                    BindingType::Argument => self.bytecode.bind_argument(binding.index),
                    BindingType::Upvalue => self.bytecode.bind_upvalue(binding.index),
                };
            //} else if state.scope.is_some() && state.scope.as_ref().unwrap().get_binding(*free_variable).is_some() {
            //    let binding = state.scope.as_ref().unwrap().get_binding(*free_variable);
            //    match binding.typ {
            //        BindingType::Argument =>
            //    }
            } else if let Some(scope) = &inner_state.scope {
                if scope.parent_has_binding(*free_variable) {
                    if let Some(function_state) = &mut state.function_state {
                        function_state.free_variables.push(*free_variable);
                        self.bytecode
                            .bind_upvalue(scope.binding_count[BindingType::Upvalue as usize] + i);
                    } else {
                        unreachable!();
                    }
                } else {
                    unreachable!();
                }
            } else {
                unreachable!();
            }
        }

        Ok(())
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
            self.compile_function(state, Some(*name), parameters, body)
        } else {
            unreachable!();
        }
    }

    fn compile_return_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::Return(expr) = &statement.value {
            if let Some(expr) = expr {
                self.compile_expression(state, expr)?;
            } else {
                self.bytecode.const_null();
            }
            self.bytecode.ret();

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
                self.bytecode.pop();
            }

            state.loop_state = old_loop_state;

            self.bytecode.op(OpCode::Jump).address_of_auto(start_label);
            self.bytecode.mark_label(end_label);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_while_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::While { predicate, body } = &statement.value {
            let start_label = self.bytecode.new_label();
            let end_label = self.bytecode.new_label();

            let loop_state = LoopState::While {
                start_label,
                end_label,
            };

            // Only enter the loop state after compiling the initializer to
            // prevent such monstrocities as:
            // for break;; {}
            let old_loop_state = state.loop_state.take();
            state.loop_state = Some(loop_state);

            self.bytecode.mark_label(start_label);

            self.compile_expression(state, &predicate)?;
            self.bytecode
                .op(OpCode::JumpIfFalse)
                .address_of_auto(end_label);

            for statement in body {
                self.compile_statement(state, &statement)?;
            }

            state.loop_state = old_loop_state;

            self.bytecode.op(OpCode::Jump).address_of_auto(start_label);
            self.bytecode.mark_label(end_label);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_break_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if StatementKind::Break == statement.value {
            if let Some(loop_state) = &state.loop_state {
                let end_label = *match loop_state {
                    LoopState::While { end_label, .. } => end_label,
                    LoopState::For { end_label, .. } => end_label,
                };

                self.bytecode.op(OpCode::Jump).address_of_auto(end_label);

                Ok(())
            } else {
                Err("Unexpected break outside of loop context".to_string())
            }
        } else {
            unreachable!();
        }
    }

    fn compile_continue_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if StatementKind::Continue == statement.value {
            if let Some(loop_state) = &state.loop_state {
                let start_label = *match loop_state {
                    LoopState::While { start_label, .. } => start_label,
                    LoopState::For {
                        increment_label, ..
                    } => increment_label,
                };

                self.bytecode.op(OpCode::Jump).address_of_auto(start_label);

                Ok(())
            } else {
                Err("Unexpected break outside of loop context".to_string())
            }
        } else {
            unreachable!();
        }
    }

    fn compile_expression_statement(
        &mut self,
        state: &mut CompilerState,
        statement: &Statement,
    ) -> CompileResult<()> {
        if let StatementKind::Expression(expr) = &statement.value {
            self.compile_expression(state, expr)?;
            self.bytecode.pop();

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        match expression.value {
            ExpressionKind::Identifier(_) => self.compile_identifier_expression(state, expression),
            ExpressionKind::Integer(_) => self.compile_integer_expression(state, expression),
            ExpressionKind::Double(_) => self.compile_double_expression(state, expression),
            ExpressionKind::String(_) => self.compile_string_expression(state, expression),
            ExpressionKind::Null => self.compile_null_expression(state, expression),
            ExpressionKind::Boolean(_) => self.compile_boolean_expression(state, expression),
            ExpressionKind::Array(_) => self.compile_array_expression(state, expression),
            ExpressionKind::Function { .. } => self.compile_function_expression(state, expression),
            // ExpressionKind::UnaryOperation(..) => self.compile_unary_operation_expression(state, expression),
            ExpressionKind::BinaryOperation(..) => {
                self.compile_binary_operation_expression(state, expression)
            }
            ExpressionKind::Call(..) => self.compile_call_expression(state, expression),
            ExpressionKind::Index(..) => self.compile_index_expression(state, expression),
            _ => unimplemented!(),
        }
    }

    fn compile_identifier_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Identifier(id) = expression.value {
            if let Some(binding) = state.resolve_binding(id) {
                match binding.typ {
                    BindingType::Argument => {
                        self.bytecode.load_argument(binding.index);
                    }
                    BindingType::Local => {
                        self.bytecode.load_local(binding.index);
                    }
                    BindingType::Upvalue => {
                        if state.function_state.is_some() {
                            self.bytecode.load_upvalue(binding.index);
                        } else {
                            return Err(
                                "Attempting to load upvalue when not in function".to_string()
                            );
                        }
                    }
                }
            } else {
                self.bytecode.load_global(id);
            }

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_integer_expression(
        &mut self,
        _state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Integer(n) = expression.value {
            self.bytecode.const_int(n);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_double_expression(
        &mut self,
        _state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Double(n) = expression.value {
            self.bytecode.const_double(n);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_string_expression(
        &mut self,
        _state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::String(s) = expression.value {
            self.bytecode.const_string(s);

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_boolean_expression(
        &mut self,
        _state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Boolean(b) = expression.value {
            if b {
                self.bytecode.const_true();
            } else {
                self.bytecode.const_false();
            }

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_null_expression(
        &mut self,
        _state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if ExpressionKind::Null == expression.value {
            self.bytecode.const_null();

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_array_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Array(exprs) = &expression.value {
            for expr in exprs {
                self.compile_expression(state, expr)?;
            }

            self.bytecode.new_array_with_values(exprs.len());

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_function_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Function { parameters, body } = &expression.value {
            self.compile_function(state, None, parameters, body)
        } else {
            unreachable!();
        }
    }

    fn compile_call_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Call(func, args) = &expression.value {
            for arg in args.iter().rev() {
                self.compile_expression(state, arg)?;
            }

            self.compile_expression(state, func)?;
            self.bytecode.call(args.len());

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_binary_operation_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::BinaryOperation(left, op, right) = &expression.value {
            match op {
                TokenType::Equal => {
                    self.compile_expression(state, right)?;
                    match &left.as_ref().value {
                        ExpressionKind::Identifier(id) => {
                            if let Some(binding) = state.resolve_binding(*id) {
                                match binding.typ {
                                    BindingType::Argument => {
                                        self.bytecode.store_argument(binding.index)
                                    }
                                    BindingType::Local => self.bytecode.store_local(binding.index),
                                    BindingType::Upvalue => {
                                        self.bytecode.store_upvalue(binding.index)
                                    }
                                };
                            } else {
                                self.bytecode.store_global(*id);
                            }
                        }

                        ExpressionKind::Index(arr, index) => {
                            self.compile_expression(state, &arr)?;
                            self.compile_expression(state, &index)?;
                            self.bytecode.array_set();
                        }

                        _ => unimplemented!(),
                    }
                }
                TokenType::EqualEqual
                | TokenType::BangEqual
                | TokenType::Star
                | TokenType::Minus
                | TokenType::Plus
                | TokenType::StarStar
                | TokenType::Percent
                | TokenType::Slash
                | TokenType::LessThan
                | TokenType::LessThanEqual
                | TokenType::GreaterThan
                | TokenType::GreaterThanEqual => {
                    self.compile_expression(state, left)?;
                    self.compile_expression(state, right)?;
                    match op {
                        TokenType::EqualEqual => self.bytecode.equal(),
                        TokenType::BangEqual => self.bytecode.not_equal(),
                        TokenType::Star => self.bytecode.mul(),
                        TokenType::Minus => self.bytecode.sub(),
                        TokenType::Plus => self.bytecode.add(),
                        TokenType::StarStar => self.bytecode.exp(),
                        TokenType::Percent => self.bytecode.rem(),
                        TokenType::Slash => self.bytecode.div(),
                        TokenType::LessThan => self.bytecode.less_than(),
                        TokenType::LessThanEqual => self.bytecode.less_than_equal(),
                        TokenType::GreaterThan => self.bytecode.greater_than(),
                        TokenType::GreaterThanEqual => self.bytecode.greater_than_equal(),
                        _ => unreachable!(),
                    };
                }
                _ => unimplemented!(),
            }

            Ok(())
        } else {
            unreachable!();
        }
    }

    fn compile_index_expression(
        &mut self,
        state: &mut CompilerState,
        expression: &Expression,
    ) -> CompileResult<()> {
        if let ExpressionKind::Index(left, right) = &expression.value {
            self.compile_expression(state, left)?;
            self.compile_expression(state, right)?;
            self.bytecode.array_get();

            Ok(())
        } else {
            unreachable!();
        }
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
                pop
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
                pop
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
                pop
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
        test_statement!(
            "for ;; {}",
            bc! {
                start:
                    jump start
            },
        )
    }

    #[test]
    fn test_for_statement() -> Result<(), String> {
        let mut agent = Agent::new();
        let ident_a = agent.intern_string("a");
        test_statement!(
            "for let a; null; null {}",
            bc! {
                const_null
                declare_global (ident_a)
                store_global (ident_a)
                pop
            start:
                const_null
                jump_if_false end
            inc:
                const_null
                pop
                jump start
            end:
            },
            agent,
        )
    }

    #[test]
    fn test_while_statement() -> Result<(), String> {
        test_statement!(
            "while null {}",
            bc! {
            start:
                const_null
                jump_if_false end
                jump start
            end:
            },
        )
    }

    #[test]
    fn test_break_statement_valid() -> Result<(), String> {
        test_statement!(
            "while null { break; }",
            bc! {
            start:
                const_null
                jump_if_false end
                jump end
                jump start
            end:
            },
        )
    }

    fn break_statement_invalid() -> Result<(), String> {
        test_statement!("break;", bc! {})
    }

    #[test]
    fn test_break_statement_invalid() {
        assert!(break_statement_invalid().is_err());
    }

    #[test]
    fn test_continue_statement_while() -> Result<(), String> {
        test_statement!(
            "while null { continue; }",
            bc! {
            start:
                const_null
                jump_if_false end
                jump start
                jump start
            end:
            },
        )
    }

    #[test]
    fn test_continue_statement_for() -> Result<(), String> {
        test_statement!(
            "for ;; null { continue; }",
            bc! {
            start:
                jump inc
            inc:
                const_null
                pop
                jump start
            end:
            },
        )
    }

    #[test]
    fn test_expression_statement() -> Result<(), String> {
        test_statement!(
            "null;",
            bc! {
                const_null
                pop
            },
        )
    }
}
