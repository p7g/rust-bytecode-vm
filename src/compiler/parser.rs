use std::iter::Peekable;
use std::str::Chars;

use crate::agent::Agent;
use crate::module::ModuleSpec;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    Identifier,
    Integer,
    Double,
    String,
    Char,
    Null,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    StarStar,
    And,
    Pipe,
    Caret,
    AndAnd,
    PipePipe,
    Semicolon,
    Equal,
    EqualEqual,
    Tilde,
    Bang,
    BangEqual,
    LessThan,
    LessLess,
    LessThanEqual,
    GreaterThan,
    GreaterGreater,
    GreaterThanEqual,
    Comma,
    Dot,

    Return,
    Function,
    For,
    If,
    Else,
    While,
    Break,
    Continue,
    Let,
    True,
    False,
    Module,
    Export,
    Import,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    position: Position,
    typ: TokenType,
    text: String,
}

impl Token {
    pub fn new<T>(typ: TokenType, line: usize, column: usize, text: T) -> Token
    where
        T: Into<String>,
    {
        Token {
            position: Position { line, column },
            typ,
            text: text.into(),
        }
    }

    pub fn lbp(&self) -> Result<usize, String> {
        Ok(match self.typ {
            TokenType::Identifier => 0,
            TokenType::Integer => 0,
            TokenType::Double => 0,
            TokenType::String => 0,
            TokenType::Char => 0,
            TokenType::Null => 0,

            TokenType::Equal => 1,
            TokenType::PipePipe => 2,
            TokenType::AndAnd => 3,
            TokenType::Pipe => 4,
            TokenType::Caret => 5,
            TokenType::And => 6,
            TokenType::EqualEqual => 7,
            TokenType::BangEqual => 7,
            TokenType::LessThan => 8,
            TokenType::LessThanEqual => 8,
            TokenType::GreaterThan => 8,
            TokenType::GreaterThanEqual => 8,
            TokenType::LessLess => 9,
            TokenType::GreaterGreater => 9,
            TokenType::Plus => 10,
            TokenType::Minus => 10,
            TokenType::Star => 11,
            TokenType::Slash => 12,
            TokenType::Percent => 13,
            TokenType::StarStar => 14,
            TokenType::LeftParen => 16,
            TokenType::LeftBracket => 16,
            TokenType::Dot => 17,

            TokenType::Semicolon => 0,
            TokenType::RightParen => 0,
            TokenType::RightBracket => 0,
            TokenType::LeftBrace => 0,
            TokenType::Comma => 0,

            _ => return Err(format!("Trying to get lbp of {:?}", self)),
        })
    }

    pub fn rbp(&self) -> usize {
        match self.typ {
            TokenType::LeftParen => 0,
            TokenType::LeftBracket => 0,
            TokenType::Bang => 11,
            TokenType::Minus => 11,
            TokenType::Tilde => 11,

            _ => unreachable!(),
        }
    }
}

pub struct Lexer<'a> {
    position: usize,
    line: usize,
    column: usize,
    input: &'a str,
    chars: Peekable<Chars<'a>>,
    pub(crate) filename: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(filename: &'a str, input: &'a str) -> Lexer<'a> {
        Lexer {
            position: 0,
            line: 1,
            column: 1,
            input,
            chars: input.chars().peekable(),
            filename,
        }
    }

    fn error(&self, msg: String) -> String {
        format!("Error in {}: {}", self.filename, msg)
    }

    fn next_char(&mut self) -> Option<char> {
        let next = self.chars.next();
        if next.is_some() {
            self.column += 1;
            self.position += 1;
        }
        next
    }

    fn peek_char(&mut self) -> Option<&char> {
        self.chars.peek()
    }

    pub fn next_token(&mut self) -> Option<Result<Token, String>> {
        let mut start = self.position;
        let mut start_line = self.line;
        let mut start_column = self.column;

        macro_rules! token {
            ($typ:expr) => {{
                return Some(Ok(Token::new(
                    $typ,
                    start_line,
                    start_column,
                    &self.input[start..self.position],
                )));
            }};
        }

        macro_rules! error {
            ($message:expr $(, $stuff:expr)* $(,)?) => {
                Some(Err(self.error(format!($message, $($stuff)*))))
            };
        }

        macro_rules! escape_sequence {
            () => {{
                if let Some(c) = self.next_char() {
                    match c {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '"' => '"',
                        '\'' => '\'',
                        _ => return error!("unrecognized escape sequence"),
                    }
                } else {
                    return error!("unterminated escape sequence");
                }
            }};
        }

        while let Some(c) = self.next_char() {
            macro_rules! or2 {
                ($char:expr, $typ:expr, $typ2:expr) => {{
                    if let Some(c) = self.peek_char() {
                        if *c == $char {
                            self.next_char();
                            token!($typ2);
                        }
                    }
                    token!($typ);
                }};
            }

            match c {
                '\n' => {
                    self.line += 1;
                    self.column = 1;
                    start_line += 1;
                    start_column = 1;
                    start += 1;
                }
                '\r' | ' ' | '\t' => {
                    start += 1;
                    start_column += 1;
                }

                '#' => {
                    let mut len = 1;
                    while let Some(c) = self.next_char() {
                        len += 1;
                        if c == '\n' {
                            break;
                        }
                    }
                    self.line += 1;
                    start += len;
                }

                '+' => token!(TokenType::Plus),
                '-' => token!(TokenType::Minus),
                '/' => token!(TokenType::Slash),
                '^' => token!(TokenType::Caret),
                '[' => token!(TokenType::LeftBracket),
                ']' => token!(TokenType::RightBracket),
                '{' => token!(TokenType::LeftBrace),
                '}' => token!(TokenType::RightBrace),
                '(' => token!(TokenType::LeftParen),
                ')' => token!(TokenType::RightParen),
                '%' => token!(TokenType::Percent),
                ';' => token!(TokenType::Semicolon),
                ',' => token!(TokenType::Comma),
                '~' => token!(TokenType::Tilde),
                '.' => token!(TokenType::Dot),
                '*' => or2!('*', TokenType::Star, TokenType::StarStar),
                '&' => or2!('&', TokenType::And, TokenType::AndAnd),
                '|' => or2!('|', TokenType::Pipe, TokenType::PipePipe),
                '=' => or2!('=', TokenType::Equal, TokenType::EqualEqual),
                '<' => or2!('=', TokenType::LessThan, TokenType::LessThanEqual),
                '>' => or2!('=', TokenType::GreaterThan, TokenType::GreaterThanEqual),
                '!' => or2!('=', TokenType::Bang, TokenType::BangEqual),

                '0'..='9' => {
                    let mut is_double = false;
                    while let Some(c) = self.peek_char() {
                        match c {
                            '0'..='9' => {
                                self.next_char();
                            }
                            '.' => {
                                self.next_char();
                                if is_double {
                                    return error!("Unexpected '.'");
                                } else {
                                    is_double = true;
                                }
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    token!(if is_double {
                        TokenType::Double
                    } else {
                        TokenType::Integer
                    });
                }

                'a'..='z' | 'A'..='Z' | '_' => {
                    while let Some(c) = self.peek_char() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => {
                                self.next_char();
                            }
                            _ => break,
                        }
                    }

                    match &self.input[start..self.position] {
                        "return" => token!(TokenType::Return),
                        "for" => token!(TokenType::For),
                        "while" => token!(TokenType::While),
                        "function" => token!(TokenType::Function),
                        "let" => token!(TokenType::Let),
                        "if" => token!(TokenType::If),
                        "else" => token!(TokenType::Else),
                        "break" => token!(TokenType::Break),
                        "continue" => token!(TokenType::Continue),
                        "null" => token!(TokenType::Null),
                        "true" => token!(TokenType::True),
                        "false" => token!(TokenType::False),
                        "module" => token!(TokenType::Module),
                        "export" => token!(TokenType::Export),
                        "import" => token!(TokenType::Import),
                        _ => token!(TokenType::Identifier),
                    }
                }

                '\'' => {
                    let c = match self.next_char() {
                        Some('\\') => escape_sequence!(),
                        Some(c) => c,
                        None => return error!("Unterminated character literal"),
                    };

                    self.next_char();

                    return Some(Ok(Token::new(
                        TokenType::Char,
                        start_line,
                        start_column,
                        c.to_string(),
                    )));
                }

                '"' => {
                    let mut buf = String::new();
                    while let Some(c) = self.peek_char() {
                        match c {
                            '\\' => {
                                self.next_char();
                                buf.push(escape_sequence!());
                            }
                            '"' => {
                                self.next_char();
                                break;
                            }
                            _ => {
                                buf.push(*c);
                                self.next_char();
                            }
                        }
                    }

                    return Some(Ok(Token::new(
                        TokenType::String,
                        start_line,
                        start_column,
                        buf.as_str(),
                    )));
                }

                _ => return error!("Unexpected character '{}'", c),
            }
        }

        None
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token, String>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub position: Position,
    pub value: StatementKind,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Function {
        name: Expression,
        parameters: Vec<Expression>,
        body: Vec<Statement>,
    },
    Let {
        name: Expression,
        value: Option<Expression>,
    },
    If {
        predicate: Expression,
        then_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    While {
        predicate: Expression,
        body: Vec<Statement>,
    },
    For {
        initializer: Option<Box<Statement>>,
        predicate: Option<Expression>,
        increment: Option<Expression>,
        body: Vec<Statement>,
    },
    Break,
    Continue,
    Return(Option<Expression>),
    Expression(Expression),
    Export(Box<Statement>),
    Import(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub position: Position,
    pub value: ExpressionKind,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    Identifier(usize),
    Integer(i64),
    Double(f64),
    String(usize),
    Char(char),
    Boolean(bool),
    Null,
    Array(Vec<Expression>),
    Function {
        parameters: Vec<Expression>,
        body: Vec<Statement>,
    },
    UnaryOperation(TokenType, Box<Expression>),
    BinaryOperation(Box<Expression>, TokenType, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
}

pub type ParseResult<T> = Result<T, String>;

#[inline]
fn assert_ident(ident: &Expression) -> ParseResult<()> {
    if let ExpressionKind::Identifier(_) = ident.value {
        Ok(())
    } else {
        Err(format!("Expected identifier at {}", ident.position))
    }
}

pub(crate) struct ParsedModule {
    pub(crate) spec: ModuleSpec,
    pub(crate) imports: Vec<String>,
    pub(crate) statements: Vec<Statement>,
}

pub struct Parser<'a> {
    agent: &'a mut Agent,
    lexer: Peekable<Lexer<'a>>,
    current_token: Option<Token>,
    module: Option<ModuleSpec>,
    filename: &'a str,
}

impl<'a> Parser<'a> {
    pub fn new(filename: &'a str, agent: &'a mut Agent, lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            agent,
            lexer: lexer.peekable(),
            current_token: None,
            module: None,
            filename,
        }
    }

    pub(crate) fn parse(&mut self) -> ParseResult<ParsedModule> {
        let (imports, statements): (Vec<ParseResult<Statement>>, Vec<ParseResult<Statement>>) =
            self.partition(|s| {
                if let Ok(Statement {
                    value: StatementKind::Import(_),
                    ..
                }) = s
                {
                    true
                } else {
                    false
                }
            });

        let imports = imports
            .into_iter()
            .map(|i| {
                if let Ok(i) = i {
                    if let StatementKind::Import(path) = i.value {
                        Ok(path)
                    } else {
                        unreachable!();
                    }
                } else if let Err(e) = i {
                    Err(e)
                } else {
                    unreachable!();
                }
            })
            .collect::<ParseResult<Vec<_>>>()?;

        Ok(ParsedModule {
            spec: self.module.take().unwrap(),
            imports,
            statements: statements.into_iter().collect::<ParseResult<Vec<_>>>()?,
        })
    }

    fn error<T>(&self, msg: String) -> ParseResult<T> {
        Err(format!("Error in {}: {}", self.filename, msg))
    }

    pub fn next_statement(&mut self) -> Option<ParseResult<Statement>> {
        match self.peek() {
            Ok(Some(_)) => Some(self.parse_statement()),
            Err(msg) => Some(Err(msg)),
            Ok(None) => None,
        }
    }

    fn next_token(&mut self) -> ParseResult<Option<Token>> {
        let result = self.lexer.next().transpose();
        match &result {
            Ok(Some(token)) => {
                self.current_token = Some(token.clone());
                result
            }
            _ => {
                self.current_token = None;
                result
            }
        }
    }

    fn expect(&mut self, expected: TokenType) -> ParseResult<Token> {
        match &self.next_token()? {
            Some(tok) if tok.typ == expected => Ok(tok.clone()),
            Some(Token { typ, position, .. }) => Err(format!(
                "Expected {:?}, got {:?} at {}",
                expected, typ, position
            )),
            None => Err(format!("Expected {:?}, found end of input", expected)),
        }
    }

    fn peek(&mut self) -> ParseResult<Option<&Token>> {
        match self.lexer.peek() {
            Some(&Ok(ref tok)) => Ok(Some(tok)),
            Some(Err(msg)) => Err(format!("Error in {}: {}", self.filename, msg.clone())),
            None => Ok(None),
        }
    }

    fn matches(&mut self, expected: TokenType) -> ParseResult<bool> {
        match self.peek()? {
            Some(&Token { typ, .. }) if typ == expected => {
                self.expect(expected)?;
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        loop {
            return match if let Some(token) = self.peek()? {
                match token.typ {
                    TokenType::Let => self.parse_let_declaration(),
                    TokenType::Function => self.parse_function_declaration(),
                    TokenType::If => self.parse_if_statement(),
                    TokenType::While => self.parse_while_statement(),
                    TokenType::For => self.parse_for_statement(),
                    TokenType::Continue => self.parse_continue_statement(),
                    TokenType::Break => self.parse_break_statement(),
                    TokenType::Return => self.parse_return_statement(),
                    TokenType::Export => self.parse_export_statement(),
                    TokenType::Import => self.parse_import_statement(),
                    TokenType::Module => {
                        self.parse_module_statement()?;
                        continue;
                    }
                    _ => self.parse_expression_statement(),
                }
            } else {
                unreachable!();
            } {
                Ok(s) => Ok(s),
                Err(msg) => self.error(msg),
            };
        }
    }

    fn parse_let_declaration(&mut self) -> ParseResult<Statement> {
        let let_ = self.expect(TokenType::Let)?;
        let ident = self.expect(TokenType::Identifier)?;
        let ident = self.parse_identifier_expression(ident)?;

        let value = if self.matches(TokenType::Equal)? {
            Some(self.parse_expression()?)
        } else {
            None
        };

        self.expect(TokenType::Semicolon)?;

        Ok(Statement {
            position: let_.position,
            value: StatementKind::Let { name: ident, value },
        })
    }

    fn parse_list<T>(
        &mut self,
        terminator: TokenType,
        separator: TokenType,
        parse: fn(&mut Self) -> ParseResult<T>,
        validate: fn(&T) -> ParseResult<()>,
    ) -> ParseResult<Vec<T>> {
        let mut items = Vec::new();
        while !self.matches(terminator)? {
            let item = parse(self)?;
            validate(&item)?;
            items.push(item);
            if !self.matches(separator)? {
                self.expect(terminator)?;
                break;
            }
        }
        Ok(items)
    }

    fn parse_function_declaration(&mut self) -> ParseResult<Statement> {
        let function = self.expect(TokenType::Function)?;
        let ident = self.expect(TokenType::Identifier)?;
        let ident = self.parse_identifier_expression(ident)?;

        self.expect(TokenType::LeftParen)?;

        let params = self.parse_list(
            TokenType::RightParen,
            TokenType::Comma,
            Self::parse_expression,
            assert_ident,
        )?;

        let mut body = Vec::new();

        self.expect(TokenType::LeftBrace)?;
        while !self.matches(TokenType::RightBrace)? {
            body.push(self.parse_statement()?);
        }

        Ok(Statement {
            position: function.position,
            value: StatementKind::Function {
                name: ident,
                parameters: params,
                body,
            },
        })
    }

    fn parse_if_statement(&mut self) -> ParseResult<Statement> {
        let if_ = self.expect(TokenType::If)?;
        let predicate = self.parse_expression()?;
        self.expect(TokenType::LeftBrace)?;

        let mut then_body = Vec::new();
        while !self.matches(TokenType::RightBrace)? {
            then_body.push(self.parse_statement()?);
        }

        let else_body = if self.matches(TokenType::Else)? {
            let peeked = self.peek()?;
            if peeked.is_some() && peeked.unwrap().typ == TokenType::If {
                Some(vec![self.parse_if_statement()?])
            } else {
                self.expect(TokenType::LeftBrace)?;
                let mut stmts = Vec::new();
                while !self.matches(TokenType::RightBrace)? {
                    stmts.push(self.parse_statement()?);
                }
                Some(stmts)
            }
        } else {
            None
        };

        Ok(Statement {
            position: if_.position,
            value: StatementKind::If {
                predicate,
                then_body,
                else_body,
            },
        })
    }

    fn parse_while_statement(&mut self) -> ParseResult<Statement> {
        let while_ = self.expect(TokenType::While)?;
        let predicate = self.parse_expression()?;

        self.expect(TokenType::LeftBrace)?;

        let mut body = Vec::new();
        while !self.matches(TokenType::RightBrace)? {
            body.push(self.parse_statement()?);
        }

        Ok(Statement {
            position: while_.position,
            value: StatementKind::While { predicate, body },
        })
    }

    fn parse_for_statement(&mut self) -> ParseResult<Statement> {
        let for_ = self.expect(TokenType::For)?;

        let initializer = if self.matches(TokenType::Semicolon)? {
            None
        } else {
            Some(Box::new(self.parse_statement()?))
        };

        let predicate = if self.matches(TokenType::Semicolon)? {
            None
        } else {
            let pred = self.parse_expression()?;
            self.expect(TokenType::Semicolon)?;
            Some(pred)
        };

        let increment = if self.matches(TokenType::LeftBrace)? {
            None
        } else {
            let inc = self.parse_expression()?;
            self.expect(TokenType::LeftBrace)?;
            Some(inc)
        };

        let mut body = Vec::new();
        while !self.matches(TokenType::RightBrace)? {
            body.push(self.parse_statement()?);
        }

        Ok(Statement {
            position: for_.position,
            value: StatementKind::For {
                initializer,
                predicate,
                increment,
                body,
            },
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<Statement> {
        let return_ = self.expect(TokenType::Return)?;

        let expression = if self.matches(TokenType::Semicolon)? {
            None
        } else {
            let expr = self.parse_expression()?;
            self.expect(TokenType::Semicolon)?;
            Some(expr)
        };

        Ok(Statement {
            position: return_.position,
            value: StatementKind::Return(expression),
        })
    }

    fn parse_export_statement(&mut self) -> ParseResult<Statement> {
        let export = self.expect(TokenType::Export)?;
        let decl = self.parse_statement()?;

        let name = match &decl.value {
            StatementKind::Function { name, .. } | StatementKind::Let { name, .. } => &name.value,
            _ => return Err("Can only export declarations".to_string()),
        };

        self.module.as_mut().unwrap().add_export(match name {
            ExpressionKind::Identifier(name) => *name,
            _ => unreachable!(),
        });

        Ok(Statement {
            position: export.position,
            value: StatementKind::Export(Box::new(decl)),
        })
    }

    fn parse_import_statement(&mut self) -> ParseResult<Statement> {
        let import = self.expect(TokenType::Import)?;
        let filename = self.parse_expression()?;

        let idx = match filename.value {
            ExpressionKind::String(idx) => idx,
            _ => return Err("Import filename must be a string literal".to_string()),
        };

        self.expect(TokenType::Semicolon)?;

        Ok(Statement {
            position: import.position,
            value: StatementKind::Import(self.agent.string_table[idx].clone()),
        })
    }

    fn parse_module_statement(&mut self) -> ParseResult<()> {
        self.expect(TokenType::Module)?;
        let name = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;

        assert_ident(&name)?;

        let name = if let ExpressionKind::Identifier(name) = name.value {
            name
        } else {
            unreachable!();
        };

        self.module = Some(ModuleSpec::new(name));

        Ok(())
    }

    fn parse_continue_statement(&mut self) -> ParseResult<Statement> {
        let continue_ = self.expect(TokenType::Continue)?;
        self.expect(TokenType::Semicolon)?;
        Ok(Statement {
            position: continue_.position,
            value: StatementKind::Continue,
        })
    }

    fn parse_break_statement(&mut self) -> ParseResult<Statement> {
        let break_ = self.expect(TokenType::Break)?;
        self.expect(TokenType::Semicolon)?;
        Ok(Statement {
            position: break_.position,
            value: StatementKind::Break,
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expression()?;
        self.expect(TokenType::Semicolon)?;

        Ok(Statement {
            position: expression.position,
            value: StatementKind::Expression(expression),
        })
    }

    fn nud(&mut self, token: Token) -> ParseResult<Expression> {
        match token.typ {
            TokenType::Identifier => self.parse_identifier_expression(token),
            TokenType::Integer => self.parse_integer_expression(token),
            TokenType::Double => self.parse_double_expression(token),
            TokenType::String => self.parse_string_expression(token),
            TokenType::Char => self.parse_char_expression(token),
            TokenType::True | TokenType::False => self.parse_boolean_expression(token),
            TokenType::Null => self.parse_null_expression(token),
            TokenType::LeftParen => self.parse_parenthesized_expression(token),
            TokenType::LeftBracket => self.parse_array_expression(token),
            TokenType::Minus | TokenType::Bang | TokenType::Tilde => {
                self.parse_unary_expression(token)
            }
            TokenType::Function => self.parse_function_expression(token),

            _ => Err(format!(
                "Unexpected token {:?} at {}",
                token.typ, token.position
            )),
        }
    }

    fn led(&mut self, token: Token, left: Expression) -> ParseResult<Expression> {
        match token.typ {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Star
            | TokenType::Slash
            | TokenType::Percent
            | TokenType::LessThan
            | TokenType::LessThanEqual
            | TokenType::GreaterThan
            | TokenType::GreaterThanEqual
            | TokenType::EqualEqual
            | TokenType::BangEqual
            | TokenType::Pipe
            | TokenType::Caret
            | TokenType::And
            | TokenType::LessLess
            | TokenType::GreaterGreater
            | TokenType::Dot
            | TokenType::AndAnd
            | TokenType::PipePipe => self.parse_left_assoc_binary(token, left),
            TokenType::Equal | TokenType::StarStar => self.parse_right_assoc_binary(token, left),
            TokenType::LeftParen => self.parse_call_expression(token, left),
            TokenType::LeftBracket => self.parse_index_expression(token, left),

            _ => Err(format!(
                "Unexpected token {:?} at {}",
                token.typ, token.position
            )),
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expression> {
        self.parse_expression_inner(0)
    }

    fn parse_expression_inner(&mut self, rbp: usize) -> ParseResult<Expression> {
        macro_rules! some {
            ($token:expr) => {
                $token.ok_or_else(|| "Unexpected end of input".to_string())?
            };
        }

        let mut t = some!(self.next_token()?);
        let mut left = self.nud(t)?;
        let mut token = self.peek()?.cloned();

        while token.is_some() && rbp < token.unwrap().lbp()? {
            t = some!(self.next_token()?);
            left = self.led(t, left)?;
            token = self.peek()?.cloned();
        }

        Ok(left)
    }

    fn parse_identifier_expression(&mut self, ident: Token) -> ParseResult<Expression> {
        let id = self.agent.intern_string(ident.text.as_ref());
        Ok(Expression {
            position: ident.position,
            value: ExpressionKind::Identifier(id),
        })
    }

    fn parse_integer_expression(&mut self, number: Token) -> ParseResult<Expression> {
        let intval = number
            .text
            .parse()
            .expect("Failed to parse lexed integer literal");

        Ok(Expression {
            position: number.position,
            value: ExpressionKind::Integer(intval),
        })
    }

    fn parse_double_expression(&mut self, number: Token) -> ParseResult<Expression> {
        let floatval: f64 = number
            .text
            .parse()
            .expect("Failed to parse lexed integer literal");

        Ok(Expression {
            position: number.position,
            value: ExpressionKind::Double(floatval),
        })
    }

    fn parse_string_expression(&mut self, string: Token) -> ParseResult<Expression> {
        let id = self.agent.intern_string(&string.text);
        Ok(Expression {
            position: string.position,
            value: ExpressionKind::String(id),
        })
    }

    fn parse_char_expression(&mut self, c: Token) -> ParseResult<Expression> {
        if let Some(charval) = c.text.chars().next() {
            Ok(Expression {
                position: c.position,
                value: ExpressionKind::Char(charval),
            })
        } else {
            unreachable!()
        }
    }

    fn parse_null_expression(&mut self, null: Token) -> ParseResult<Expression> {
        Ok(Expression {
            position: null.position,
            value: ExpressionKind::Null,
        })
    }

    fn parse_boolean_expression(&mut self, b: Token) -> ParseResult<Expression> {
        Ok(Expression {
            position: b.position,
            value: ExpressionKind::Boolean(match b.typ {
                TokenType::True => true,
                TokenType::False => false,
                _ => unreachable!(),
            }),
        })
    }

    fn parse_parenthesized_expression(&mut self, left_paren: Token) -> ParseResult<Expression> {
        let mut expr = self.parse_expression()?;
        self.expect(TokenType::RightParen)?;
        expr.position = left_paren.position;
        Ok(expr)
    }

    fn parse_array_expression(&mut self, left_bracket: Token) -> ParseResult<Expression> {
        Ok(Expression {
            position: left_bracket.position,
            value: ExpressionKind::Array(self.parse_list(
                TokenType::RightBracket,
                TokenType::Comma,
                Self::parse_expression,
                |_| Ok(()),
            )?),
        })
    }

    fn parse_unary_expression(&mut self, op: Token) -> ParseResult<Expression> {
        let right = self.parse_expression_inner(op.rbp())?;

        Ok(Expression {
            position: op.position,
            value: ExpressionKind::UnaryOperation(op.typ, Box::new(right)),
        })
    }

    fn parse_left_assoc_binary(&mut self, op: Token, left: Expression) -> ParseResult<Expression> {
        let right = self.parse_expression_inner(op.lbp()?)?;

        Ok(Expression {
            position: left.position,
            value: ExpressionKind::BinaryOperation(Box::new(left), op.typ, Box::new(right)),
        })
    }

    fn parse_right_assoc_binary(&mut self, op: Token, left: Expression) -> ParseResult<Expression> {
        let right = self.parse_expression_inner(op.lbp()? - 1)?;

        Ok(Expression {
            position: left.position,
            value: ExpressionKind::BinaryOperation(Box::new(left), op.typ, Box::new(right)),
        })
    }

    fn parse_call_expression(
        &mut self,
        _left_paren: Token,
        callee: Expression,
    ) -> ParseResult<Expression> {
        let args = self.parse_list(
            TokenType::RightParen,
            TokenType::Comma,
            Self::parse_expression,
            |_| Ok(()),
        )?;

        Ok(Expression {
            position: callee.position,
            value: ExpressionKind::Call(Box::new(callee), args),
        })
    }

    fn parse_index_expression(
        &mut self,
        _left_bracket: Token,
        arr: Expression,
    ) -> ParseResult<Expression> {
        let index = self.parse_expression()?;
        self.expect(TokenType::RightBracket)?;

        Ok(Expression {
            position: arr.position,
            value: ExpressionKind::Index(Box::new(arr), Box::new(index)),
        })
    }

    fn parse_function_expression(&mut self, functionkw: Token) -> ParseResult<Expression> {
        self.expect(TokenType::LeftParen)?;

        let parameters = self.parse_list(
            TokenType::RightParen,
            TokenType::Comma,
            Self::parse_expression,
            assert_ident,
        )?;

        self.expect(TokenType::LeftBrace)?;

        let mut body = Vec::new();
        while !self.matches(TokenType::RightBrace)? {
            body.push(self.parse_statement()?);
        }

        Ok(Expression {
            position: functionkw.position,
            value: ExpressionKind::Function { parameters, body },
        })
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = ParseResult<Statement>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_statement()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_lexer_symbols() {
        let input = "[ ] { } ( ) + - * ** / & && | || ^ % ; < > <= >= = == ! != ,";
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![
                Token::new(TokenType::LeftBracket, 1, 1, "["),
                Token::new(TokenType::RightBracket, 1, 3, "]"),
                Token::new(TokenType::LeftBrace, 1, 5, "{"),
                Token::new(TokenType::RightBrace, 1, 7, "}"),
                Token::new(TokenType::LeftParen, 1, 9, "("),
                Token::new(TokenType::RightParen, 1, 11, ")"),
                Token::new(TokenType::Plus, 1, 13, "+"),
                Token::new(TokenType::Minus, 1, 15, "-"),
                Token::new(TokenType::Star, 1, 17, "*"),
                Token::new(TokenType::StarStar, 1, 19, "**"),
                Token::new(TokenType::Slash, 1, 22, "/"),
                Token::new(TokenType::And, 1, 24, "&"),
                Token::new(TokenType::AndAnd, 1, 26, "&&"),
                Token::new(TokenType::Pipe, 1, 29, "|"),
                Token::new(TokenType::PipePipe, 1, 31, "||"),
                Token::new(TokenType::Caret, 1, 34, "^"),
                Token::new(TokenType::Percent, 1, 36, "%"),
                Token::new(TokenType::Semicolon, 1, 38, ";"),
                Token::new(TokenType::LessThan, 1, 40, "<"),
                Token::new(TokenType::GreaterThan, 1, 42, ">"),
                Token::new(TokenType::LessThanEqual, 1, 44, "<="),
                Token::new(TokenType::GreaterThanEqual, 1, 47, ">="),
                Token::new(TokenType::Equal, 1, 50, "="),
                Token::new(TokenType::EqualEqual, 1, 52, "=="),
                Token::new(TokenType::Bang, 1, 55, "!"),
                Token::new(TokenType::BangEqual, 1, 57, "!="),
                Token::new(TokenType::Comma, 1, 60, ","),
            ],
        );
    }

    #[test]
    fn test_integer() {
        let input = "123";
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(TokenType::Integer, 1, 1, "123")]
        );
    }

    #[test]
    fn test_double() {
        let input = "1.23";
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(TokenType::Double, 1, 1, "1.23")]
        );
    }

    #[test]
    fn test_identifier() {
        let input = "
abc
_a
_
_1312
return
while
for
let
function
";
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![
                Token::new(TokenType::Identifier, 2, 1, "abc"),
                Token::new(TokenType::Identifier, 3, 1, "_a"),
                Token::new(TokenType::Identifier, 4, 1, "_"),
                Token::new(TokenType::Identifier, 5, 1, "_1312"),
                Token::new(TokenType::Return, 6, 1, "return"),
                Token::new(TokenType::While, 7, 1, "while"),
                Token::new(TokenType::For, 8, 1, "for"),
                Token::new(TokenType::Let, 9, 1, "let"),
                Token::new(TokenType::Function, 10, 1, "function"),
            ],
        );
    }

    #[test]
    fn test_string() {
        let input = r#""this is a string""#;
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(TokenType::String, 1, 1, "this is a string"),]
        );
    }

    #[test]
    fn test_string_escapes() {
        let input = r#""so I says, \"this\nis\tan escaped string\"""#;
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(
                TokenType::String,
                1,
                1,
                "so I says, \"this\nis\tan escaped string\""
            ),]
        );
    }

    #[test]
    fn test_lexing_language_snippet() {
        let input = r#"
            function testing() {
                return "hello world";
            }

            print(testing());
        "#;
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer
                .filter_map(|tok| tok.ok().map(|tok| (tok.typ, tok.text)))
                .collect::<Vec<_>>(),
            vec![
                (TokenType::Function, "function".to_string()),
                (TokenType::Identifier, "testing".to_string()),
                (TokenType::LeftParen, "(".to_string()),
                (TokenType::RightParen, ")".to_string()),
                (TokenType::LeftBrace, "{".to_string()),
                (TokenType::Return, "return".to_string()),
                (TokenType::String, "hello world".to_string()),
                (TokenType::Semicolon, ";".to_string()),
                (TokenType::RightBrace, "}".to_string()),
                (TokenType::Identifier, "print".to_string()),
                (TokenType::LeftParen, "(".to_string()),
                (TokenType::Identifier, "testing".to_string()),
                (TokenType::LeftParen, "(".to_string()),
                (TokenType::RightParen, ")".to_string()),
                (TokenType::RightParen, ")".to_string()),
                (TokenType::Semicolon, ";".to_string()),
            ],
        );
    }

    #[test]
    fn test_lexer_iterator() {
        let input = "test 1 1.2";
        let lexer = Lexer::new("test", input);

        assert_eq!(
            lexer
                .filter_map(|t| t.ok().map(|t| (t.typ, t.text)))
                .collect::<Vec<_>>(),
            vec![
                (TokenType::Identifier, "test".to_string()),
                (TokenType::Integer, "1".to_string()),
                (TokenType::Double, "1.2".to_string()),
            ]
        );
    }

    #[test]
    fn test_let_declaration() {
        let mut agent = Agent::new();
        let name = agent.intern_string("something");
        let someident = agent.intern_string("someident");
        let input = "let something = someident;";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 1, column: 1 },
                value: StatementKind::Let {
                    name: Expression {
                        position: Position { line: 1, column: 5 },
                        value: ExpressionKind::Identifier(name),
                    },
                    value: Some(Expression {
                        position: Position {
                            line: 1,
                            column: 17
                        },
                        value: ExpressionKind::Identifier(someident),
                    }),
                },
            })],
        );
    }

    #[test]
    fn test_function_declaration() {
        let mut agent = Agent::new();
        let name = agent.intern_string("test");
        let ident_a = agent.intern_string("a");
        let ident_b = agent.intern_string("b");
        let ident_c = agent.intern_string("c");
        let ident_hello = agent.intern_string("hello");

        let input = "
function test(a, b, c) {
    let hello = b;
}
";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.filter_map(|s| s.ok()).collect::<Vec<_>>(),
            vec![Statement {
                position: Position { line: 2, column: 1 },
                value: StatementKind::Function {
                    name: Expression {
                        position: Position {
                            line: 2,
                            column: 10
                        },
                        value: ExpressionKind::Identifier(name),
                    },
                    parameters: vec![
                        Expression {
                            position: Position {
                                line: 2,
                                column: 15
                            },
                            value: ExpressionKind::Identifier(ident_a),
                        },
                        Expression {
                            position: Position {
                                line: 2,
                                column: 18
                            },
                            value: ExpressionKind::Identifier(ident_b),
                        },
                        Expression {
                            position: Position {
                                line: 2,
                                column: 21
                            },
                            value: ExpressionKind::Identifier(ident_c),
                        },
                    ],
                    body: vec![Statement {
                        position: Position { line: 3, column: 5 },
                        value: StatementKind::Let {
                            name: Expression {
                                position: Position { line: 3, column: 9 },
                                value: ExpressionKind::Identifier(ident_hello),
                            },
                            value: Some(Expression {
                                position: Position {
                                    line: 3,
                                    column: 17
                                },
                                value: ExpressionKind::Identifier(ident_b),
                            }),
                        },
                    },],
                },
            },],
        );
    }

    #[test]
    fn test_parameter_list_trailing_comma() {
        let mut agent = Agent::new();
        let input = "
function test(
    this,
    func,
    has,
    many,
    parameters,
) {}
";
        let ident_test = agent.intern_string("test");
        let ident_this = agent.intern_string("this");
        let ident_func = agent.intern_string("func");
        let ident_has = agent.intern_string("has");
        let ident_many = agent.intern_string("many");
        let ident_parameters = agent.intern_string("parameters");
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 2, column: 1 },
                value: StatementKind::Function {
                    name: Expression {
                        position: Position {
                            line: 2,
                            column: 10
                        },
                        value: ExpressionKind::Identifier(ident_test),
                    },
                    parameters: vec![
                        Expression {
                            position: Position { line: 3, column: 5 },
                            value: ExpressionKind::Identifier(ident_this),
                        },
                        Expression {
                            position: Position { line: 4, column: 5 },
                            value: ExpressionKind::Identifier(ident_func),
                        },
                        Expression {
                            position: Position { line: 5, column: 5 },
                            value: ExpressionKind::Identifier(ident_has),
                        },
                        Expression {
                            position: Position { line: 6, column: 5 },
                            value: ExpressionKind::Identifier(ident_many),
                        },
                        Expression {
                            position: Position { line: 7, column: 5 },
                            value: ExpressionKind::Identifier(ident_parameters),
                        },
                    ],
                    body: Vec::new(),
                },
            }),],
        );
    }

    #[test]
    fn test_missing_semicolon() {
        let mut agent = Agent::new();
        let input = "let something = someident";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Err(
                "Error in test: Expected Semicolon, found end of input".to_string()
            ),],
        );
    }

    #[test]
    fn test_keyword_as_identifier() {
        let mut agent = Agent::new();
        let input = "let while = ok;";
        let lexer = Lexer::new("test", input);
        let mut parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.next().unwrap(),
            Err("Error in test: Expected Identifier, got While at 1:5".to_string()),
        );
    }

    #[test]
    fn test_if_statement() {
        let mut agent = Agent::new();
        let input = "
if truee {
    let test;
}
";
        let ident_true = agent.intern_string("truee");
        let ident_test = agent.intern_string("test");
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 2, column: 1 },
                value: StatementKind::If {
                    predicate: Expression {
                        position: Position { line: 2, column: 4 },
                        value: ExpressionKind::Identifier(ident_true),
                    },
                    then_body: vec![Statement {
                        position: Position { line: 3, column: 5 },
                        value: StatementKind::Let {
                            name: Expression {
                                position: Position { line: 3, column: 9 },
                                value: ExpressionKind::Identifier(ident_test),
                            },
                            value: None,
                        },
                    },],
                    else_body: None,
                },
            },)],
        );
    }

    #[test]
    fn test_if_else_statement() {
        let mut agent = Agent::new();
        let input = "
if truee {
    let test;
} else {
    function test() {}
}
";
        let ident_true = agent.intern_string("truee");
        let ident_test = agent.intern_string("test");
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 2, column: 1 },
                value: StatementKind::If {
                    predicate: Expression {
                        position: Position { line: 2, column: 4 },
                        value: ExpressionKind::Identifier(ident_true),
                    },
                    then_body: vec![Statement {
                        position: Position { line: 3, column: 5 },
                        value: StatementKind::Let {
                            name: Expression {
                                position: Position { line: 3, column: 9 },
                                value: ExpressionKind::Identifier(ident_test),
                            },
                            value: None,
                        },
                    },],
                    else_body: Some(vec![Statement {
                        position: Position { line: 5, column: 5 },
                        value: StatementKind::Function {
                            name: Expression {
                                position: Position {
                                    line: 5,
                                    column: 14
                                },
                                value: ExpressionKind::Identifier(ident_test),
                            },
                            parameters: Vec::new(),
                            body: Vec::new(),
                        },
                    },]),
                },
            },)],
        );
    }

    #[test]
    fn test_if_else_if_statement() {
        let mut agent = Agent::new();
        let input = "
if truee {
    let test;
} else if test {
    function test() {}
} else {}
";
        let ident_true = agent.intern_string("truee");
        let ident_test = agent.intern_string("test");
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 2, column: 1 },
                value: StatementKind::If {
                    predicate: Expression {
                        position: Position { line: 2, column: 4 },
                        value: ExpressionKind::Identifier(ident_true),
                    },
                    then_body: vec![Statement {
                        position: Position { line: 3, column: 5 },
                        value: StatementKind::Let {
                            name: Expression {
                                position: Position { line: 3, column: 9 },
                                value: ExpressionKind::Identifier(ident_test),
                            },
                            value: None,
                        },
                    }],
                    else_body: Some(vec![Statement {
                        position: Position { line: 4, column: 8 },
                        value: StatementKind::If {
                            predicate: Expression {
                                position: Position {
                                    line: 4,
                                    column: 11
                                },
                                value: ExpressionKind::Identifier(ident_test),
                            },
                            then_body: vec![Statement {
                                position: Position { line: 5, column: 5 },
                                value: StatementKind::Function {
                                    name: Expression {
                                        position: Position {
                                            line: 5,
                                            column: 14
                                        },
                                        value: ExpressionKind::Identifier(ident_test),
                                    },
                                    parameters: Vec::new(),
                                    body: Vec::new(),
                                },
                            }],
                            else_body: Some(Vec::new()),
                        },
                    }]),
                },
            },)],
        );
    }

    #[test]
    fn test_while_statement() {
        let mut agent = Agent::new();
        let input = "while 1 { if 2 {} }";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 1, column: 1 },
                value: StatementKind::While {
                    predicate: Expression {
                        position: Position { line: 1, column: 7 },
                        value: ExpressionKind::Integer(1),
                    },
                    body: vec![Statement {
                        position: Position {
                            line: 1,
                            column: 11
                        },
                        value: StatementKind::If {
                            predicate: Expression {
                                position: Position {
                                    line: 1,
                                    column: 14
                                },
                                value: ExpressionKind::Integer(2),
                            },
                            then_body: Vec::new(),
                            else_body: None,
                        },
                    },],
                },
            }),],
        );
    }

    #[test]
    fn test_for_statement() {
        let mut agent = Agent::new();
        let input = "
for ;; {}
for let a;; {}
for ; a; {}
for let a; a; {}
for ;; a {}
for let a;; a {}
for let a; a; a {}
";
        let ident_a = agent.intern_string("a");
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![
                Ok(Statement {
                    position: Position { line: 2, column: 1 },
                    value: StatementKind::For {
                        initializer: None,
                        predicate: None,
                        increment: None,
                        body: Vec::new(),
                    },
                }),
                Ok(Statement {
                    position: Position { line: 3, column: 1 },
                    value: StatementKind::For {
                        initializer: Some(Box::new(Statement {
                            position: Position { line: 3, column: 5 },
                            value: StatementKind::Let {
                                name: Expression {
                                    position: Position { line: 3, column: 9 },
                                    value: ExpressionKind::Identifier(ident_a),
                                },
                                value: None,
                            },
                        })),
                        predicate: None,
                        increment: None,
                        body: Vec::new(),
                    },
                }),
                Ok(Statement {
                    position: Position { line: 4, column: 1 },
                    value: StatementKind::For {
                        initializer: None,
                        predicate: Some(Expression {
                            position: Position { line: 4, column: 7 },
                            value: ExpressionKind::Identifier(ident_a),
                        }),
                        increment: None,
                        body: Vec::new(),
                    },
                }),
                Ok(Statement {
                    position: Position { line: 5, column: 1 },
                    value: StatementKind::For {
                        initializer: Some(Box::new(Statement {
                            position: Position { line: 5, column: 5 },
                            value: StatementKind::Let {
                                name: Expression {
                                    position: Position { line: 5, column: 9 },
                                    value: ExpressionKind::Identifier(ident_a),
                                },
                                value: None,
                            },
                        })),
                        predicate: Some(Expression {
                            position: Position {
                                line: 5,
                                column: 12
                            },
                            value: ExpressionKind::Identifier(ident_a),
                        }),
                        increment: None,
                        body: Vec::new(),
                    },
                }),
                Ok(Statement {
                    position: Position { line: 6, column: 1 },
                    value: StatementKind::For {
                        initializer: None,
                        predicate: None,
                        increment: Some(Expression {
                            position: Position { line: 6, column: 8 },
                            value: ExpressionKind::Identifier(ident_a),
                        }),
                        body: Vec::new(),
                    },
                }),
                Ok(Statement {
                    position: Position { line: 7, column: 1 },
                    value: StatementKind::For {
                        initializer: Some(Box::new(Statement {
                            position: Position { line: 7, column: 5 },
                            value: StatementKind::Let {
                                name: Expression {
                                    position: Position { line: 7, column: 9 },
                                    value: ExpressionKind::Identifier(ident_a),
                                },
                                value: None,
                            },
                        })),
                        predicate: None,
                        increment: Some(Expression {
                            position: Position {
                                line: 7,
                                column: 13
                            },
                            value: ExpressionKind::Identifier(ident_a),
                        }),
                        body: Vec::new(),
                    },
                }),
                Ok(Statement {
                    position: Position { line: 8, column: 1 },
                    value: StatementKind::For {
                        initializer: Some(Box::new(Statement {
                            position: Position { line: 8, column: 5 },
                            value: StatementKind::Let {
                                name: Expression {
                                    position: Position { line: 8, column: 9 },
                                    value: ExpressionKind::Identifier(ident_a),
                                },
                                value: None,
                            },
                        })),
                        predicate: Some(Expression {
                            position: Position {
                                line: 8,
                                column: 12
                            },
                            value: ExpressionKind::Identifier(ident_a),
                        }),
                        increment: Some(Expression {
                            position: Position {
                                line: 8,
                                column: 15
                            },
                            value: ExpressionKind::Identifier(ident_a),
                        }),
                        body: Vec::new(),
                    },
                }),
            ],
        );
    }

    #[test]
    fn test_continue_statement() {
        let mut agent = Agent::new();
        let input = "continue;";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 1, column: 1 },
                value: StatementKind::Continue,
            }),],
        );
    }

    #[test]
    fn test_break_statement() {
        let mut agent = Agent::new();
        let input = "break;";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 1, column: 1 },
                value: StatementKind::Break,
            }),],
        );
    }

    #[test]
    fn test_return_statement() {
        let mut agent = Agent::new();
        let input = "
return;
return 1;
";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![
                Ok(Statement {
                    position: Position { line: 2, column: 1 },
                    value: StatementKind::Return(None),
                }),
                Ok(Statement {
                    position: Position { line: 3, column: 1 },
                    value: StatementKind::Return(Some(Expression {
                        position: Position { line: 3, column: 8 },
                        value: ExpressionKind::Integer(1),
                    })),
                }),
            ],
        );
    }

    #[test]
    fn test_export_statement() {
        let mut agent = Agent::new();
        let ident_a = agent.intern_string("a");
        let input = "module Test; export let a = 0; export function a() {} export 123;";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![
                Ok(Statement {
                    position: Position {
                        line: 1,
                        column: 14
                    },
                    value: StatementKind::Export(Box::new(Statement {
                        position: Position {
                            line: 1,
                            column: 21
                        },
                        value: StatementKind::Let {
                            name: Expression {
                                position: Position {
                                    line: 1,
                                    column: 25
                                },
                                value: ExpressionKind::Identifier(ident_a),
                            },
                            value: Some(Expression {
                                position: Position {
                                    line: 1,
                                    column: 29,
                                },
                                value: ExpressionKind::Integer(0),
                            }),
                        },
                    })),
                }),
                Ok(Statement {
                    position: Position {
                        line: 1,
                        column: 32,
                    },
                    value: StatementKind::Export(Box::new(Statement {
                        position: Position {
                            line: 1,
                            column: 39,
                        },
                        value: StatementKind::Function {
                            name: Expression {
                                position: Position {
                                    line: 1,
                                    column: 48,
                                },
                                value: ExpressionKind::Identifier(ident_a),
                            },
                            parameters: Vec::new(),
                            body: Vec::new(),
                        },
                    })),
                }),
                Err("Error in test: Can only export declarations".to_string()),
            ]
        );
    }

    #[test]
    fn test_import_statement() {
        let mut agent = Agent::new();
        let input = r#"import "test.rbcvm";"#;
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 1, column: 1 },
                value: StatementKind::Import("test.rbcvm".to_string())
            })]
        );
    }

    #[test]
    fn test_import_statement_non_string() {
        let mut agent = Agent::new();
        let input = r#"import abc"#;
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Err(
                "Error in test: Import filename must be a string literal".to_string()
            )]
        );
    }

    #[test]
    fn test_expression_statement() {
        let mut agent = Agent::new();
        let input = "123;";
        let lexer = Lexer::new("test", input);
        let parser = Parser::new("test", &mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Ok(Statement {
                position: Position { line: 1, column: 1 },
                value: StatementKind::Expression(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Integer(123),
                }),
            }),],
        );
    }

    macro_rules! test_expression {
        ($input:expr, $result:expr $(,)?) => {{
            let agent = Agent::new();
            test_expression!($input, $result, agent);
        }};
        ($input:expr, $result:expr, $agent:expr $(,)?) => {{
            let mut agent = $agent;
            let input = $input;
            let lexer = Lexer::new("test", input);
            let parser = Parser::new("test", &mut agent, lexer);

            assert_eq!(
                parser.collect::<Vec<_>>(),
                vec![Ok(Statement {
                    position: Position { line: 1, column: 1 },
                    value: StatementKind::Expression(Expression {
                        position: Position { line: 1, column: 1 },
                        value: $result,
                    }),
                }),],
            );
        }};
    }

    #[test]
    fn test_identifier_expression() {
        let mut agent = Agent::new();
        let ident_a = agent.intern_string("a");
        test_expression!("a;", ExpressionKind::Identifier(ident_a), agent);
    }

    #[test]
    fn test_integer_expression() {
        test_expression!("123;", ExpressionKind::Integer(123));
    }

    #[test]
    fn test_double_expression() {
        test_expression!("1.23;", ExpressionKind::Double(1.23));
    }

    #[test]
    fn test_string_expression() {
        let mut agent = Agent::new();
        let s = agent.intern_string("hello world");
        test_expression!("\"hello world\";", ExpressionKind::String(s), agent);
    }

    #[test]
    fn test_null_expression() {
        test_expression!("null;", ExpressionKind::Null);
    }

    #[test]
    fn test_parenthesized_expression() {
        test_expression!("(123);", ExpressionKind::Integer(123));
    }

    #[test]
    fn test_array_expression() {
        test_expression!(
            "[1, 2, 3];",
            ExpressionKind::Array(vec![
                Expression {
                    position: Position { line: 1, column: 2 },
                    value: ExpressionKind::Integer(1),
                },
                Expression {
                    position: Position { line: 1, column: 5 },
                    value: ExpressionKind::Integer(2),
                },
                Expression {
                    position: Position { line: 1, column: 8 },
                    value: ExpressionKind::Integer(3),
                },
            ])
        );
    }

    #[test]
    fn test_unary_minus() {
        test_expression!(
            "-1;",
            ExpressionKind::UnaryOperation(
                TokenType::Minus,
                Box::new(Expression {
                    position: Position { line: 1, column: 2 },
                    value: ExpressionKind::Integer(1),
                })
            )
        );
        test_expression!(
            "---1;",
            ExpressionKind::UnaryOperation(
                TokenType::Minus,
                Box::new(Expression {
                    position: Position { line: 1, column: 2 },
                    value: ExpressionKind::UnaryOperation(
                        TokenType::Minus,
                        Box::new(Expression {
                            position: Position { line: 1, column: 3 },
                            value: ExpressionKind::UnaryOperation(
                                TokenType::Minus,
                                Box::new(Expression {
                                    position: Position { line: 1, column: 4 },
                                    value: ExpressionKind::Integer(1),
                                }),
                            )
                        }),
                    )
                })
            )
        );
    }

    #[test]
    fn test_binary_addition() {
        test_expression!(
            "1 + 1;",
            ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Integer(1),
                }),
                TokenType::Plus,
                Box::new(Expression {
                    position: Position { line: 1, column: 5 },
                    value: ExpressionKind::Integer(1),
                }),
            ),
        );
    }

    #[test]
    fn test_binary_precedence() {
        test_expression!(
            "1 + 2 * 3;",
            ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Integer(1),
                }),
                TokenType::Plus,
                Box::new(Expression {
                    position: Position { line: 1, column: 5 },
                    value: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            position: Position { line: 1, column: 5 },
                            value: ExpressionKind::Integer(2),
                        }),
                        TokenType::Star,
                        Box::new(Expression {
                            position: Position { line: 1, column: 9 },
                            value: ExpressionKind::Integer(3),
                        }),
                    ),
                }),
            ),
        );
    }

    #[test]
    fn test_binary_left_assoc() {
        let mut agent = Agent::new();
        let a = agent.intern_string("a");
        let b = agent.intern_string("b");
        let c = agent.intern_string("c");
        test_expression!(
            "a + b + c;",
            ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            position: Position { line: 1, column: 1 },
                            value: ExpressionKind::Identifier(a),
                        }),
                        TokenType::Plus,
                        Box::new(Expression {
                            position: Position { line: 1, column: 5 },
                            value: ExpressionKind::Identifier(b),
                        }),
                    ),
                }),
                TokenType::Plus,
                Box::new(Expression {
                    position: Position { line: 1, column: 9 },
                    value: ExpressionKind::Identifier(c),
                }),
            ),
        );
    }

    #[test]
    fn test_binary_right_assoc() {
        let mut agent = Agent::new();
        let a = agent.intern_string("a");
        let b = agent.intern_string("b");
        let c = agent.intern_string("c");
        test_expression!(
            "a = b = c;",
            ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Identifier(a),
                }),
                TokenType::Equal,
                Box::new(Expression {
                    position: Position { line: 1, column: 5 },
                    value: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            position: Position { line: 1, column: 5 },
                            value: ExpressionKind::Identifier(b),
                        }),
                        TokenType::Equal,
                        Box::new(Expression {
                            position: Position { line: 1, column: 9 },
                            value: ExpressionKind::Identifier(c),
                        }),
                    ),
                }),
            ),
        );
    }

    #[test]
    fn test_call_expression() {
        let mut agent = Agent::new();
        let ident_print = agent.intern_string("print");
        test_expression!(
            "print(1 + 1, 2, 3);",
            ExpressionKind::Call(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Identifier(ident_print),
                }),
                vec![
                    Expression {
                        position: Position { line: 1, column: 7 },
                        value: ExpressionKind::BinaryOperation(
                            Box::new(Expression {
                                position: Position { line: 1, column: 7 },
                                value: ExpressionKind::Integer(1),
                            }),
                            TokenType::Plus,
                            Box::new(Expression {
                                position: Position {
                                    line: 1,
                                    column: 11
                                },
                                value: ExpressionKind::Integer(1),
                            }),
                        ),
                    },
                    Expression {
                        position: Position {
                            line: 1,
                            column: 14
                        },
                        value: ExpressionKind::Integer(2),
                    },
                    Expression {
                        position: Position {
                            line: 1,
                            column: 17
                        },
                        value: ExpressionKind::Integer(3),
                    },
                ],
            ),
        );
    }

    #[test]
    fn test_index_expression() {
        let mut agent = Agent::new();
        let ident_array = agent.intern_string("array");
        test_expression!(
            "array[1];",
            ExpressionKind::Index(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Identifier(ident_array),
                }),
                Box::new(Expression {
                    position: Position { line: 1, column: 7 },
                    value: ExpressionKind::Integer(1),
                }),
            ),
            agent,
        );
    }

    #[test]
    fn test_nontrivial_index_expression() {
        test_expression!(
            "(1 = 2)[1];",
            ExpressionKind::Index(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            position: Position { line: 1, column: 2 },
                            value: ExpressionKind::Integer(1),
                        }),
                        TokenType::Equal,
                        Box::new(Expression {
                            position: Position { line: 1, column: 6 },
                            value: ExpressionKind::Integer(2),
                        }),
                    ),
                }),
                Box::new(Expression {
                    position: Position { line: 1, column: 9 },
                    value: ExpressionKind::Integer(1),
                }),
            ),
        );
    }

    #[test]
    fn test_binary_expression_with_index_expression() {
        test_expression!(
            "1 + (3 / 4)[0] * 3;",
            ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Integer(1),
                }),
                TokenType::Plus,
                Box::new(Expression {
                    position: Position { line: 1, column: 5 },
                    value: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            position: Position { line: 1, column: 5 },
                            value: ExpressionKind::Index(
                                Box::new(Expression {
                                    position: Position { line: 1, column: 5 },
                                    value: ExpressionKind::BinaryOperation(
                                        Box::new(Expression {
                                            position: Position { line: 1, column: 6 },
                                            value: ExpressionKind::Integer(3),
                                        }),
                                        TokenType::Slash,
                                        Box::new(Expression {
                                            position: Position {
                                                line: 1,
                                                column: 10
                                            },
                                            value: ExpressionKind::Integer(4),
                                        }),
                                    ),
                                }),
                                Box::new(Expression {
                                    position: Position {
                                        line: 1,
                                        column: 13
                                    },
                                    value: ExpressionKind::Integer(0),
                                }),
                            ),
                        }),
                        TokenType::Star,
                        Box::new(Expression {
                            position: Position {
                                line: 1,
                                column: 18
                            },
                            value: ExpressionKind::Integer(3),
                        }),
                    ),
                }),
            ),
        );
    }

    #[test]
    fn test_mixed_associativity_and_precedence() {
        test_expression!(
            "2 = 1 * 2 + 3 ** 5 - 1;",
            ExpressionKind::BinaryOperation(
                Box::new(Expression {
                    position: Position { line: 1, column: 1 },
                    value: ExpressionKind::Integer(2),
                }),
                TokenType::Equal,
                Box::new(Expression {
                    position: Position { line: 1, column: 5 },
                    value: ExpressionKind::BinaryOperation(
                        Box::new(Expression {
                            position: Position { line: 1, column: 5 },
                            value: ExpressionKind::BinaryOperation(
                                Box::new(Expression {
                                    position: Position { line: 1, column: 5 },
                                    value: ExpressionKind::BinaryOperation(
                                        Box::new(Expression {
                                            position: Position { line: 1, column: 5 },
                                            value: ExpressionKind::Integer(1),
                                        }),
                                        TokenType::Star,
                                        Box::new(Expression {
                                            position: Position { line: 1, column: 9 },
                                            value: ExpressionKind::Integer(2),
                                        }),
                                    ),
                                }),
                                TokenType::Plus,
                                Box::new(Expression {
                                    position: Position {
                                        line: 1,
                                        column: 13
                                    },
                                    value: ExpressionKind::BinaryOperation(
                                        Box::new(Expression {
                                            position: Position {
                                                line: 1,
                                                column: 13
                                            },
                                            value: ExpressionKind::Integer(3),
                                        }),
                                        TokenType::StarStar,
                                        Box::new(Expression {
                                            position: Position {
                                                line: 1,
                                                column: 18
                                            },
                                            value: ExpressionKind::Integer(5),
                                        }),
                                    ),
                                }),
                            ),
                        }),
                        TokenType::Minus,
                        Box::new(Expression {
                            position: Position {
                                line: 1,
                                column: 22
                            },
                            value: ExpressionKind::Integer(1),
                        }),
                    ),
                }),
            ),
        );
    }

    #[test]
    fn test_function_expression() {
        test_expression!(
            "(function() {});",
            ExpressionKind::Function {
                parameters: Vec::new(),
                body: Vec::new(),
            },
        );
    }

    #[test]
    fn test_boolean_expression_true() {
        test_expression!("true;", ExpressionKind::Boolean(true),);
    }

    #[test]
    fn test_boolean_expression_false() {
        test_expression!("false;", ExpressionKind::Boolean(false),);
    }

    #[test]
    fn test_char_literal() {
        test_expression!("'c';", ExpressionKind::Char('c'),);
    }

    #[test]
    fn test_char_escape_sequence() {
        test_expression!(r#"'\n';"#, ExpressionKind::Char('\n'),);
    }
}
