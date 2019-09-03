use std::iter::Peekable;
use std::str::Chars;

use crate::agent::Agent;

#[derive(Debug, PartialEq, Clone, Copy)]
enum TokenType {
    Identifier,
    Integer,
    Double,
    String,
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
    Bang,
    BangEqual,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
    Comma,

    Return,
    Function,
    For,
    If,
    Else,
    While,
    Break,
    Continue,
    Let,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct Position {
    line: usize,
    column: usize,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Debug, PartialEq, Clone)]
struct Token {
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

    pub fn lbp(&self) -> usize {
        match self.typ {
            TokenType::Integer => 0,
            TokenType::Double => 0,
            TokenType::String => 0,
            TokenType::Null => 0,

            TokenType::Equal => 1,
            TokenType::PipePipe => 2,
            TokenType::AndAnd => 3,
            TokenType::EqualEqual => 4,
            TokenType::BangEqual => 4,
            TokenType::LessThan => 5,
            TokenType::LessThanEqual => 5,
            TokenType::GreaterThan => 5,
            TokenType::GreaterThanEqual => 5,
            TokenType::Plus => 6,
            TokenType::Minus => 6,
            TokenType::Star => 7,
            TokenType::Slash => 8,
            TokenType::Percent => 9,
            TokenType::StarStar => 10,
            TokenType::LeftParen => 12,
            TokenType::LeftBracket => 12,

            TokenType::Semicolon => 0,
            TokenType::RightParen => 0,
            TokenType::RightBracket => 0,
            TokenType::LeftBrace => 0,
            TokenType::Comma => 0,

            _ => panic!("Trying to get lbp of {:?}", self),
        }
    }

    pub fn rbp(&self) -> usize {
        match self.typ {
            TokenType::LeftParen => 0,
            TokenType::LeftBracket => 0,
            TokenType::Bang => 11,
            TokenType::Minus => 11,

            _ => unreachable!(),
        }
    }
}

struct Lexer<'a> {
    position: usize,
    line: usize,
    column: usize,
    input: &'a str,
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer {
            position: 0,
            line: 1,
            column: 1,
            input,
            chars: input.chars().peekable(),
        }
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
                Some(Err(format!($message, $($stuff)*)))
            };
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
                    while let Some(c) = self.next_char() {
                        if c == '\n' {
                            break;
                        }
                    }
                    start += 1;
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
                        _ => token!(TokenType::Identifier),
                    }
                }

                '"' => {
                    while let Some(c) = self.peek_char() {
                        match c {
                            '\\' => {
                                self.next_char();
                                if let Some(c) = self.next_char() {
                                    match c {
                                        'n' | 't' | '"' => {}
                                        _ => return error!("unrecognized escape sequence"),
                                    }
                                } else {
                                    return error!("unterminated escape sequence");
                                }
                            }
                            '"' => {
                                self.next_char();
                                break;
                            }
                            _ => {
                                self.next_char();
                            }
                        }
                    }

                    token!(TokenType::String);
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
struct Statement {
    position: Position,
    value: StatementKind,
}

#[allow(clippy::large_enum_variant)]
#[derive(Debug, PartialEq, Clone)]
enum StatementKind {
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
}

#[derive(Debug, PartialEq, Clone)]
struct Expression {
    position: Position,
    value: ExpressionKind,
}

#[derive(Debug, PartialEq, Clone)]
enum ExpressionKind {
    Identifier(usize),
    Integer(i64),
    Double(f64),
    String(String),
    Null,
    Array(Vec<Expression>),
    Function {
        name: Option<usize>,
        parameters: Vec<Expression>,
        body: Vec<Statement>,
    },
    UnaryOperation(TokenType, Box<Expression>),
    BinaryOperation(Box<Expression>, TokenType, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Index(Box<Expression>, Box<Expression>),
}

pub type ParseResult<T> = Result<T, String>;

struct Parser<'a> {
    agent: &'a mut Agent,
    lexer: Peekable<Lexer<'a>>,
    current_token: Option<Token>,
}

impl<'a> Parser<'a> {
    pub fn new(agent: &'a mut Agent, lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            agent,
            lexer: lexer.peekable(),
            current_token: None,
        }
    }

    pub fn next_statement(&mut self) -> Option<ParseResult<Statement>> {
        match self.peek() {
            Ok(Some(_)) => Some(self.parse_statement()),
            Err(msg) => Some(Err(msg.clone())),
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
            Some(Err(msg)) => Err(msg.clone()),
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
        if let Some(token) = self.peek()? {
            match token.typ {
                TokenType::Let => self.parse_let_declaration(),
                TokenType::Function => self.parse_function_declaration(),
                TokenType::If => self.parse_if_statement(),
                TokenType::While => self.parse_while_statement(),
                TokenType::For => self.parse_for_statement(),
                TokenType::Continue => self.parse_continue_statement(),
                TokenType::Break => self.parse_break_statement(),
                TokenType::Return => self.parse_return_statement(),
                _ => self.parse_expression_statement(),
            }
        } else {
            unreachable!();
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

        if let ExpressionKind::Identifier(_) = ident.value {
        } else {
            return Err(format!("Expected identifier at {}", ident.position));
        }

        self.expect(TokenType::LeftParen)?;

        let params = self.parse_list(
            TokenType::RightParen,
            TokenType::Comma,
            Self::parse_expression,
            |ident| {
                if let ExpressionKind::Identifier(_) = ident.value {
                    Ok(())
                } else {
                    Err(format!("Expected identifier at {}", ident.position))
                }
            },
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
            TokenType::Null => self.parse_null_expression(token),
            TokenType::LeftParen => self.parse_parenthesized_expression(token),
            TokenType::LeftBracket => self.parse_array_expression(token),

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
            | TokenType::StarStar
            | TokenType::LessThan
            | TokenType::LessThanEqual
            | TokenType::GreaterThan
            | TokenType::GreaterThanEqual
            | TokenType::EqualEqual
            | TokenType::BangEqual => self.parse_left_assoc_binary(token, left),
            TokenType::Equal => self.parse_right_assoc_binary(token, left),
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
        let mut token = self.peek()?.cloned();
        let mut left = self.nud(t)?;

        while token.is_some() && rbp < token.unwrap().lbp() {
            t = some!(self.next_token()?);
            token = self.peek()?.cloned();
            left = self.led(t, left)?;
        }

        Ok(left)
    }

    fn parse_identifier_expression(&mut self, ident: Token) -> ParseResult<Expression> {
        let s = ident.text.to_string();
        let id = self.agent.intern_string(s.as_ref());
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
        Ok(Expression {
            position: string.position,
            value: ExpressionKind::String(string.text),
        })
    }

    fn parse_null_expression(&mut self, null: Token) -> ParseResult<Expression> {
        Ok(Expression {
            position: null.position,
            value: ExpressionKind::Null,
        })
    }

    fn parse_parenthesized_expression(&mut self, _: Token) -> ParseResult<Expression> {
        let expr = self.parse_expression()?;
        self.expect(TokenType::RightParen)?;
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

    fn parse_left_assoc_binary(&mut self, op: Token, left: Expression) -> ParseResult<Expression> {
        let right = self.parse_expression_inner(op.lbp())?;

        Ok(Expression {
            position: left.position,
            value: ExpressionKind::BinaryOperation(Box::new(left), op.typ, Box::new(right)),
        })
    }

    fn parse_right_assoc_binary(&mut self, op: Token, left: Expression) -> ParseResult<Expression> {
        let right = self.parse_expression_inner(op.lbp() - 1)?;

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

        Ok(Expression {
            position: arr.position,
            value: ExpressionKind::Index(Box::new(arr), Box::new(index)),
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
        let lexer = Lexer::new(input);

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
        let lexer = Lexer::new(input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(TokenType::Integer, 1, 1, "123")]
        );
    }

    #[test]
    fn test_double() {
        let input = "1.23";
        let lexer = Lexer::new(input);

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
        let lexer = Lexer::new(input);

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
        let lexer = Lexer::new(input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(TokenType::String, 1, 1, r#""this is a string""#),]
        );
    }

    #[test]
    fn test_string_escapes() {
        let input = r#""so I says, \"this\nis\tan escaped string\"""#;
        let lexer = Lexer::new(input);

        assert_eq!(
            lexer.filter_map(|a| a.ok()).collect::<Vec<_>>(),
            vec![Token::new(
                TokenType::String,
                1,
                1,
                r#""so I says, \"this\nis\tan escaped string\"""#
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
        let lexer = Lexer::new(input);

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
                (TokenType::String, r#""hello world""#.to_string()),
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
        let lexer = Lexer::new(input);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

        assert_eq!(
            parser.collect::<Vec<_>>(),
            vec![Err("Expected Semicolon, found end of input".to_string()),],
        );
    }

    #[test]
    fn test_keyword_as_identifier() {
        let mut agent = Agent::new();
        let input = "let while = ok;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(&mut agent, lexer);

        assert_eq!(
            parser.next().unwrap(),
            Err("Expected Identifier, got While at 1:5".to_string()),
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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
    fn test_expression_statement() {
        let mut agent = Agent::new();
        let input = "123;";
        let lexer = Lexer::new(input);
        let parser = Parser::new(&mut agent, lexer);

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
        ($input:expr, $result:expr) => {{
            let agent = Agent::new();
            test_expression!($input, $result, agent);
        }};
        ($input:expr, $result:expr, $agent:expr) => {{
            let mut agent = $agent;
            let input = $input;
            let lexer = Lexer::new(input);
            let parser = Parser::new(&mut agent, lexer);

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
}
