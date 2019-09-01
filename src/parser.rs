use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone, Copy)]
enum TokenType {
    Identifier,
    Integer,
    Double,
    String,
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
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next().unwrap()
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
    fn test_language_snippet() {
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
    fn test_iterator() {
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
}
