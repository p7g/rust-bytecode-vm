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

    Return,
    Function,
    For,
    While,
    Let,
}

#[derive(Debug, PartialEq)]
struct Token<'a> {
    line: usize,
    column: usize,
    typ: TokenType,
    text: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(typ: TokenType, line: usize, column: usize, text: &'a str) -> Token {
        Token {
            line,
            column,
            typ,
            text,
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

    pub fn all(&mut self) -> Vec<Token<'a>> {
        let mut tokens = Vec::new();
        while let Ok(Some(token)) = self.next() {
            tokens.push(token);
        }
        tokens
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

    pub fn next(&mut self) -> Result<Option<Token<'a>>, String> {
        let mut start = self.position;
        let mut start_line = self.line;
        let mut start_column = self.column;

        macro_rules! token {
            ($typ:expr) => {{
                return Ok(Some(Token::new(
                    $typ,
                    start_line,
                    start_column,
                    &self.input[start..self.position],
                )));
            }};
        }

        macro_rules! error {
            ($message:expr $(, $stuff:expr)* $(,)?) => {
                Err(format!($message, $($stuff)*))
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
                '*' => or2!('*', TokenType::Star, TokenType::StarStar),
                '&' => or2!('&', TokenType::And, TokenType::AndAnd),
                '|' => or2!('|', TokenType::Pipe, TokenType::PipePipe),

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

        Ok(None)
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
        let input = "[ ] { } ( ) + - * ** / & && | || ^ % ;";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.all(),
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
            ],
        );
    }

    #[test]
    fn test_integer() {
        let input = "123";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.all(),
            vec![Token::new(TokenType::Integer, 1, 1, "123")]
        );
    }

    #[test]
    fn test_double() {
        let input = "1.23";
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.all(),
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
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.all(),
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
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.all(),
            vec![Token::new(TokenType::String, 1, 1, r#""this is a string""#),]
        );
    }

    #[test]
    fn test_string_escapes() {
        let input = r#""so I says, \"this\nis\tan escaped string\"""#;
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer.all(),
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
        let mut lexer = Lexer::new(input);

        assert_eq!(
            lexer
                .all()
                .iter()
                .map(|tok| (tok.typ, tok.text))
                .collect::<Vec<(TokenType, &str)>>(),
            vec![
                (TokenType::Function, "function"),
                (TokenType::Identifier, "testing"),
                (TokenType::LeftParen, "("),
                (TokenType::RightParen, ")"),
                (TokenType::LeftBrace, "{"),
                (TokenType::Return, "return"),
                (TokenType::String, r#""hello world""#),
                (TokenType::Semicolon, ";"),
                (TokenType::RightBrace, "}"),
                (TokenType::Identifier, "print"),
                (TokenType::LeftParen, "("),
                (TokenType::Identifier, "testing"),
                (TokenType::LeftParen, "("),
                (TokenType::RightParen, ")"),
                (TokenType::RightParen, ")"),
                (TokenType::Semicolon, ";"),
            ],
        );
    }

    #[test]
    fn test_iterator() {
        let input = "test 1 1.2";
        let lexer = Lexer::new(input);

        assert_eq!(
            lexer.map(|t| (t.typ, t.text)).collect::<Vec<_>>(),
            vec![
                (TokenType::Identifier, "test"),
                (TokenType::Integer, "1"),
                (TokenType::Double, "1.2")
            ]
        );
    }
}
