use std::iter::Peekable;
use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub(crate) enum Token<'a> {
    EOF,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Assign,
    Semicolon,
    Comma,
    Let,
    Func,
    Return,
    If,
    Else,
    True,
    False,
    Minus,
    UnOp(&'a str),
    BinOp(&'a str),
    Ident(&'a str),
    Num(u8),
    Illegal(&'a str),
}

const RADIX: u32 = 10;

pub(crate) fn get_tokens(string: &str) -> Vec<Token<'_>> {
    let mut tokens: Vec<Token<'_>> = Vec::with_capacity(string.len());
    let mut chars: Peekable<CharIndices<'_>> =
        string.char_indices().peekable();

    macro_rules! get_substring {
        ($fn:expr, $i:expr $(,)?) => {{
            let mut substring: &str = &string[$i..$i];
            while let Some((j, c)) = chars.peek() {
                substring = &string[$i..*j];
                if $fn(c) {
                    let _: Option<(usize, char)> = chars.next();
                } else {
                    break;
                }
            }
            substring
        }};
    }

    macro_rules! push_equality_or {
        ($bin_op:expr, $fallback:expr $(,)?) => {{
            if let Some((_, c)) = chars.peek() {
                if *c == '=' {
                    tokens.push(Token::BinOp($bin_op));
                    let _: Option<(usize, char)> = chars.next();
                    continue;
                }
            }
            tokens.push($fallback)
        }};
    }

    macro_rules! return_illegal {
        ($string:expr $(,)?) => {
            tokens.push(Token::Illegal($string));
            return tokens;
        };
    }

    macro_rules! do_other_char {
        ($i:expr, $c:expr $(,)?) => {{
            if $c.is_whitespace() {
                continue;
            } else if $c.is_alphabetic() {
                let substring: &str = get_substring!(
                    |c: &char| c.is_alphabetic() || (*c == '_'),
                    $i,
                );
                let token: Token<'_> = match substring {
                    "let" => Token::Let,
                    "fn" => Token::Func,
                    "return" => Token::Return,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "true" => Token::True,
                    "false" => Token::False,
                    _ => Token::Ident(substring),
                };
                tokens.push(token);
            } else if $c.is_digit(RADIX) {
                let substring: &str =
                    get_substring!(|c: &char| c.is_digit(RADIX), $i);
                if let Ok(n) = substring.parse() {
                    tokens.push(Token::Num(n))
                } else {
                    return_illegal!(substring);
                }
            } else {
                return_illegal!(&string[$i..$i + 1]);
            }
        }};
    }

    loop {
        if let Some((i, c)) = chars.next() {
            match c {
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '{' => tokens.push(Token::LBrace),
                '}' => tokens.push(Token::RBrace),
                ';' => tokens.push(Token::Semicolon),
                ',' => tokens.push(Token::Comma),
                '-' => tokens.push(Token::Minus),
                '=' => push_equality_or!("==", Token::Assign),
                '!' => push_equality_or!("!=", Token::UnOp("!")),
                '+' => tokens.push(Token::BinOp("+")),
                '*' => tokens.push(Token::BinOp("*")),
                '/' => tokens.push(Token::BinOp("/")),
                '<' => tokens.push(Token::BinOp("<")),
                '>' => tokens.push(Token::BinOp(">")),
                _ => do_other_char!(i, c),
            }
        } else {
            tokens.push(Token::EOF);
            return tokens;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{get_tokens, Token};

    #[test]
    fn single_char_tokens() {
        assert_eq!(
            get_tokens("=+(){},;\n"),
            vec![
                Token::Assign,
                Token::BinOp("+"),
                Token::LParen,
                Token::RParen,
                Token::LBrace,
                Token::RBrace,
                Token::Comma,
                Token::Semicolon,
                Token::EOF,
            ],
        )
    }

    #[test]
    fn multi_char_tokens() {
        assert_eq!(
            get_tokens(
                "let five = 5;\
                 let ten = 10;\
                 let add = fn(x, y) { x + y; };\
                 let result = add(five, ten);\
                 !-/*5;\
                 5 < 10 > 5;\
                 if (5 < 10) { return true; } else { return false; }
                 10 == 10;\
                 10 != 9;\n"
            ),
            vec![
                Token::Let,
                Token::Ident("five"),
                Token::Assign,
                Token::Num(5),
                Token::Semicolon,
                Token::Let,
                Token::Ident("ten"),
                Token::Assign,
                Token::Num(10),
                Token::Semicolon,
                Token::Let,
                Token::Ident("add"),
                Token::Assign,
                Token::Func,
                Token::LParen,
                Token::Ident("x"),
                Token::Comma,
                Token::Ident("y"),
                Token::RParen,
                Token::LBrace,
                Token::Ident("x"),
                Token::BinOp("+"),
                Token::Ident("y"),
                Token::Semicolon,
                Token::RBrace,
                Token::Semicolon,
                Token::Let,
                Token::Ident("result"),
                Token::Assign,
                Token::Ident("add"),
                Token::LParen,
                Token::Ident("five"),
                Token::Comma,
                Token::Ident("ten"),
                Token::RParen,
                Token::Semicolon,
                Token::UnOp("!"),
                Token::Minus,
                Token::BinOp("/"),
                Token::BinOp("*"),
                Token::Num(5),
                Token::Semicolon,
                Token::Num(5),
                Token::BinOp("<"),
                Token::Num(10),
                Token::BinOp(">"),
                Token::Num(5),
                Token::Semicolon,
                Token::If,
                Token::LParen,
                Token::Num(5),
                Token::BinOp("<"),
                Token::Num(10),
                Token::RParen,
                Token::LBrace,
                Token::Return,
                Token::True,
                Token::Semicolon,
                Token::RBrace,
                Token::Else,
                Token::LBrace,
                Token::Return,
                Token::False,
                Token::Semicolon,
                Token::RBrace,
                Token::Num(10),
                Token::BinOp("=="),
                Token::Num(10),
                Token::Semicolon,
                Token::Num(10),
                Token::BinOp("!="),
                Token::Num(9),
                Token::Semicolon,
                Token::EOF,
            ],
        )
    }
}
