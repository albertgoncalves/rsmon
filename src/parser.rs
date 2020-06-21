use crate::tokenizer::Token;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    Let {
        ident: &'a str,
        value: Expression<'a>,
    },
}

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    Num(u8),
    Ident(&'a str),
}

fn get_ident<'a, 'b>(
    tokens: &mut Peekable<Iter<'a, Token<'b>>>,
) -> Option<&'b str> {
    if let Some(Token::Ident(x)) = tokens.next() {
        Some(x)
    } else {
        None
    }
}

macro_rules! if_semicolon {
    ($tokens:expr, $x:expr) => {{
        if let Some(Token::Semicolon) = $tokens.peek() {
            return Some($x);
        }
    }};
}

fn get_expression<'a, 'b>(
    tokens: &mut Peekable<Iter<'a, Token<'b>>>,
) -> Option<Expression<'b>> {
    if let Some(t) = tokens.next() {
        match t {
            Token::Num(n) => if_semicolon!(tokens, Expression::Num(*n)),
            Token::Ident(x) => if_semicolon!(tokens, Expression::Ident(x)),
            _ => (),
        }
    }
    None
}

macro_rules! push_let {
    ($tokens:expr, $ast:expr $(,)?) => {{
        let ident: &str = if let Some(x) = get_ident(&mut $tokens) {
            x
        } else {
            break;
        };
        if $tokens.next() != Some(&Token::Assign) {
            break;
        }
        let value: Expression<'_> =
            if let Some(x) = get_expression(&mut $tokens) {
                x
            } else {
                break;
            };
        if $tokens.next() != Some(&Token::Semicolon) {
            break;
        }
        $ast.push(Statement::Let { ident, value });
    }};
}

fn get_ast<'a>(tokens: &[Token<'a>]) -> Option<Vec<Statement<'a>>> {
    let mut ast: Vec<Statement<'_>> = Vec::with_capacity(tokens.len());
    let mut tokens: Peekable<Iter<'_, Token<'_>>> = tokens.iter().peekable();
    loop {
        if let Some(t) = tokens.next() {
            match t {
                Token::EOF => return Some(ast),
                Token::Let => push_let!(tokens, ast),
                _ => (),
            }
        } else {
            break;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::{get_ast, Expression, Statement};
    use crate::tokenizer::get_tokens;

    #[test]
    fn let_statements() {
        assert_eq!(
            get_ast(&get_tokens("let x = 5;\nlet y = x;\n")),
            Some(vec![
                Statement::Let {
                    ident: "x",
                    value: Expression::Num(5),
                },
                Statement::Let {
                    ident: "y",
                    value: Expression::Ident("x"),
                },
            ]),
        );
    }
}
