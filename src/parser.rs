use crate::tokenizer::Token;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Debug, PartialEq)]
enum Statement<'a> {
    Let {
        ident: &'a str,
        value: Expression<'a>,
    },
    Return(Expression<'a>),
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

fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'a, Token<'b>>>,
) -> Option<Expression<'b>> {
    macro_rules! if_semicolon {
        ($x:expr) => {{
            if let Some(Token::Semicolon) = tokens.peek() {
                return Some($x);
            }
        }};
    }
    if let Some(t) = tokens.next() {
        match t {
            Token::Num(n) => if_semicolon!(Expression::Num(*n)),
            Token::Ident(x) => if_semicolon!(Expression::Ident(x)),
            _ => (),
        }
    }
    None
}

macro_rules! break_if_not {
    ($tokens:expr, $x:expr) => {
        if $tokens.next() != Some(&$x) {
            break;
        }
    };
}

macro_rules! push_let {
    ($tokens:expr, $ast:expr $(,)?) => {{
        let ident: &str = if let Some(x) = get_ident(&mut $tokens) {
            x
        } else {
            break;
        };
        break_if_not!($tokens, Token::Assign);
        let value: Expression<'_> = if let Some(x) = get_expr(&mut $tokens) {
            x
        } else {
            break;
        };
        break_if_not!($tokens, Token::Semicolon);
        $ast.push(Statement::Let { ident, value });
    }};
}

macro_rules! push_return {
    ($tokens:expr, $ast:expr $(,)?) => {{
        let expression: Expression<'_> =
            if let Some(x) = get_expr(&mut $tokens) {
                x
            } else {
                break;
            };
        break_if_not!($tokens, Token::Semicolon);
        $ast.push(Statement::Return(expression));
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
                Token::Return => push_return!(tokens, ast),
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

    #[test]
    fn return_statement() {
        assert_eq!(
            get_ast(&get_tokens("return 5;\n")),
            Some(vec![Statement::Return(Expression::Num(5))]),
        );
    }

    #[test]
    fn let_return_statement() {
        assert_eq!(
            get_ast(&get_tokens("let x = 5;\nreturn x;\n")),
            Some(vec![
                Statement::Let {
                    ident: "x",
                    value: Expression::Num(5),
                },
                Statement::Return(Expression::Ident("x")),
            ]),
        );
    }
}
