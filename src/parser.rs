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
    Orphan(Expression<'a>),
}

#[derive(Debug, PartialEq)]
enum Expression<'a> {
    Num(u8),
    Ident(&'a str),
    Prefix {
        op: &'a str,
        value: Box<Expression<'a>>,
    },
    Infix {
        op: &'a str,
        left: Box<Expression<'a>>,
        right: Box<Expression<'a>>,
    },
}

macro_rules! eat_token {
    ($tokens:expr $(,)?) => {{
        let _: Option<&Token<'_>> = $tokens.next();
    }};
}

fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'a, Token<'b>>>,
) -> Option<Expression<'b>> {
    let mut expression: Option<Expression<'_>> = None;
    macro_rules! set_prefix {
        ($op:expr $(,)?) => {{
            if let Some(x) = get_expr(tokens) {
                expression = Some(Expression::Prefix {
                    op: $op,
                    value: Box::new(x),
                });
            } else {
                return None;
            }
        }};
    }
    macro_rules! set_infix {
        ($op:expr $(,)?) => {{
            eat_token!(tokens);
            if let Some(left) = expression {
                if let Some(right) = get_expr(tokens) {
                    expression = Some(Expression::Infix {
                        op: $op,
                        left: Box::new(left),
                        right: Box::new(right),
                    });
                } else {
                    return None;
                }
            }
        }};
    }
    if let Some(t) = tokens.next() {
        match t {
            Token::Num(n) => expression = Some(Expression::Num(*n)),
            Token::Ident(i) => expression = Some(Expression::Ident(i)),
            Token::Minus => set_prefix!("-"),
            Token::UnOp(o) => set_prefix!(*o),
            _ => (),
        }
    }
    if let Some(t) = tokens.peek() {
        match t {
            Token::Minus => set_infix!("-"),
            Token::BinOp(o) => set_infix!(*o),
            _ => (),
        }
    }
    expression
}

fn get_ast<'a>(tokens: &[Token<'a>]) -> Option<Vec<Statement<'a>>> {
    let mut ast: Vec<Statement<'_>> = Vec::with_capacity(tokens.len());
    let mut tokens: Peekable<Iter<'_, Token<'_>>> = tokens.iter().peekable();
    macro_rules! get_expr_or_break {
        () => {
            if let Some(x) = get_expr(&mut tokens) {
                x
            } else {
                break;
            };
        };
    }
    macro_rules! break_if_not {
        ($x:expr) => {
            if tokens.next() != Some(&$x) {
                break;
            }
        };
    }
    loop {
        if let Some(t) = tokens.peek() {
            match t {
                Token::EOF => return Some(ast),
                Token::Let => {
                    eat_token!(tokens);
                    let ident: &str =
                        if let Some(Token::Ident(i)) = tokens.next() {
                            i
                        } else {
                            break;
                        };
                    break_if_not!(Token::Assign);
                    let value: Expression<'_> = get_expr_or_break!();
                    break_if_not!(Token::Semicolon);
                    ast.push(Statement::Let { ident, value });
                }
                Token::Return => {
                    eat_token!(tokens);
                    let expression: Expression<'_> = get_expr_or_break!();
                    break_if_not!(Token::Semicolon);
                    ast.push(Statement::Return(expression));
                }
                _ => {
                    if let Some(x) = get_expr(&mut tokens) {
                        break_if_not!(Token::Semicolon);
                        ast.push(Statement::Orphan(x));
                    } else {
                        break;
                    }
                }
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

    macro_rules! test_eq {
        ($test:ident, $input:expr, $output:expr $(,)?) => {
            #[test]
            fn $test() {
                assert_eq!(get_ast(&get_tokens($input)), $output)
            }
        };
    }

    macro_rules! test_all_eq {
        ($test:ident, $inputs:expr, $output:expr $(,)?) => {
            #[test]
            fn $test() {
                for x in $inputs {
                    assert_eq!(get_ast(&get_tokens(x)), $output);
                }
            }
        };
    }

    test_eq!(
        let_statements,
        "let x = 5;\nlet y = x;\n",
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

    test_all_eq!(
        fail_let_statement,
        &[
            "let x;\n",
            "let x =;\n",
            "let x = 5\n",
            "let x 10;\n",
            "let x = 5;\nlet y =;\n",
        ],
        None,
    );

    test_eq!(
        return_statement,
        "return 5;\n",
        Some(vec![Statement::Return(Expression::Num(5))]),
    );

    test_eq!(
        let_return_statement,
        "let x = 5;\nreturn x;\n",
        Some(vec![
            Statement::Let {
                ident: "x",
                value: Expression::Num(5),
            },
            Statement::Return(Expression::Ident("x")),
        ]),
    );

    test_eq!(fail_return_statement, "return;\n", None);

    test_eq!(
        orphan_num,
        "1;\n",
        Some(vec![Statement::Orphan(Expression::Num(1))])
    );

    test_eq!(
        orphan_ident,
        "x;\n",
        Some(vec![Statement::Orphan(Expression::Ident("x"))]),
    );

    test_all_eq!(fail_orphan_ident, &["x\n", "x; y\n"], None);

    test_eq!(
        orphan_negative_num,
        "-1;\n",
        Some(vec![Statement::Orphan(Expression::Prefix {
            op: "-",
            value: Box::new(Expression::Num(1)),
        })]),
    );

    test_eq!(
        orphan_negative_ident,
        "-x;\n",
        Some(vec![Statement::Orphan(Expression::Prefix {
            op: "-",
            value: Box::new(Expression::Ident("x")),
        })]),
    );

    test_eq!(
        let_negative_num,
        "let x = -1;\n",
        Some(vec![Statement::Let {
            ident: "x",
            value: Expression::Prefix {
                op: "-",
                value: Box::new(Expression::Num(1)),
            },
        }]),
    );

    test_eq!(
        let_negative_ident,
        "let x = -y;\n",
        Some(vec![Statement::Let {
            ident: "x",
            value: Expression::Prefix {
                op: "-",
                value: Box::new(Expression::Ident("y")),
            },
        }]),
    );

    test_eq!(
        orphan_bang_ident,
        "!x;\n",
        Some(vec![Statement::Orphan(Expression::Prefix {
            op: "!",
            value: Box::new(Expression::Ident("x")),
        })]),
    );

    test_eq!(
        let_bang_ident,
        "let x = !y;\n",
        Some(vec![Statement::Let {
            ident: "x",
            value: Expression::Prefix {
                op: "!",
                value: Box::new(Expression::Ident("y")),
            },
        }]),
    );

    test_eq!(
        let_infix_add_nums,
        "let x = 2 + 1;\n",
        Some(vec![Statement::Let {
            ident: "x",
            value: Expression::Infix {
                op: "+",
                left: Box::new(Expression::Num(2)),
                right: Box::new(Expression::Num(1)),
            },
        }]),
    );

    test_eq!(
        let_infix_sub_nums,
        "let x = 2 - 1;\n",
        Some(vec![Statement::Let {
            ident: "x",
            value: Expression::Infix {
                op: "-",
                left: Box::new(Expression::Num(2)),
                right: Box::new(Expression::Num(1)),
            },
        }]),
    );
}
