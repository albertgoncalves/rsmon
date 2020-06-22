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

fn get_prefix_binding_power(op: &str) -> u8 {
    match op {
        "-" | "!" => 9,
        _ => panic!(),
    }
}

fn get_infix_binding_power(op: &str) -> (u8, u8) {
    match op {
        "*" | "/" => (7, 8),
        "+" | "-" => (5, 6),
        "<" | ">" => (3, 4),
        "==" | "!=" => (1, 2),
        _ => panic!(),
    }
}

/* NOTE: See
 *  `https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html`.
 */
fn get_expr<'a, 'b>(
    tokens: &mut Peekable<Iter<'a, Token<'b>>>,
    precedence: u8,
) -> Option<Expression<'b>> {
    let mut expression: Option<Expression<'_>> = None;

    macro_rules! set_prefix {
        ($op:expr $(,)?) => {{
            if let Some(x) = get_expr(tokens, get_prefix_binding_power($op)) {
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
            let (l_power, r_power): (u8, u8) = get_infix_binding_power($op);
            if l_power < precedence {
                break;
            }
            eat_token!(tokens);
            if let (Some(left), Some(right)) =
                (expression, get_expr(tokens, r_power))
            {
                expression = Some(Expression::Infix {
                    op: $op,
                    left: Box::new(left),
                    right: Box::new(right),
                });
            } else {
                return None;
            }
        }};
    }

    if let Some(t) = tokens.next() {
        match t {
            Token::LParen => {
                expression = get_expr(tokens, 0);
                if tokens.next() != Some(&Token::RParen) {
                    return None;
                }
            }
            Token::Num(n) => expression = Some(Expression::Num(*n)),
            Token::Ident(i) => expression = Some(Expression::Ident(i)),
            Token::Minus => set_prefix!("-"),
            Token::UnOp(o) => set_prefix!(*o),
            _ => return None,
        }
    }
    while let Some(t) = tokens.peek() {
        match t {
            Token::Semicolon => break,
            Token::RParen => return expression,
            Token::Minus => set_infix!("-"),
            Token::BinOp(o) => set_infix!(*o),
            _ => eat_token!(tokens),
        }
    }
    expression
}

fn get_ast<'a>(tokens: &[Token<'a>]) -> Option<Vec<Statement<'a>>> {
    let mut ast: Vec<Statement<'_>> = Vec::with_capacity(tokens.len());
    let mut tokens: Peekable<Iter<'_, Token<'_>>> = tokens.iter().peekable();

    macro_rules! get_expr_or_break {
        () => {
            if let Some(x) = get_expr(&mut tokens, 0) {
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
                    if let Some(x) = get_expr(&mut tokens, 0) {
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

    macro_rules! stmt {
        (Let, $ident:expr, $value:expr $(,)?) => {
            Statement::Let {
                ident: $ident,
                value: $value,
            }
        };
        (Return, $expr:expr $(,)?) => {
            Statement::Return($expr)
        };
        (Orphan, $expr:expr $(,)?) => {
            Statement::Orphan($expr)
        };
    }

    macro_rules! expr {
        (Num, $n:expr $(,)?) => {
            Expression::Num($n)
        };
        (Ident, $i:expr $(,)?) => {
            Expression::Ident($i)
        };
        (Prefix, $op:expr, $value:expr $(,)?) => {
            Expression::Prefix {
                op: $op,
                value: Box::new($value),
            }
        };
        (Infix, $op:expr, $left:expr, $right:expr $(,)?) => {
            Expression::Infix {
                op: $op,
                left: Box::new($left),
                right: Box::new($right),
            }
        };
    }

    macro_rules! test_eq {
        ($test:ident, $input:expr, $output:expr $(,)?) => {
            #[test]
            fn $test() {
                assert_eq!(get_ast(&get_tokens($input)), Some($output))
            }
        };
    }

    macro_rules! test_all_none {
        ($test:ident, $inputs:expr $(,)?) => {
            #[test]
            fn $test() {
                for x in $inputs {
                    assert_eq!(get_ast(&get_tokens(x)), None);
                }
            }
        };
    }

    test_eq!(
        let_statements,
        "let x = 5;\nlet y = x;\n",
        vec![
            stmt!(Let, "x", expr!(Num, 5)),
            stmt!(Let, "y", expr!(Ident, "x")),
        ],
    );

    test_all_none!(
        fail_let_statement,
        &[
            "let x;\n",
            "let x =;\n",
            "let x = 5\n",
            "let x 10;\n",
            "let x = 5;\nlet y =;\n",
        ],
    );

    test_eq!(
        return_statement,
        "return 5;\n",
        vec![stmt!(Return, expr!(Num, 5))],
    );

    test_eq!(
        let_return_statement,
        "let x = 5;\nreturn x;\n",
        vec![
            stmt!(Let, "x", expr!(Num, 5)),
            stmt!(Return, expr!(Ident, "x")),
        ],
    );

    test_all_none!(fail_return_statement, &["return;\n", "return\n"]);

    test_eq!(orphan_num, "1;\n", vec![stmt!(Orphan, expr!(Num, 1))]);

    test_eq!(orphan_ident, "x;\n", vec![stmt!(Orphan, expr!(Ident, "x"))]);

    test_all_none!(fail_orphan_ident, &["x\n", "x; y\n"]);

    test_eq!(
        orphan_negative_num,
        "-1;\n",
        vec![stmt!(Orphan, expr!(Prefix, "-", expr!(Num, 1)))],
    );

    test_eq!(
        orphan_negative_ident,
        "-x;\n",
        vec![stmt!(Orphan, expr!(Prefix, "-", expr!(Ident, "x")))],
    );

    test_eq!(
        let_negative_num,
        "let x = -1;\n",
        vec![stmt!(Let, "x", expr!(Prefix, "-", expr!(Num, 1)))],
    );

    test_eq!(
        let_negative_ident,
        "let x = -y;\n",
        vec![stmt!(Let, "x", expr!(Prefix, "-", expr!(Ident, "y")))],
    );

    test_eq!(
        orphan_bang_ident,
        "!x;\n",
        vec![stmt!(Orphan, expr!(Prefix, "!", expr!(Ident, "x")))],
    );

    test_eq!(
        let_bang_ident,
        "let x = !y;\n",
        vec![stmt!(Let, "x", expr!(Prefix, "!", expr!(Ident, "y")))],
    );

    test_eq!(
        let_infix_add_nums,
        "let x = 2 + 1;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(Infix, "+", expr!(Num, 2), expr!(Num, 1)),
        )],
    );

    test_eq!(
        let_infix_sub_nums,
        "let x = 2 - 1;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(Infix, "-", expr!(Num, 2), expr!(Num, 1)),
        )],
    );

    test_eq!(
        let_parens_infix,
        "let x = (1 + 2) * 3;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "*",
                expr!(Infix, "+", expr!(Num, 1), expr!(Num, 2)),
                expr!(Num, 3),
            ),
        )],
    );

    test_eq!(
        let_many_parens_infix,
        "let x = (((1 + 2) * 3) - 4) / 5;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "/",
                expr!(
                    Infix,
                    "-",
                    expr!(
                        Infix,
                        "*",
                        expr!(Infix, "+", expr!(Num, 1), expr!(Num, 2)),
                        expr!(Num, 3),
                    ),
                    expr!(Num, 4),
                ),
                expr!(Num, 5),
            ),
        )]
    );

    test_eq!(
        let_many_precedence_parens_infix,
        "let x = ((1 + 2 * 3) / 4) - 5 * 6 ;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "-",
                expr!(
                    Infix,
                    "/",
                    expr!(
                        Infix,
                        "+",
                        expr!(Num, 1),
                        expr!(Infix, "*", expr!(Num, 2), expr!(Num, 3)),
                    ),
                    expr!(Num, 4),
                ),
                expr!(Infix, "*", expr!(Num, 5), expr!(Num, 6)),
            ),
        )],
    );

    test_all_none!(
        fail_many_parens_infix,
        &[
            "let x = (((1 + 2) * 3) - 4) / 5);\n",
            "let x = (((1 + 2) * 3) - 4 / 5;\n",
            "let x = (((1 + 2 * 3) - 4) / 5;\n",
            "let x = ((1 + 2) * 3) - 4) / 5;\n",
        ],
    );

    test_eq!(
        let_infix_add_precedence,
        "let x = 1 + 2 + 3;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "+",
                expr!(Infix, "+", expr!(Num, 1), expr!(Num, 2)),
                expr!(Num, 3),
            ),
        )],
    );

    test_eq!(
        let_infix_add_mul_precedence,
        "let x = 1 + 2 * 3;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "+",
                expr!(Num, 1),
                expr!(Infix, "*", expr!(Num, 2), expr!(Num, 3)),
            ),
        )],
    );

    test_eq!(
        let_infix_mul_add_precedence,
        "let x = 1 * 2 + 3;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "+",
                expr!(Infix, "*", expr!(Num, 1), expr!(Num, 2)),
                expr!(Num, 3),
            ),
        )],
    );

    test_eq!(
        let_infix_mul_add_negative_precedence,
        "let x = -1 * -2 + -3;\n",
        vec![stmt!(
            Let,
            "x",
            expr!(
                Infix,
                "+",
                expr!(
                    Infix,
                    "*",
                    expr!(Prefix, "-", expr!(Num, 1)),
                    expr!(Prefix, "-", expr!(Num, 2)),
                ),
                expr!(Prefix, "-", expr!(Num, 3)),
            ),
        )],
    );

    test_eq!(
        conditional_precedence,
        "1 < -2 != 3 > -4;",
        vec![stmt!(
            Orphan,
            expr!(
                Infix,
                "!=",
                expr!(
                    Infix,
                    "<",
                    expr!(Num, 1),
                    expr!(Prefix, "-", expr!(Num, 2)),
                ),
                expr!(
                    Infix,
                    ">",
                    expr!(Num, 3),
                    expr!(Prefix, "-", expr!(Num, 4)),
                ),
            ),
        )],
    );

    test_eq!(
        conditional_precedence_parens,
        "!(1 < -2 == !(3 > -4));",
        vec![stmt!(
            Orphan,
            expr!(
                Prefix,
                "!",
                expr!(
                    Infix,
                    "==",
                    expr!(
                        Infix,
                        "<",
                        expr!(Num, 1),
                        expr!(Prefix, "-", expr!(Num, 2)),
                    ),
                    expr!(
                        Prefix,
                        "!",
                        expr!(
                            Infix,
                            ">",
                            expr!(Num, 3),
                            expr!(Prefix, "-", expr!(Num, 4)),
                        ),
                    ),
                ),
            ),
        )],
    );

    test_eq!(
        stacked_prefix,
        "!-1;",
        vec![stmt!(
            Orphan,
            expr!(Prefix, "!", expr!(Prefix, "-", expr!(Num, 1))),
        )],
    );
}
