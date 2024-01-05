use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, char, multispace1},
    error::{self, ErrorKind},
    sequence::delimited,
    Finish, Parser,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp {
    // A Vec with >= 2 len
    Appl(Vec<Exp>),
    Builtin(Builtin),
    Const(String),
    Lambda { arg: String, body: Box<Exp> },
    // A Vec as stack for lists
    Q(Vec<Exp>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Builtin {
    Eval,
    Hd,
    Tl,
}

impl Exp {
    fn substitute(&mut self, from: &str, to: &Self) {
        match self {
            Exp::Appl(x) => {
                for y in x {
                    y.substitute(from, to);
                }
            }
            Exp::Const(x) => {
                if x == from {
                    *self = to.clone();
                }
            }
            Exp::Lambda { arg, body } => {
                if arg != from {
                    body.substitute(from, to);
                }
            }
            Exp::Q(_) | Exp::Builtin(_) => {}
        }
    }
}

pub fn parse(input: &str) -> Result<Exp, error::Error<&str>> {
    let exp = exp(input).finish()?;
    if exp.0.is_empty() {
        Ok(exp.1)
    } else {
        Err(error::Error {
            input: exp.0,
            code: ErrorKind::NonEmpty,
        })
    }
}

fn exp(input: &str) -> nom::IResult<&str, Exp> {
    alt((
        tag("hd").map(|_| Exp::Builtin(Builtin::Hd)),
        tag("tl").map(|_| Exp::Builtin(Builtin::Tl)),
        tag("eval").map(|_| Exp::Builtin(Builtin::Eval)),
        alpha1.map(|x: &str| Exp::Const(x.to_string())),
        delimited(
            char('('),
            alt((
                nom::sequence::tuple((alpha1, char('.'), exp)).map(|x| Exp::Lambda {
                    arg: x.0.to_string(),
                    body: Box::new(x.2),
                }),
                nom::multi::separated_list1(multispace1, exp)
                    .and_then(|x: Vec<Exp>| {
                        if x.len() < 2 {
                            Err(nom::Err::Error(error::Error {
                                input,
                                code: ErrorKind::SeparatedList,
                            }))
                        } else {
                            Ok((vec![], x))
                        }
                    })
                    .map(Exp::Appl),
            )),
            char(')'),
        ),
        delimited(
            char('{'),
            nom::multi::separated_list0(multispace1, exp),
            char('}'),
        )
        .map(|mut x| {
            x.reverse();
            Exp::Q(x)
        }),
    ))(input)
}

#[must_use]
pub fn eval(exp: &Exp) -> Exp {
    match exp {
        Exp::Appl(x) => {
            let mut acc = x[0].clone();
            let mut it = x.iter().peekable();
            it.next();
            while let Some(y) = it.peek() {
                if let Exp::Lambda { arg, mut body } = acc {
                    body.substitute(&arg, y);
                    acc = *body;
                    it.next();
                } else {
                    break;
                }
            }
            if let Exp::Builtin(ref x) = acc {
                if let Some(Exp::Q(y)) = it.peek() {
                    match x {
                        Builtin::Hd => {
                            acc = Exp::Q(match y.last() {
                                Some(z) => vec![z.clone()],
                                None => Vec::new(),
                            });
                        }
                        Builtin::Tl => {
                            let mut tail = y.clone();
                            tail.pop();
                            acc = Exp::Q(tail);
                        }
                        Builtin::Eval => acc = eval(&Exp::Appl(y.clone())),
                    }
                    it.next();
                }
            }
            if it.len() > 0 {
                let mut tmp = vec![eval(&acc)];
                tmp.extend(it.map(eval));
                Exp::Appl(tmp)
            } else {
                eval(&acc)
            }
        }
        Exp::Lambda { arg, body } => Exp::Lambda {
            arg: arg.clone(),
            body: Box::new(eval(body)),
        },
        x @ (Exp::Const(_) | Exp::Q(_) | Exp::Builtin(_)) => x.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Builtin::{Eval, Hd, Tl};
    use Exp::{Appl, Const, Lambda, Q};

    #[test]
    fn parse_t_1() {
        assert_eq!(
            parse("(hey way)").unwrap(),
            Appl(vec![Const("hey".to_string()), Const("way".to_string())])
        );
    }

    #[test]
    fn parse_t_2() {
        assert!(parse("hey way").is_err());
    }

    #[test]
    fn parse_t_3() {
        assert_eq!(parse("good").unwrap(), Const("good".to_string()));
    }

    #[test]
    fn parse_t_4() {
        assert_eq!(
            parse("((hey way) eey)").unwrap(),
            Appl(vec![
                Appl(vec![Const("hey".to_string()), Const("way".to_string())]),
                Const("eey".to_string())
            ])
        );
    }

    #[test]
    fn parse_t_5() {
        assert!(parse("()").is_err());
    }

    #[test]
    fn parse_t_6() {
        assert!(parse("(constant)").is_err());
    }

    #[test]
    fn parse_t_7() {
        assert_eq!(
            parse("(x.y)").unwrap(),
            Lambda {
                arg: "x".to_string(),
                body: Box::new(Const("y".to_string()))
            }
        );
    }

    #[test]
    fn parse_t_8() {
        assert!(parse("x.()").is_err());
    }

    #[test]
    fn parse_t_9() {
        assert!(parse("x.y").is_err());
    }

    #[test]
    fn parse_t_10() {
        assert_eq!(
            parse("{x y z}").unwrap(),
            Q(vec![
                Const("z".to_string()),
                Const("y".to_string()),
                Const("x".to_string())
            ])
        );
    }

    #[test]
    fn parse_t_11() {
        assert_eq!(
            parse("(hd {x y z})").unwrap(),
            Appl(vec![
                Exp::Builtin(Builtin::Hd),
                Q(vec![
                    Const("z".to_string()),
                    Const("y".to_string()),
                    Const("x".to_string())
                ])
            ])
        );
    }

    #[test]
    fn eval_t_1() {
        assert_eq!(eval(&Const("some".to_string())), Const("some".to_string()));
    }

    #[test]
    fn eval_t_2() {
        assert_eq!(
            eval(&Lambda {
                arg: "x".to_string(),
                body: Box::new(Const("y".to_string()))
            }),
            Lambda {
                arg: "x".to_string(),
                body: Box::new(Const("y".to_string()))
            }
        );
    }

    #[test]
    fn eval_t_3() {
        assert_eq!(
            eval(&Appl(vec![
                Lambda {
                    arg: "x".to_string(),
                    body: Box::new(Const("y".to_string()))
                },
                Const("z".to_string())
            ])),
            Const("y".to_string())
        );
    }

    #[test]
    fn eval_t_4() {
        assert_eq!(
            eval(&Appl(vec![
                Lambda {
                    arg: "x".to_string(),
                    body: Box::new(Const("x".to_string()))
                },
                Const("z".to_string())
            ])),
            Const("z".to_string())
        );
    }

    #[test]
    fn eval_t_5() {
        assert_eq!(
            eval(&Appl(vec![
                Lambda {
                    arg: "x".to_string(),
                    body: Box::new(Lambda {
                        arg: "x".to_string(),
                        body: Box::new(Const("y".to_string()))
                    })
                },
                Const("x".to_string())
            ])),
            Lambda {
                arg: "x".to_string(),
                body: Box::new(Const("y".to_string()))
            }
        );
    }

    #[test]
    fn eval_t_6() {
        assert_eq!(
            eval(&Appl(vec![
                Lambda {
                    arg: "y".to_string(),
                    body: Box::new(Lambda {
                        arg: "x".to_string(),
                        body: Box::new(Const("y".to_string()))
                    })
                },
                Const("z".to_string())
            ])),
            Lambda {
                arg: "x".to_string(),
                body: Box::new(Const("z".to_string()))
            }
        );
    }

    #[test]
    fn eval_t_7() {
        assert_eq!(
            eval(&Appl(vec![
                Lambda {
                    arg: "x".to_string(),
                    body: Box::new(Lambda {
                        arg: "y".to_string(),
                        body: Box::new(Appl(vec![
                            Const("z".to_string()),
                            Const("y".to_string()),
                            Const("x".to_string()),
                            Const("w".to_string())
                        ]))
                    })
                },
                Const("q".to_string())
            ])),
            Lambda {
                arg: "y".to_string(),
                body: Box::new(Appl(vec![
                    Const("z".to_string()),
                    Const("y".to_string()),
                    Const("q".to_string()),
                    Const("w".to_string())
                ]))
            }
        );
    }

    #[test]
    fn eval_t_8() {
        assert_eq!(
            eval(&Q(vec![
                Const("i".to_string()),
                Const("x".to_string()),
                Const("z".to_string())
            ])),
            Q(vec![
                Const("i".to_string()),
                Const("x".to_string()),
                Const("z".to_string())
            ])
        );
    }

    #[test]
    fn eval_t_9() {
        assert_eq!(
            eval(&Appl(vec![Exp::Appl(vec![
                Exp::Builtin(Hd),
                Q(vec![
                    Const("z".to_string()),
                    Const("y".to_string()),
                    Const("x".to_string())
                ])
            ])])),
            Q(vec![Const("x".to_string())])
        );
    }

    #[test]
    fn eval_t_10() {
        assert_eq!(
            eval(&Appl(vec![Exp::Appl(vec![
                Exp::Builtin(Tl),
                Q(vec![
                    Const("z".to_string()),
                    Const("y".to_string()),
                    Const("x".to_string())
                ])
            ])])),
            Q(vec![Const("z".to_string()), Const("y".to_string()),])
        );
    }

    #[test]
    fn eval_t_11() {
        assert_eq!(
            eval(&Appl(vec![Exp::Appl(vec![
                Exp::Builtin(Eval),
                Q(vec![
                    Lambda {
                        arg: "x".to_string(),
                        body: Box::new(Const("x".to_string()))
                    },
                    Const("y".to_string())
                ])
            ])])),
            Const("y".to_string())
        );
    }
}
