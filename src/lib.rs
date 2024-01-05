// TODO: Q-exr
// TODO: hd and tl for Q-exr
// TODO: eval keywoard

use nom::{
    branch::alt,
    character::complete::{alpha1, char},
    error::{self, ErrorKind},
    Finish, Parser,
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Exp {
    // Vec with >= 2 len
    Appl(Vec<Exp>),
    Const(String),
    Lambda { arg: String, body: Box<Exp> },
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
        alpha1.map(|x: &str| Exp::Const(x.to_string())),
        nom::sequence::delimited(
            char('('),
            alt((
                nom::sequence::tuple((alpha1, char('.'), exp)).map(|x| Exp::Lambda {
                    arg: x.0.to_string(),
                    body: Box::new(x.2),
                }),
                nom::multi::separated_list1(nom::character::complete::multispace1, exp)
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
    ))(input)
}

#[must_use]
pub fn eval(exp: &Exp) -> Exp {
    match exp {
        Exp::Appl(x) => {
            let mut acc = x[0].clone();
            let mut it = x.iter();
            it.next();
            while let Exp::Lambda { arg, mut body } = acc {
                if let Some(y) = it.next() {
                    substitute(&mut body, &arg, y);
                    acc = *body;
                } else {
                    acc = Exp::Lambda { arg, body };
                    break;
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
        x @ Exp::Const(_) => x.clone(),
        Exp::Lambda { arg, body } => Exp::Lambda {
            arg: arg.clone(),
            body: Box::new(eval(body)),
        },
    }
}

fn substitute(exp: &mut Exp, from: &str, to: &Exp) {
    match exp {
        Exp::Appl(x) => {
            for y in x {
                substitute(y, from, to);
            }
        }
        Exp::Const(x) => {
            if x == from {
                *exp = to.clone();
            }
        }
        Exp::Lambda { arg, body } => {
            if arg != from {
                substitute(body, from, to);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Exp::{Appl, Const, Lambda};

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
            Exp::Lambda {
                arg: "x".to_string(),
                body: Box::new(Exp::Const("y".to_string()))
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
                        body: Box::new(Exp::Const("y".to_string()))
                    })
                },
                Const("x".to_string())
            ])),
            Lambda {
                arg: "x".to_string(),
                body: Box::new(Exp::Const("y".to_string()))
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
                        body: Box::new(Exp::Const("y".to_string()))
                    })
                },
                Const("z".to_string())
            ])),
            Lambda {
                arg: "x".to_string(),
                body: Box::new(Exp::Const("z".to_string()))
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
                        body: Box::new(Exp::Appl(vec![
                            Exp::Const("z".to_string()),
                            Exp::Const("y".to_string()),
                            Exp::Const("x".to_string()),
                            Exp::Const("w".to_string())
                        ]))
                    })
                },
                Const("q".to_string())
            ])),
            Lambda {
                arg: "y".to_string(),
                body: Box::new(Exp::Appl(vec![
                    Exp::Const("z".to_string()),
                    Exp::Const("y".to_string()),
                    Exp::Const("q".to_string()),
                    Exp::Const("w".to_string())
                ]))
            }
        );
    }
}
