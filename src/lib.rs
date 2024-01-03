use nom::{
    character::complete::{char, multispace1},
    error::{self, ErrorKind},
    Finish, Parser,
};

#[derive(Debug, PartialEq, Eq)]
pub enum Exp {
    Const(String),
    // Vec with >= 2 len
    Appl(Vec<Exp>),
}

pub fn parsed(input: &str) -> Result<Exp, error::Error<&str>> {
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
    nom::branch::alt((
        nom::character::complete::alpha1.map(|x: &str| Exp::Const(x.to_string())),
        nom::sequence::delimited(
            char('('),
            nom::multi::separated_list1(multispace1, exp),
            char(')'),
        )
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
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use Exp::{Appl, Const};

    #[test]
    fn parsed_t_1() {
        assert_eq!(
            parsed("(hey way)").unwrap(),
            Appl(vec![Const("hey".to_string()), Const("way".to_string())])
        );
    }

    #[test]
    fn parsed_t_2() {
        assert!(parsed("hey way").is_err());
    }

    #[test]
    fn parsed_t_3() {
        assert_eq!(parsed("good").unwrap(), Const("good".to_string()));
    }

    #[test]
    fn parsed_t_4() {
        assert_eq!(
            parsed("((hey way) eey)").unwrap(),
            Appl(vec![
                Appl(vec![Const("hey".to_string()), Const("way".to_string())]),
                Const("eey".to_string())
            ])
        );
    }

    #[test]
    fn parsed_t_5() {
        assert!(parsed("()").is_err());
    }

    #[test]
    fn parsed_t_6() {
        assert!(parsed("(constant)").is_err());
    }
}
