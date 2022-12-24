use std::{collections::HashMap, str::FromStr};

use nom::{
    branch::*, bytes::complete::tag, character::complete::*, combinator::*, sequence::*, IResult,
};

fn parse_num<T: FromStr>(input: &str) -> IResult<&str, T> {
    map_res(recognize(tuple((opt(tag("+-")), digit1))), &str::parse::<T>)(input)
}

type Value = i64;

#[derive(Debug)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinaryOperator {
    fn eval(&self, a: &Value, b: &Value) -> Value {
        match *self {
            BinaryOperator::Add => a + b,
            BinaryOperator::Sub => a - b,
            BinaryOperator::Mul => a * b,
            BinaryOperator::Div => a / b,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq)]
struct Symbol(String);

#[derive(Debug)]
enum Expression {
    Val(Value),
    Binary(Symbol, BinaryOperator, Symbol),
}

impl Expression {
    fn eval(&self, symbols: &HashMap<Symbol, Expression>) -> Value {
        match self {
            Self::Val(val) => val.clone(),
            Self::Binary(ref a, op, ref b) => {
                op.eval(&symbols[a].eval(symbols), &symbols[b].eval(symbols))
            }
        }
    }
}

fn parse_value(input: &str) -> IResult<&str, Expression> {
    parse_num::<i64>(input).map(|(i, v)| (i, Expression::Val(v)))
}

fn parse_operator(input: &str) -> IResult<&str, BinaryOperator> {
    let (input, op) = alt((tag("+"), tag("-"), tag("*"), tag("/")))(input)?;

    let operator = match op {
        "+" => BinaryOperator::Add,
        "-" => BinaryOperator::Sub,
        "*" => BinaryOperator::Mul,
        "/" => BinaryOperator::Div,
        _ => unreachable!(),
    };

    Ok((input, operator))
}

fn parse_binary_expr(input: &str) -> IResult<&str, Expression> {
    let (input, a) = alpha1(input)?;
    let (input, _) = space0(input)?;
    let (input, op) = parse_operator(input)?;
    let (input, _) = space0(input)?;
    let (input, b) = alpha1(input)?;

    Ok((
        input,
        Expression::Binary(Symbol(a.to_owned()), op, Symbol(b.to_owned())),
    ))
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((parse_value, parse_binary_expr))(input)
}

fn parse_line(input: &str) -> IResult<&str, (Symbol, Expression)> {
    let (input, id) = alpha1(input)?;
    let (input, _) = tag(":")(input)?;
    let (input, _) = space1(input)?;
    let (input, expr) = parse_expression(input)?;

    Ok((input, (Symbol(id.to_owned()), expr)))
}

fn main() {
    let symbols = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_line(&s).unwrap().1)
        .collect::<HashMap<_, _>>();

    let sol = symbols[&Symbol("root".to_owned())].eval(&symbols);

    println!("{sol}");
}
