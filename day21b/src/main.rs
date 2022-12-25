use std::{collections::HashMap, hash::Hash, str::FromStr};

use nom::{
    branch::*, bytes::complete::tag, character::complete::*, combinator::*, sequence::*, IResult,
};

fn parse_num<T: FromStr>(input: &str) -> IResult<&str, T> {
    map_res(recognize(tuple((opt(tag("+-")), digit1))), &str::parse::<T>)(input)
}

#[derive(Debug, Clone)]
enum Value {
    I64(i64),
    Bool(bool),
}

#[derive(Debug, Clone)]
enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
}

impl BinaryOperator {
    fn eval(&self, a: &Value, b: &Value) -> Value {
        match *self {
            BinaryOperator::Add => match (a, b) {
                (Value::I64(l), Value::I64(r)) => Value::I64(*l + *r),
                _ => panic!("Type error"),
            },
            BinaryOperator::Sub => match (a, b) {
                (Value::I64(l), Value::I64(r)) => Value::I64(*l - *r),
                _ => panic!("Type error"),
            },
            BinaryOperator::Mul => match (a, b) {
                (Value::I64(l), Value::I64(r)) => Value::I64(*l * *r),
                _ => panic!("Type error"),
            },
            BinaryOperator::Div => match (a, b) {
                (Value::I64(l), Value::I64(r)) => Value::I64(*l / *r),
                _ => panic!("Type error"),
            },
            BinaryOperator::Eq => match (a, b) {
                (Value::I64(l), Value::I64(r)) => Value::Bool(*l == *r),
                (Value::Bool(l), Value::Bool(r)) => Value::Bool(*l == *r),
                _ => panic!("Type error"),
            },
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone)]
struct Symbol(String);

#[derive(Debug, Clone)]
enum Expression {
    Val(Value),
    Binary(Symbol, BinaryOperator, Symbol),
    Unknown,
}

impl Expression {
    fn eval(&self, symbols: &HashMap<Symbol, Expression>) -> Value {
        match self {
            Self::Val(val) => val.clone(),
            Self::Binary(ref a, op, ref b) => {
                op.eval(&symbols[a].eval(symbols), &symbols[b].eval(symbols))
            }
            Self::Unknown => panic!("Unknown!"),
        }
    }
}

fn parse_value(input: &str) -> IResult<&str, Expression> {
    parse_num::<i64>(input).map(|(i, v)| (i, Expression::Val(Value::I64(v))))
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

fn extract_expr(
    symbol: &Symbol,
    symbols: &HashMap<Symbol, Expression>,
    new_symbols: &mut HashMap<Symbol, Expression>,
) -> Expression {
    match &symbols[symbol] {
        expr @ Expression::Binary(a, op, b) => {
            let left = &extract_expr(a, symbols, new_symbols);
            let right = &extract_expr(b, symbols, new_symbols);

            match (left, right) {
                (Expression::Unknown, Expression::Unknown) => {
                    panic!("Unknown variable on both sides!");
                }
                (Expression::Unknown, _) => {
                    new_symbols.insert(
                        a.clone(),
                        match op {
                            BinaryOperator::Add => {
                                Expression::Binary(symbol.clone(), BinaryOperator::Sub, b.clone())
                            }
                            BinaryOperator::Sub => {
                                Expression::Binary(symbol.clone(), BinaryOperator::Add, b.clone())
                            }
                            BinaryOperator::Mul => {
                                Expression::Binary(symbol.clone(), BinaryOperator::Div, b.clone())
                            }
                            BinaryOperator::Div => {
                                Expression::Binary(symbol.clone(), BinaryOperator::Mul, b.clone())
                            }
                            BinaryOperator::Eq => right.clone(),
                        },
                    );
                    Expression::Unknown
                }
                (_, Expression::Unknown) => {
                    new_symbols.insert(
                        b.clone(),
                        match op {
                            BinaryOperator::Add => {
                                Expression::Binary(symbol.clone(), BinaryOperator::Sub, a.clone())
                            }
                            BinaryOperator::Sub => {
                                Expression::Binary(a.clone(), BinaryOperator::Sub, symbol.clone())
                            }
                            BinaryOperator::Mul => {
                                Expression::Binary(symbol.clone(), BinaryOperator::Div, a.clone())
                            }
                            BinaryOperator::Div => {
                                Expression::Binary(a.clone(), BinaryOperator::Mul, symbol.clone())
                            }
                            BinaryOperator::Eq => left.clone(),
                        },
                    );
                    Expression::Unknown
                }
                (_, _) => {
                    new_symbols.insert(symbol.clone(), expr.clone());

                    expr.clone()
                }
            }
        }
        val @ Expression::Val(_) => {
            new_symbols.insert(symbol.clone(), val.clone());
            val.clone()
        }
        unkn @ Expression::Unknown => unkn.clone(),
    }
}

fn main() {
    let mut symbols = std::io::stdin()
        .lines()
        .map(Result::unwrap)
        .map(|s| parse_line(&s).unwrap().1)
        .collect::<HashMap<_, _>>();

    let root = &Symbol("root".to_owned());
    match symbols.get_mut(root) {
        Some(Expression::Binary(_, ref mut op, _)) => {
            *op = BinaryOperator::Eq;
        }
        _ => panic!("Could not find a valid root node"),
    };

    let mut symbols_orig = symbols.clone();

    let humn_sym = &Symbol("humn".to_owned());
    match symbols.get_mut(humn_sym) {
        Some(a @ Expression::Val(_)) => {
            *a = Expression::Unknown;
        }
        _ => panic!("Could not find a valid human node"),
    }

    let sol = symbols[&Symbol("root".to_owned())].clone();

    if let Expression::Binary(ref a, BinaryOperator::Eq, ref b) = sol {
        let mut new_sym = HashMap::new();
        extract_expr(root, &symbols, &mut new_sym);
        let new_sym = new_sym;

        let sol = new_sym[humn_sym].eval(&new_sym);

        println!("Human: {:?} = {:?}", humn_sym, sol);

        match symbols_orig.get_mut(humn_sym) {
            Some(a @ Expression::Val(_)) => {
                *a = Expression::Val(sol);
            }
            _ => panic!("Could not find a valid human node"),
        }

        println!("Left:  {:?}", symbols_orig[a].eval(&symbols_orig));
        println!("Right: {:?}", symbols_orig[b].eval(&symbols_orig));
        println!("Eq:    {:?}", symbols_orig[root].eval(&symbols_orig));
    }
}
