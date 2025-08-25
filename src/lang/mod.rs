use crate::lang::parser::Parser;
use std::fs::read_to_string;
use std::path::PathBuf;

pub(crate) mod combinators;
pub(crate) mod parser;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<A> {
    Var(Name),
    Num(i64),
    Constr {
        tag: u32,
        arity: u32,
    },
    Ap(Box<Expr<A>>, Box<Expr<A>>),
    Let {
        defs: Vec<(A, Expr<A>)>,
        body: Box<Expr<A>>,
    },
    Case(Box<Expr<A>>, Vec<Alter<A>>),
    Lam(Vec<A>, Box<Expr<A>>),
}

impl<A> Expr<A> {
    pub fn is_let(&self) -> bool {
        match self {
            Expr::Let { .. } => true,
            Expr::Ap(a, b) => a.is_let() || b.is_let(),
            _ => false,
        }
    }

    pub fn get_var_mut(&mut self, var_name: &str) -> Option<&mut Expr<A>> {
        match self {
            Expr::Var(n) if n == var_name => Some(self),
            Expr::Var(_) => None,
            Expr::Ap(left, right) => left
                .get_var_mut(var_name)
                .or_else(|| right.get_var_mut(var_name)),
            _ => None,
        }
    }
}

pub(crate) type Name = String;
pub(crate) type CoreExpr = Expr<Name>;
type Alter<A> = (u32, Vec<A>, Expr<A>);
type CoreAlt = Alter<Name>;

type Program<A> = Vec<ScDefn<A>>;
pub(crate) type CoreProgram = Program<Name>;

type ScDefn<A> = (Name, Vec<A>, Expr<A>);
pub(crate) type CoreScDefn = ScDefn<Name>;

/// a simple program in core lang.
/// ```
/// main = double 21 ;
/// double x = x + x
/// ```
pub const SIMPLE_PROGRAM: &'static str = "main = double 21; double x = x + x";

pub const PRELUDE_DEFS: &'static str = "I x = x; K x y = x; K1 x y = y; S f g x = f x (g x); compose f g x = f (g x); twice f = compose f f";

type Token = (u32, String);

pub(crate) fn clex(input: String) -> Vec<Token> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut chars = input.chars();
    let mut c = chars.next();
    let mut line = 0;
    let mut temp;
    while let Some(c_) = c {
        match c_ {
            '|' => {
                let c_next = chars.next();
                match c_next {
                    Some('|') => {
                        while chars.next() != Some('\n') {}
                        line += 1;
                    }
                    _ => tokens.push((line, String::from('|'))),
                }
            }
            c__ if TWO_CHAR_OPS.iter().find(|&&s| s.starts_with(c__)).is_some() => {
                let c_next = chars.next();
                match (c__, c_next) {
                    ('=', Some('=')) => tokens.push((line, String::from("=="))),
                    ('~', Some('=')) => tokens.push((line, String::from("~="))),
                    ('>', Some('=')) => tokens.push((line, String::from(">="))),
                    ('<', Some('=')) => tokens.push((line, String::from("<="))),
                    ('-', Some('>')) => tokens.push((line, String::from("->"))),
                    _ => {
                        tokens.push((line, String::from(c__)));
                        temp = chars.collect::<String>();
                        if let Some(c_next) = c_next {
                            temp.insert(0, c_next);
                        }
                        chars = temp.chars();
                    }
                }
            }
            c__ if c__.is_whitespace() && c__ == '\n' => {
                line += 1;
            }
            c__ if c__.is_digit(10) => {
                let mut rest = chars
                    .clone()
                    .take_while(|c| c.is_digit(10))
                    .collect::<String>();
                rest.insert(0, c__);
                tokens.push((line, rest));
                temp = chars.skip_while(|c| c.is_digit(10)).collect::<String>();
                chars = temp.chars();
            }
            c__ if c__.is_alphabetic() => {
                let mut rest = chars
                    .clone()
                    .take_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect::<String>();
                rest.insert(0, c__);
                tokens.push((line, rest));
                temp = chars
                    .skip_while(|c| c.is_alphanumeric() || *c == '_')
                    .collect::<String>();
                chars = temp.chars();
            }
            c__ => tokens.push((line, String::from(c__))),
        }
        c = chars.next();
    }
    tokens
}

const TWO_CHAR_OPS: [&'static str; 5] = ["==", "~=", ">=", "<=", "->"];

pub(crate) fn parse_with<A>(parser: Parser<A>, tokens: Vec<Token>) -> Result<Vec<A>, SyntaxError> {
    Ok(parser(tokens)
        .into_iter()
        .filter(|(_, v)| v.is_empty())
        .map(|(p, _)| p)
        .collect())
}

pub(crate) fn syntax(tokens: Vec<Token>) -> Result<Vec<CoreProgram>, SyntaxError> {
    parse_with(parser::program(), tokens)
}

#[derive(Debug, Clone)]
enum PartialExpr {
    NoOp,
    FoundOp(Name, CoreExpr),
}

pub fn parse(p: PathBuf) -> Result<Vec<CoreProgram>, SyntaxError> {
    let s = read_to_string(p).expect("Failed to read file");
    syntax(clex(s))
}

pub fn parse_raw(s: String) -> Result<Vec<CoreProgram>, SyntaxError> {
    let tokens = clex(s);
    syntax(tokens)
}

#[derive(Debug)]
pub struct SyntaxError(pub Token);

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "Syntax error")
    }
}

impl std::error::Error for SyntaxError {}
