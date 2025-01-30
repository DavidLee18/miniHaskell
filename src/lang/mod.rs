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
        is_rec: bool,
        defs: Vec<(A, Expr<A>)>,
        body: Box<Expr<A>>,
    },
    Case(Box<Expr<A>>, Vec<Alter<A>>),
    Lam(Vec<A>, Box<Expr<A>>),
}

type Name = String;
type CoreExpr = Expr<Name>;
type Alter<A> = (u32, Vec<A>, Expr<A>);
type CoreAlt = Alter<Name>;

type Program<A> = Vec<ScDefn<A>>;
type CoreProgram = Program<Name>;

type ScDefn<A> = (Name, Vec<A>, Expr<A>);
type CoreScDefn = ScDefn<Name>;

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
            c__ if c__.is_whitespace() => {
                if c__ == '\n' {
                    line += 1;
                }
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

pub fn syntax(tokens: Vec<Token>) -> CoreProgram {
    take_first_parse(parser::program()(tokens))
}

fn take_first_parse(programs: Vec<(CoreProgram, Vec<Token>)>) -> CoreProgram {
    programs
        .into_iter()
        .find(|(_, v)| v.is_empty())
        .map(|(p, _)| p)
        .expect("Syntax error")
}

#[derive(Debug, Clone)]
enum PartialExpr {
    NoOp,
    FoundOp(Name, CoreExpr),
}
