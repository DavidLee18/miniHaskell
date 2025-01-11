pub mod parser;

#[derive(Debug, Clone)]
enum Expr<A> {
    Var(Name),
    Num(i64),
    Constr {
        tag: i64,
        arity: i64,
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
type Alter<A> = (i64, Vec<A>, Expr<A>);
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
fn simple_program() -> CoreProgram {
    vec![
        (
            Name::from("main"),
            vec![],
            Expr::Ap(
                Box::new(Expr::Var(Name::from("double"))),
                Box::new(Expr::Num(21)),
            ),
        ),
        (
            Name::from("double"),
            vec![Name::from("x")],
            Expr::Ap(
                Box::new(Expr::Ap(
                    Box::new(Expr::Var(Name::from("+"))),
                    Box::new(Expr::Var(Name::from("x"))),
                )),
                Box::new(Expr::Var(Name::from("x"))),
            ),
        ),
    ]
}

fn prelude_defs() -> CoreProgram {
    vec![
        (
            Name::from("I"),
            vec![Name::from("x")],
            Expr::Var(Name::from("x")),
        ),
        (
            Name::from("K"),
            vec![Name::from("x"), Name::from("y")],
            Expr::Var(Name::from("x")),
        ),
        (
            Name::from("K1"),
            vec![Name::from("x"), Name::from("y")],
            Expr::Var(Name::from("y")),
        ),
        (
            Name::from("S"),
            vec![Name::from("f"), Name::from("g"), Name::from("x")],
            Expr::Ap(
                Box::new(Expr::Ap(
                    Box::new(Expr::Var(Name::from("f"))),
                    Box::new(Expr::Var(Name::from("x"))),
                )),
                Box::new(Expr::Ap(
                    Box::new(Expr::Var(Name::from("g"))),
                    Box::new(Expr::Var(Name::from("x"))),
                )),
            ),
        ),
        (
            Name::from("compose"),
            vec![Name::from("f"), Name::from("g"), Name::from("x")],
            Expr::Ap(
                Box::new(Expr::Var(Name::from("f"))),
                Box::new(Expr::Ap(
                    Box::new(Expr::Var(Name::from("g"))),
                    Box::new(Expr::Var(Name::from("x"))),
                )),
            ),
        ),
        (
            Name::from("twice"),
            vec![Name::from("f")],
            Expr::Ap(
                Box::new(Expr::Ap(
                    Box::new(Expr::Var(Name::from("compose"))),
                    Box::new(Expr::Var(Name::from("f"))),
                )),
                Box::new(Expr::Var(Name::from("f"))),
            ),
        ),
    ]
}

impl std::fmt::Display for CoreExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CoreExpr::Var(v) => write!(f, "{}", v),
            CoreExpr::Num(n) => write!(f, "{}", n),
            CoreExpr::Constr { .. } => todo!(),
            CoreExpr::Ap(e1, e2) => write!(f, "{} {}", e1, e2),
            CoreExpr::Let { .. } => todo!(),
            CoreExpr::Case(_, _) => todo!(),
            CoreExpr::Lam(_, _) => todo!(),
        }
    }
}

type Token = (u32, String);

fn clex(input: String) -> Vec<Token> {
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
                    },
                    _ => tokens.push((line, String::from('|'))),
                }
            }
            c__ if TWO_CHAR_OPS.iter().find(|&&s| s.starts_with(c__)).is_some() => {
                match TWO_CHAR_OPS.iter().find(|&&s| s.starts_with(c__)) {
                    Some(&"==") => tokens.push((line, String::from("=="))),
                    Some(&"~=") => tokens.push((line, String::from("~="))),
                    Some(&">=") => tokens.push((line, String::from(">="))),
                    Some(&"<=") => tokens.push((line, String::from("<="))),
                    Some(&"->") => tokens.push((line, String::from("->"))),
                    _ => unreachable!(),
                }
            }
            c__ if c__.is_whitespace() => {
                if c__ == '\n' { line += 1; }
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

fn syntax(tokens: Vec<Token>) -> CoreProgram {
    todo!()
}
