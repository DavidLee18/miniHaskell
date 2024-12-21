#[derive(Debug)]
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
const SIMPLE_PROGRAM: CoreProgram = vec![
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
];

const PRELUDE_DEFS: CoreProgram = vec![
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
];

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