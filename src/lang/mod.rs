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
type Alter<A> = (i64, Vec<A>, Box<Expr<A>>);
type CoreAlt = Alter<Name>;

type Program<A> = Vec<ScDefn<A>>;
type CoreProgram = Program<Name>;

type ScDefn<A> = (Name, Vec<A>, Box<Expr<A>>);
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
        Box::new(Expr::Ap(
            Box::new(Expr::Var(Name::from("double"))),
            Box::new(Expr::Num(21)),
        )),
    ),
    (
        Name::from("double"),
        vec![Name::from("x")],
        Box::new(Expr::Ap(
            Box::new(Expr::Ap(
                Box::new(Expr::Var(Name::from("+"))),
                Box::new(Expr::Var(Name::from("x"))),
            )),
            Box::new(Expr::Var(Name::from("x"))),
        )),
    ),
];

const PRELUDE_DEFS: CoreProgram = vec![(
    Name::from("I"),
    vec![Name::from("x")],
    Box::new(Expr::Var(Name::from("x"))),
)];
