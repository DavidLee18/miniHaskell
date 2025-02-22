#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Constr(u32, u32),
    If,
}

pub const PRIMITIVES: [(&'static str, Primitive); 6] = [
    ("negate", Primitive::Neg),
    ("+", Primitive::Add),
    ("-", Primitive::Sub),
    ("*", Primitive::Mul),
    ("/", Primitive::Div),
    ("if", Primitive::If),
];

pub fn arith(p: &Primitive, a: i64, b: i64) -> i64 {
    match p {
        Primitive::Add => a + b,
        Primitive::Sub => a - b,
        Primitive::Mul => a * b,
        Primitive::Div => a / b,
        _ => panic!("not supported"),
    }
}
