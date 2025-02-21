#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
}

pub const PRIMITIVES: [(&'static str, Primitive); 5] = [
    ("negate", Primitive::Neg),
    ("+", Primitive::Add),
    ("-", Primitive::Sub),
    ("*", Primitive::Mul),
    ("/", Primitive::Div),
];

pub fn arith(p: &Primitive, a: i64, b: i64) -> i64 {
    match p {
        Primitive::Neg => panic!("not supported"),
        Primitive::Add => a + b,
        Primitive::Sub => a - b,
        Primitive::Mul => a * b,
        Primitive::Div => a / b,
    }
}
