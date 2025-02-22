use crate::compiler::Node;

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Neg,
    Add,
    Sub,
    Mul,
    Div,
    Constr(u32, u32),
    If,
    Greater,
    GreaterEq,
    Less,
    LessEq,
    Eq,
    NotEq,
}

impl Primitive {
    pub fn arith(&self, a: i64, b: i64) -> i64 {
        match self {
            Primitive::Add => a + b,
            Primitive::Sub => a - b,
            Primitive::Mul => a * b,
            Primitive::Div => a / b,
            _ => panic!("not supported"),
        }
    }

    pub fn cmp(&self, a: i64, b: i64) -> Node {
        match self {
            Primitive::Greater => Node::bool(a > b),
            Primitive::GreaterEq => Node::bool(a >= b),
            Primitive::Less => Node::bool(a < b),
            Primitive::LessEq => Node::bool(a <= b),
            Primitive::Eq => Node::bool(a == b),
            Primitive::NotEq => Node::bool(a != b),
            _ => panic!("not supported"),
        }
    }

    pub fn eq_data(&self, a: &Node, b: &Node) -> Node {
        let (Node::Data(ta, aas), Node::Data(tb, abs)) = (a, b) else {
            panic!("not a data");
        };
        match self {
            Primitive::Eq => Node::bool(ta == tb && aas == abs),
            Primitive::NotEq => Node::bool(ta != tb || aas != abs),
            _ => panic!("not supported"),
        }
    }
}

pub const PRIMITIVES: [(&'static str, Primitive); 12] = [
    ("negate", Primitive::Neg),
    ("+", Primitive::Add),
    ("-", Primitive::Sub),
    ("*", Primitive::Mul),
    ("/", Primitive::Div),
    ("if", Primitive::If),
    (">", Primitive::Greater),
    (">=", Primitive::GreaterEq),
    ("<", Primitive::Less),
    ("<=", Primitive::LessEq),
    ("==", Primitive::Eq),
    ("~=", Primitive::NotEq),
];
