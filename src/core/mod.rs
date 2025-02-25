use crate::compiler::primitives::Primitive;
use crate::compiler::Node;
use crate::lang::{CoreExpr, Name};
use std::collections::VecDeque;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct Heap<A> {
    size: usize,
    cts: Vec<(Addr, A)>,
    alloc_count: usize,
}
pub(crate) type Addr = usize;
pub(crate) type ASSOC<A, B> = VecDeque<(A, B)>;

pub(crate) fn map_accuml<A, B, C, F: Fn(&mut A, B) -> C>(
    f: F,
    acc: &mut A,
    inputs: Vec<B>,
) -> VecDeque<C> {
    if inputs.is_empty() {
        VecDeque::new()
    } else {
        let mut res = VecDeque::new();
        for input in inputs {
            res.push_back(f(acc, input));
        }
        res
    }
}

impl<A> Heap<A> {
    pub fn new() -> Self {
        Self {
            size: 0,
            cts: Vec::new(),
            alloc_count: 0,
        }
    }

    fn get_addr(&self) -> Addr {
        let mut i = 1;
        let mut found = self.cts.iter().find(|(a, _)| *a == i);
        while let Some(_) = found {
            i += 1;
            found = self.cts.iter().find(|(a, _)| *a == i);
        }
        i
    }

    pub fn alloc(&mut self, val: A) -> Addr {
        let addr = self.get_addr();
        self.size += 1;
        self.cts.push((addr, val));
        self.alloc_count += 1;
        addr
    }

    pub fn alloc_count(&self) -> usize {
        self.alloc_count
    }

    pub fn update(&mut self, addr: Addr, val: A) -> Result<(), HeapError> {
        let v = self.cts.iter_mut().find(|(a, _)| *a == addr);
        match v {
            None => Err(HeapError::NotFound),
            Some(v) => {
                v.1 = val;
                Ok(())
            }
        }
    }

    pub fn free(&mut self, addr: Addr) {
        self.cts.retain(|(a, _)| *a != addr);
        self.size -= 1;
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn addresses(&self) -> Vec<Addr> {
        self.cts.iter().map(|&(a, _)| a).collect()
    }

    pub fn lookup(&self, addr: Addr) -> Result<&A, HeapError> {
        self.cts
            .iter()
            .find(|(a, _)| *a == addr)
            .map(|(_, v)| v)
            .ok_or(HeapError::NotFound)
    }

    pub fn iter(&self) -> impl Iterator<Item = &(usize, A)> {
        self.cts.iter()
    }
}

impl Heap<Node> {
    pub fn get_args(&self, stack: &Vec<Addr>, len: usize) -> Result<Vec<Addr>, HeapError> {
        let mut res = vec![];
        let mut i = 0;
        for addr in stack.iter().rev().skip(1) {
            match self.lookup(*addr) {
                Ok(Node::Ap(_, arg)) => {
                    i += 1;
                    res.push(*arg);
                }
                _ => return Err(HeapError::NotAp),
            }
        }
        if i < len {
            return Err(HeapError::ArgsLengthMismatch {
                expected: len,
                actual: i,
            });
        }
        Ok(res)
    }

    pub fn instantiate(
        &mut self,
        expr: CoreExpr,
        env: &mut ASSOC<Name, Addr>,
    ) -> Result<Addr, HeapError> {
        // println!("env: {:?}", env);
        match expr {
            CoreExpr::Var(v) => env
                .iter()
                .find(|(n, _)| *n == v)
                .map(|(_, addr)| *addr)
                .ok_or(HeapError::UndefinedName(v)),
            CoreExpr::Num(n) => Ok(self.alloc(Node::Num(n))),
            CoreExpr::Constr { tag, arity } => Ok(self.alloc(Node::Prim(
                String::from("Pack"),
                Primitive::Constr(tag, arity),
            ))),
            CoreExpr::Ap(e1, e2) => {
                let a1 = self.instantiate(*e1, env)?;
                let a2 = self.instantiate(*e2, env)?;
                Ok(self.alloc(Node::Ap(a1, a2)))
            }
            CoreExpr::Let { defs, body } => {
                for (name, expr) in defs {
                    let addr = self.alloc(Node::SuperComb(name.clone(), vec![], expr));
                    env.push_front((name, addr));
                }
                self.instantiate(*body, env)
            }
            CoreExpr::Case(_, _) => Err(HeapError::NotInstantiable),
            CoreExpr::Lam(_, _) => Err(HeapError::NotInstantiable),
        }
    }

    pub fn instantiate_and_update(
        &mut self,
        body: CoreExpr,
        root_addr: Addr,
        env: &mut ASSOC<Name, Addr>,
    ) -> Result<(), HeapError> {
        // println!("env: {:?}", env);
        match body {
            CoreExpr::Var(a) => {
                let a_addr = env
                    .iter()
                    .find(|(n, _)| *n == a)
                    .map(|(_, addr)| *addr)
                    .ok_or(HeapError::UndefinedName(a))?;
                self.update(root_addr, Node::Ind(a_addr))
            }
            CoreExpr::Num(n) => self.update(root_addr, Node::Num(n)),
            CoreExpr::Constr { tag, arity } => self.update(
                root_addr,
                Node::Prim(String::from("Pack"), Primitive::Constr(tag, arity)),
            ),
            CoreExpr::Ap(e1, e2) => {
                let a1 = self.instantiate(*e1, env)?;
                let a2 = self.instantiate(*e2, env)?;
                self.update(root_addr, Node::Ap(a1, a2))
            }
            CoreExpr::Let { defs, body } => {
                for (name, expr) in defs {
                    let addr = self.alloc(Node::SuperComb(name.clone(), vec![], expr));
                    env.push_front((name, addr));
                }
                self.instantiate_and_update(*body, root_addr, env)
            }
            CoreExpr::Case(_, _) => Err(HeapError::NotInstantiable),
            CoreExpr::Lam(_, _) => Err(HeapError::NotInstantiable),
        }
    }

    pub fn node_ref_eq(&self, a: &Node, b: &Node) -> Result<bool, HeapError> {
        let mut a = a;
        let mut b = b;
        while let Node::Ind(a_) = a {
            a = self.lookup(*a_)?;
        }
        while let Node::Ind(b_) = b {
            b = self.lookup(*b_)?;
        }
        Ok(a == b)
    }
}

#[derive(Debug)]
pub enum HeapError {
    NotFound,
    NotAp,
    ArgsLengthMismatch { expected: usize, actual: usize },
    NotInstantiable,
    UndefinedName(String),
}

impl Display for HeapError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapError::NotFound => write!(f, "item not found on the heap"),
            HeapError::NotAp => write!(f, "not an Ap node"),
            HeapError::ArgsLengthMismatch { expected, actual } => {
                write!(f, "expected {} arguments, got {}", expected, actual)
            }
            HeapError::NotInstantiable => write!(f, "the expression is not instantiable"),
            HeapError::UndefinedName(name) => write!(f, "undefined name '{}'", name),
        }
    }
}

impl Error for HeapError {}
