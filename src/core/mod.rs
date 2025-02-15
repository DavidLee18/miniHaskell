use crate::compiler::Node;
use crate::lang::{CoreExpr, Name};

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct Heap<A> {
    size: usize,
    cts: Vec<(Addr, A)>,
}
pub(crate) type Addr = usize;
pub(crate) type ASSOC<A, B> = Vec<(A, B)>;

pub(crate) fn map_accuml<A, B, C, F: Fn(&mut A, B) -> C>(
    f: F,
    acc: &mut A,
    inputs: Vec<B>,
) -> Vec<C> {
    if inputs.is_empty() {
        vec![]
    } else {
        let mut res = vec![];
        for input in inputs {
            res.push(f(acc, input));
        }
        res
    }
}

impl<A> Heap<A> {
    pub fn new() -> Self {
        Self {
            size: 0,
            cts: Vec::new(),
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

    pub fn alloc(&mut self, val: A) -> Option<Addr> {
        let addr = self.get_addr();
        self.size += 1;
        self.cts.push((addr, val));
        Some(addr)
    }

    pub fn update(&mut self, addr: Addr, val: A) {
        let v = self.cts.iter_mut().find(|(a, _)| *a == addr);
        match v {
            None => {}
            Some(v) => {
                v.1 = val;
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

    pub fn lookup(&self, addr: Addr) -> Option<&A> {
        self.cts.iter().find(|(a, _)| *a == addr).map(|(_, v)| v)
    }
}

impl Heap<Node> {
    pub fn get_args(&mut self, stack: &Vec<Addr>) -> Vec<Addr> {
        let mut res = vec![];
        for addr in stack.iter().rev() {
            match self.lookup(*addr) {
                Some(Node::Ap(fun, arg)) => res.push(*arg),
                _ => continue,
            }
        }
        res
    }

    pub fn instantiate(&mut self, expr: CoreExpr, env: &ASSOC<Name, Addr>) -> Addr {
        match expr {
            CoreExpr::Var(v) => env
                .iter()
                .find(|(n, _)| *n == v)
                .map(|(_, addr)| *addr)
                .expect(&format!("undefined name {}", v)),
            CoreExpr::Num(n) => self.alloc(Node::Num(n)).expect("heap alloc failed"),
            CoreExpr::Constr { .. } => panic!("unable to instantiate constr yet"),
            CoreExpr::Ap(e1, e2) => {
                let a1 = self.instantiate(*e1, env);
                let a2 = self.instantiate(*e2, env);
                self.alloc(Node::Ap(a1, a2))
                    .expect("heap instantiate failed")
            }
            CoreExpr::Let { .. } => panic!("unable to instantiate let yet"),
            CoreExpr::Case(_, _) => panic!("unable to instantiate case"),
            CoreExpr::Lam(_, _) => panic!("unable to instantiate lam"),
        }
    }
}
