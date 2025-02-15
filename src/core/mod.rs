use std::collections::VecDeque;

#[derive(Debug, Eq, PartialEq, Clone)]
pub(crate) struct Heap<A> {
    size: usize,
    free: VecDeque<Addr>,
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
            free: (1..usize::MAX).collect(),
            cts: Vec::new(),
        }
    }

    pub fn alloc(&mut self, val: A) -> Option<Addr> {
        if self.free.is_empty() {
            None
        } else {
            self.size += 1;
            let addr = self.free.pop_front().unwrap();
            self.cts.push((addr, val));
            Some(addr)
        }
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
        self.free.push_front(addr);
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
