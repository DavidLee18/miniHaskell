use crate::compiler::primitives::Primitive;
use crate::core::{map_accuml, Addr, Heap, HeapError, ASSOC};
use crate::lang;
use crate::lang::SyntaxError;
use std::cmp::max;

pub mod primitives;

type TiState = (TiOutput, TiStack, TiDump, TiHeap, TiGlobals, TiStats);

type TiStack = Vec<Addr>;
type TiDump = Vec<TiStack>;

type TiHeap = Heap<Node>;

type TiOutput = Vec<i64>;

#[derive(Clone, Debug, PartialEq)]
pub enum Node {
    Ap(Addr, Addr),
    SuperComb(lang::Name, Vec<lang::Name>, lang::CoreExpr),
    Num(i64),
    Ind(Addr),
    Prim(lang::Name, Primitive),
    Data(u32, Vec<Addr>),
}

impl Node {
    pub fn bool(val: bool) -> Self {
        match val {
            true => Self::Data(2, vec![]),
            false => Self::Data(1, vec![]),
        }
    }
    pub fn is_data_node(&self) -> bool {
        match self {
            Self::Num(_) => true,
            Self::Data(_, _) => true,
            Self::Prim(_, Primitive::Stop) => true,
            _ => false,
        }
    }
}

type TiGlobals = ASSOC<lang::Name, Addr>;

#[derive(Debug, Default, Clone)]
pub(crate) struct TiStats {
    reductions: usize,
    max_stack_size: usize,
    heap_alloc_count: usize,
}

#[derive(Debug)]
pub enum CompileError {
    NoMain,
    Syntax(SyntaxError),
}

#[derive(Debug)]
pub enum EvalError {
    EmptyStack,
    Heap(HeapError),
    EmptyDump,
    NumAp,
    ArgsLengthMismatch { expected: usize, actual: usize },
    TypeMismatch,
    DataAp,
    AbortSig,
}

pub(crate) fn compile(p: lang::CoreProgram) -> Result<TiState, CompileError> {
    let sc_defs = vec![
        p,
        lang::parse_raw(String::from(lang::PRELUDE_DEFS)).map_err(CompileError::Syntax)?[0].clone(),
        lang::parse_raw(String::from(EXTRA_PRELUDE_DEFS)).map_err(CompileError::Syntax)?[0].clone(),
    ]
        .into_iter()
        .flatten()
        .collect();
    let (init_heap, globals) = build_init_heap(sc_defs);
    let main_addr = lookup(&globals, &String::from("main")).ok_or(CompileError::NoMain)?;
    let alloc_count = init_heap.alloc_count();
    Ok((
        vec![],
        vec![*main_addr],
        vec![],
        init_heap,
        globals,
        TiStats {
            heap_alloc_count: alloc_count,
            max_stack_size: 1,
            ..TiStats::default()
        },
    ))
}

fn lookup<'a, A: PartialEq, B>(a: &'a ASSOC<A, B>, k: &A) -> Option<&'a B> {
    a.iter().find(|&(a, _)| *a == *k).map(|(_, b)| b)
}

const EXTRA_PRELUDE_DEFS: &'static str = r#"
    False = Pack{1, 0};
    True = Pack{2, 0};
    and x y = if x y False;
    or x y = if x True y;
    xor x y = and (or x y) (not (and x y));
    not x = if x False True;
    Pair = Pack{1, 2};
    fst p = casePair p K;
    snd p = casePair p K1;
    Nil = Pack{1, 0};
    Cons = Pack{2, 2};
    head l = caseList l abort K;
    tail l = caseList l abort K1;
    printList xs = caseList xs stop printList_;
    printList_ x xs = print x (printList xs);
    "#;

fn build_init_heap(sc_defs: Vec<lang::CoreScDefn>) -> (TiHeap, TiGlobals) {
    let mut init_heap = Heap::new();
    let mut sc_addrs = map_accuml(allocate_sc, &mut init_heap, sc_defs);
    let mut prim_addrs = map_accuml(
        allocate_prim,
        &mut init_heap,
        Vec::from(primitives::PRIMITIVES),
    );
    sc_addrs.append(&mut prim_addrs);
    (init_heap, sc_addrs)
}

fn allocate_prim(heap: &mut TiHeap, primitive: (&'static str, Primitive)) -> (lang::Name, Addr) {
    let (name, prim) = primitive;
    let addr = heap.alloc(Node::Prim(lang::Name::from(name), prim));
    (lang::Name::from(name), addr)
}

fn allocate_sc(heap: &mut TiHeap, sc_defs: lang::CoreScDefn) -> (lang::Name, Addr) {
    let (name, args, body) = sc_defs;
    let addr = heap.alloc(Node::SuperComb(name.clone(), args, body));
    (name, addr)
}

pub(crate) fn eval(state: TiState) -> Result<Vec<TiState>, EvalError> {
    let mut res = vec![];
    let mut temp = state;
    res.push(temp.clone());
    step(&mut temp)?;
    do_admin(&mut temp);
    while !ti_final(&temp)? {
        res.push(temp.clone());
        step(&mut temp)?;
        do_admin(&mut temp);
    }
    res.push(temp);
    Ok(res)
}

fn step(state: &mut TiState) -> Result<(), EvalError> {
    let (_, stack, dump, heap, _, _) = state;
    // println!("Stack: {:?}", stack);
    // println!("Dump: {:?}", dump);
    // println!("{:?}", heap);

    let last_stack = *stack.last().ok_or(EvalError::EmptyStack)?;

    match heap.lookup(last_stack).expect("cannot be found on heap") {
        Node::Ap(a1, a2) => {
            let a1 = *a1;
            if let Ok(Node::Ind(a3)) = heap.lookup(*a2) {
                heap.update(last_stack, Node::Ap(a1, *a3))
                    .map_err(EvalError::Heap)?;
            }
            stack.push(a1);
            Ok(())
        }
        Node::SuperComb(_, args, body) => {
            let (args, body) = (args.clone(), body.clone());
            sc_step(state, last_stack, args, body)
        }
        Node::Num(_) => {
            if stack.len() != 1 {
                Err(EvalError::NumAp)
            } else {
                *stack = dump.pop().ok_or(EvalError::EmptyDump)?;
                Ok(())
            }
        }
        Node::Ind(r) => {
            stack.pop().ok_or(EvalError::EmptyStack)?;
            stack.push(*r);
            Ok(())
        }
        Node::Prim(_, p) => {
            let p = p.clone();
            prim_step(state, p)
        }
        Node::Data(_, _) => {
            if stack.len() != 1 {
                Err(EvalError::DataAp)
            } else {
                *stack = dump.pop().ok_or(EvalError::EmptyDump)?;
                Ok(())
            }
        }
    }
}

fn unwrap_data<'a>(
    s: &mut TiStack,
    d: &mut TiDump,
    new_stack: Vec<Addr>,
    n: &'a Node,
) -> Result<Option<(&'a u32, &'a Vec<Addr>)>, EvalError> {
    let Node::Data(t, args_) = n else {
        s.pop().ok_or(EvalError::EmptyStack)?;
        d.push(s.clone());
        *s = new_stack;
        return Ok(None);
    };
    Ok(Some((t, args_)))
}

fn prim_step(state: &mut TiState, prim: Primitive) -> Result<(), EvalError> {
    let (output, stack, dump, heap, _, _) = state;
    let args_len = match &prim {
        Primitive::Abort => return Err(EvalError::AbortSig),
        Primitive::Stop => return Err(EvalError::TypeMismatch),
        Primitive::Neg => 1,
        Primitive::Constr(_, n) => *n as usize,
        Primitive::If | Primitive::CaseList => 3,
        Primitive::Eq
        | Primitive::NotEq
        | Primitive::Greater
        | Primitive::GreaterEq
        | Primitive::Less
        | Primitive::LessEq
        | Primitive::Add
        | Primitive::Sub
        | Primitive::Mul
        | Primitive::Div
        | Primitive::CasePair
        | Primitive::Print => 2,
    };
    if stack.len() != args_len + 1 {
        Err(EvalError::ArgsLengthMismatch {
            expected: args_len + 1,
            actual: stack.len(),
        })
    } else {
        let args = heap.get_args(stack, args_len).map_err(EvalError::Heap)?;
        match &prim {
            Primitive::Neg => {
                let arg = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                match arg {
                    Node::Num(n) => {
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        heap.update(*stack.last().ok_or(EvalError::EmptyStack)?, Node::Num(-n))
                            .map_err(EvalError::Heap)
                    }
                    _ => {
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        dump.push(stack.clone());
                        *stack = args;
                        Ok(())
                    }
                }
            }
            Primitive::Constr(t, n) => {
                for _ in 0..*n {
                    stack.pop().ok_or(EvalError::EmptyStack)?;
                }
                heap.update(
                    *stack.last().ok_or(EvalError::EmptyStack)?,
                    Node::Data(*t, args),
                )
                    .map_err(EvalError::Heap)
            }
            Primitive::If => {
                let cond = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                match unwrap_data(stack, dump, vec![args[0]], cond)? {
                    Some((t, args_)) => {
                        if !args_.is_empty() {
                            return Err(EvalError::TypeMismatch);
                        }
                        match t {
                            1 | 2 => {
                                stack.pop().ok_or(EvalError::EmptyStack)?;
                                stack.pop().ok_or(EvalError::EmptyStack)?;
                                stack.pop().ok_or(EvalError::EmptyStack)?;
                                heap.update(
                                    *stack.last().ok_or(EvalError::EmptyStack)?,
                                    Node::Ind(args[args_len - *t as usize]),
                                )
                                    .map_err(EvalError::Heap)
                            }
                            _ => Err(EvalError::TypeMismatch),
                        }
                    }
                    None => Ok(()),
                }
            }
            Primitive::Add
            | Primitive::Sub
            | Primitive::Mul
            | Primitive::Div
            | Primitive::Greater
            | Primitive::GreaterEq
            | Primitive::Less
            | Primitive::LessEq
            | Primitive::Eq
            | Primitive::NotEq => {
                let arg1 = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                let arg2 = heap.lookup(args[1]).map_err(EvalError::Heap)?;
                let res = match (arg1, arg2) {
                    (Node::Num(_), Node::Data(_, _)) | (Node::Data(_, _), Node::Num(_)) => {
                        return Err(EvalError::TypeMismatch);
                    }
                    (Node::Data(_, _), Node::Data(_, _)) => match &prim {
                        Primitive::Eq | Primitive::NotEq => prim.eq_data(arg1, arg2),
                        _ => return Err(EvalError::TypeMismatch),
                    },
                    (Node::Num(m), Node::Num(n)) => match &prim {
                        Primitive::Eq
                        | Primitive::NotEq
                        | Primitive::LessEq
                        | Primitive::Less
                        | Primitive::GreaterEq
                        | Primitive::Greater => prim.cmp(*m, *n),
                        Primitive::Add | Primitive::Sub | Primitive::Mul | Primitive::Div => {
                            Node::Num(prim.arith(*m, *n))
                        }
                        _ => return Err(EvalError::TypeMismatch),
                    },
                    (Node::Num(_), _) | (Node::Data(_, _), _) => {
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        dump.push(stack.clone());
                        *stack = vec![args[1]];
                        return Ok(());
                    }
                    (_, _) => {
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        dump.push(stack.clone());
                        *stack = vec![args[0]];
                        return Ok(());
                    }
                };
                stack.pop().ok_or(EvalError::EmptyStack)?;
                stack.pop().ok_or(EvalError::EmptyStack)?;
                heap.update(*stack.last().unwrap(), res)
                    .map_err(EvalError::Heap)
            }
            Primitive::CasePair => {
                let pair = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                match unwrap_data(stack, dump, vec![args[0]], pair)? {
                    Some((t, args_)) => {
                        if *t != 1 || args_.len() != 2 {
                            return Err(EvalError::TypeMismatch);
                        }
                        let f_addr = args[1];
                        let fst = args_[0];
                        let snd = args_[1];
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        let f_a = heap.alloc(Node::Ap(f_addr, fst));
                        heap.update(
                            *stack.last().ok_or(EvalError::EmptyStack)?,
                            Node::Ap(f_a, snd),
                        )
                            .map_err(EvalError::Heap)
                    }
                    None => Ok(()),
                }
            }
            Primitive::CaseList => {
                let list = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                match unwrap_data(stack, dump, vec![args[0]], list)? {
                    Some((t, args_)) => {
                        if *t == 1 && args_.is_empty() {
                            let nil = args[1];
                            stack.pop().ok_or(EvalError::EmptyStack)?;
                            stack.pop().ok_or(EvalError::EmptyStack)?;
                            stack.pop().ok_or(EvalError::EmptyStack)?;
                            heap.update(*stack.last().ok_or(EvalError::EmptyStack)?, Node::Ind(nil))
                                .map_err(EvalError::Heap)
                        } else if *t == 2 && args_.len() == 2 {
                            let head = args_[0];
                            let tail = args_[1];
                            let f = args[2];
                            stack.pop().ok_or(EvalError::EmptyStack)?;
                            stack.pop().ok_or(EvalError::EmptyStack)?;
                            stack.pop().ok_or(EvalError::EmptyStack)?;
                            let f_h = heap.alloc(Node::Ap(f, head));
                            heap.update(
                                *stack.last().ok_or(EvalError::EmptyStack)?,
                                Node::Ap(f_h, tail),
                            )
                                .map_err(EvalError::Heap)
                        } else {
                            Err(EvalError::TypeMismatch)
                        }
                    }
                    None => Ok(()),
                }
            }
            Primitive::Abort => unreachable!(),
            Primitive::Stop => unreachable!(),
            Primitive::Print => {
                let arg1 = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                match arg1 {
                    Node::Num(n) => {
                        output.push(*n);
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.push(args[1]);
                        Ok(())
                    }
                    _ => {
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        stack.pop().ok_or(EvalError::EmptyStack)?;
                        dump.push(stack.clone());
                        *stack = vec![args[0]];
                        Ok(())
                    }
                }
            }
        }
    }
}

fn sc_step(
    state: &mut TiState,
    sc_addr: Addr,
    arg_names: Vec<lang::Name>,
    body: lang::CoreExpr,
) -> Result<(), EvalError> {
    let (_, stack, _, heap, globals, stat) = state;
    let arg_names_len = arg_names.len();
    let arg_bindings = arg_names
        .into_iter()
        .zip(
            heap.get_args(stack, arg_names_len)
                .map_err(EvalError::Heap)?,
        )
        .collect::<Vec<_>>();
    for arg in arg_bindings.iter().rev() {
        globals.push_front(arg.clone());
    }
    let is_let = body.is_let();
    for _ in 0..arg_names_len {
        stack.pop().ok_or(EvalError::EmptyStack)?;
    }
    heap.instantiate_and_update(body, *stack.last().unwrap_or(&sc_addr), globals)
        .map_err(EvalError::Heap)?;
    if !is_let {
        for (name, _) in arg_bindings {
            for i in 0..globals.len() {
                if globals[i].0 == name {
                    globals.remove(i).expect("unknown error");
                    break;
                }
            }
        }
    }
    stat.reductions += 1;
    Ok(())
}

fn ti_final(state: &TiState) -> Result<bool, EvalError> {
    let (_, stack, dump, heap, _, _) = state;
    if !dump.is_empty() {
        Ok(false)
    } else {
        match stack.len() {
            1 => heap
                .lookup(stack[0])
                .map(Node::is_data_node)
                .map_err(EvalError::Heap),
            0 => Err(EvalError::EmptyStack),
            _ => Ok(false),
        }
    }
}

fn do_admin(state: &mut TiState) {
    let (_, stack, _, heap, _, stat) = state;
    stat.heap_alloc_count = heap.alloc_count();
    stat.max_stack_size = max(stat.max_stack_size, stack.len());
}

pub(crate) fn show_results(
    states: Vec<TiState>,
) -> Result<(Vec<i64>, Vec<Node>, TiStats), ResultError> {
    let last_state = states.last().ok_or(ResultError::NoState)?;
    let mut nodes = get_stack_results(last_state)?;
    if nodes.len() == 1 {
        if let Node::Prim(_, Primitive::Stop) = nodes[0] {
            nodes.remove(0);
        }
    }
    let (output, _, _, _, _, stat) = last_state;
    Ok((output.clone(), nodes, stat.clone()))
}

pub(crate) fn get_stack_results(state: &TiState) -> Result<Vec<Node>, ResultError> {
    let (_, stack, _, heap, _, _) = state;
    Ok(stack
        .iter()
        .map(|addr| {
            heap.lookup(*addr)
                .map_err(ResultError::Heap)
                .map(Clone::clone)
        })
        .collect::<Result<Vec<_>, ResultError>>()?)
}

#[derive(Debug)]
pub enum ResultError {
    NoState,
    Heap(HeapError),
    Syntax(SyntaxError),
    Compile(CompileError),
    Eval(EvalError),
}
