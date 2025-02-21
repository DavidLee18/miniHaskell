use crate::compiler::primitives::Primitive;
use crate::core::{map_accuml, Addr, Heap, HeapError, ASSOC};
use crate::lang;
use crate::lang::SyntaxError;
use std::cmp::max;

pub mod primitives;

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats);

type TiStack = Vec<Addr>;
type TiDump = Vec<TiStack>;

type TiHeap = Heap<Node>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Node {
    Ap(Addr, Addr),
    SuperComb(lang::Name, Vec<lang::Name>, lang::CoreExpr),
    Num(i64),
    Ind(Addr),
    Prim(lang::Name, Primitive),
}

impl Node {
    pub fn is_data_node(&self) -> bool {
        match self {
            Node::Num(_) => true,
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
}

pub(crate) fn compile(p: lang::CoreProgram) -> Result<TiState, CompileError> {
    let sc_defs = vec![
        p,
        lang::parse_raw(String::from(lang::PRELUDE_DEFS)).map_err(CompileError::Syntax)?[0].clone(),
        // lang::syntax(lang::clex(String::from(EXTRA_PRELUDE_DEFS))),
    ]
        .into_iter()
        .flatten()
        .collect();
    let (init_heap, globals) = build_init_heap(sc_defs);
    let main_addr = lookup(&globals, &String::from("main")).ok_or(CompileError::NoMain)?;
    let alloc_count = init_heap.alloc_count();
    Ok((
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

const EXTRA_PRELUDE_DEFS: &'static str = "";

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
    let (stack, dump, heap, _, _) = state;
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
    }
}

fn prim_step(state: &mut TiState, prim: Primitive) -> Result<(), EvalError> {
    let (stack, dump, heap, _, _) = state;
    let args_len = match &prim {
        Primitive::Neg => 1,
        _ => 2,
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
            _ => {
                let arg1 = heap.lookup(args[0]).map_err(EvalError::Heap)?;
                let Node::Num(n) = arg1 else {
                    stack.pop().ok_or(EvalError::EmptyStack)?;
                    dump.push(stack.clone());
                    *stack = vec![args[0]];
                    return Ok(());
                };
                let arg2 = heap.lookup(args[1]).map_err(EvalError::Heap)?;
                let Node::Num(m) = arg2 else {
                    stack.pop().ok_or(EvalError::EmptyStack)?;
                    dump.push(stack.clone());
                    *stack = vec![args[1]];
                    return Ok(());
                };
                stack.pop().ok_or(EvalError::EmptyStack)?;
                stack.pop().ok_or(EvalError::EmptyStack)?;
                heap.update(
                    *stack.last().unwrap(),
                    Node::Num(primitives::arith(&prim, *n, *m)),
                )
                    .map_err(EvalError::Heap)
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
    let (stack, _, heap, globals, stat) = state;
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
    let (stack, dump, heap, _, _) = state;
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
    let (stack, _, heap, _, stat) = state;
    stat.heap_alloc_count = heap.alloc_count();
    stat.max_stack_size = max(stat.max_stack_size, stack.len());
}

pub(crate) fn show_results(states: Vec<TiState>) -> Result<(Vec<Node>, TiStats), ResultError> {
    let last_state = states.last().ok_or(ResultError::NoState)?;
    let nodes = get_stack_results(last_state)?;
    let (_, _, _, _, stat) = last_state;
    Ok((nodes, stat.clone()))
}

pub(crate) fn get_stack_results(state: &TiState) -> Result<Vec<Node>, ResultError> {
    let (stack, _, heap, _, _) = state;
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
