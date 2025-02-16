use crate::core::{map_accuml, Addr, Heap, ASSOC};
use crate::lang;
use std::cmp::max;

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats);

type TiStack = Vec<Addr>;
type TiDump = ();

type TiHeap = Heap<Node>;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Node {
    Ap(Addr, Addr),
    SuperComb(lang::Name, Vec<lang::Name>, lang::CoreExpr),
    Num(i64),
}
type TiGlobals = ASSOC<lang::Name, Addr>;

#[derive(Debug, Default, Clone)]
pub(crate) struct TiStats {
    reductions: usize,
    max_stack_size: usize,
    heap_alloc_count: usize,
}

pub(crate) fn compile(p: lang::CoreProgram) -> TiState {
    let sc_defs = vec![
        p,
        lang::syntax(lang::clex(String::from(lang::PRELUDE_DEFS))),
        // lang::syntax(lang::clex(String::from(EXTRA_PRELUDE_DEFS))),
    ]
        .into_iter()
        .flatten()
        .collect();
    let (init_heap, globals) = build_init_heap(sc_defs);
    let main_addr = lookup(&globals, &String::from("main")).expect("main is not defined");
    let alloc_count = init_heap.alloc_count();
    (
        vec![*main_addr],
        (),
        init_heap,
        globals,
        TiStats {
            heap_alloc_count: alloc_count,
            max_stack_size: 1,
            ..TiStats::default()
        },
    )
}

fn lookup<'a, A: PartialEq, B>(a: &'a ASSOC<A, B>, k: &A) -> Option<&'a B> {
    a.iter().find(|&(a, _)| *a == *k).map(|(_, b)| b)
}

const EXTRA_PRELUDE_DEFS: &'static str = "";

fn build_init_heap(sc_defs: Vec<lang::CoreScDefn>) -> (TiHeap, TiGlobals) {
    let mut init_heap = Heap::new();
    let globals = map_accuml(allocate_sc, &mut init_heap, sc_defs);
    (init_heap, globals)
}

fn allocate_sc(heap: &mut TiHeap, sc_defs: lang::CoreScDefn) -> (lang::Name, Addr) {
    let (name, args, body) = sc_defs;
    let addr = heap.alloc(Node::SuperComb(name.clone(), args, body));
    (name, addr.expect("heap alloc failed"))
}

pub(crate) fn eval(state: TiState) -> Vec<TiState> {
    let mut res = vec![];
    let mut temp = state;
    res.push(temp.clone());
    step(&mut temp);
    do_admin(&mut temp);
    while !ti_final(&temp) {
        res.push(temp.clone());
        step(&mut temp);
        do_admin(&mut temp);
    }
    res.push(temp);
    res
}

fn step(state: &mut TiState) {
    let (stack, _, heap, _, _) = state;
    // println!("Stack: {:?}", stack);
    // println!("{:?}", heap);

    match heap
        .lookup(*stack.last().expect("Empty stack"))
        .expect("cannot be found on heap")
    {
        Node::Ap(a1, a2) => stack.push(*a1),
        Node::SuperComb(sc, args, body) => {
            let (sc, args, body) = (sc.clone(), args.clone(), body.clone());
            sc_step(state, sc, args, body)
        }
        Node::Num(n) => panic!("Number applied as a function"),
    }
}

fn sc_step(
    state: &mut TiState,
    sc_name: lang::Name,
    arg_names: Vec<lang::Name>,
    body: lang::CoreExpr,
) {
    let (stack, _, heap, globals, stat) = state;
    let arg_names_len = arg_names.len();
    let arg_bindings = arg_names
        .into_iter()
        .zip(heap.get_args(stack, arg_names_len))
        .collect::<Vec<_>>();
    // println!("Args: {:?}", arg_bindings);
    let env = arg_bindings
        .into_iter()
        .chain(globals.iter().cloned())
        .collect();
    let result_addr = heap.instantiate(body, &env);
    for _ in 0..=arg_names_len {
        stack.pop();
    }
    stack.push(result_addr);
    stat.reductions += 1;
}

fn ti_final(state: &TiState) -> bool {
    match state.0.len() {
        1 => state
            .2
            .lookup(state.0[0])
            .map(is_data_node)
            .unwrap_or(false),
        0 => panic!("Empty stack!"),
        _ => false,
    }
}

fn is_data_node(node: &Node) -> bool {
    match node {
        Node::Num(n) => true,
        _ => false,
    }
}

fn do_admin(state: &mut TiState) {
    let (stack, _, heap, _, stat) = state;
    stat.heap_alloc_count = heap.alloc_count();
    stat.max_stack_size = max(stat.max_stack_size, stack.len());
}

pub(crate) fn show_results(states: Vec<TiState>) -> String {
    let last_state = states.last().expect("No states to show");
    let nodes = get_stack_results(last_state);
    let (_, _, _, _, stat) = last_state;
    format!("{:?}", (nodes, stat))
}

pub(crate) fn get_stack_results(state: &TiState) -> Vec<Node> {
    let (stack, _, heap, _, stat) = state;
    stack
        .iter()
        .map(|addr| heap.lookup(*addr).expect("lookup failed").clone())
        .collect::<Vec<_>>()
}
