use crate::core::{map_accuml, Addr, Heap, ASSOC};
use crate::lang;

type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats);

type TiStack = Vec<Addr>;
type TiDump = ();

type TiHeap = Heap<Node>;

#[derive(Clone, Debug)]
pub(crate) enum Node {
    Ap(Addr, Addr),
    SuperComb(lang::Name, Vec<lang::Name>, lang::CoreExpr),
    Num(i64),
}
type TiGlobals = ASSOC<lang::Name, Addr>;
type TiStats = u64;

fn apply_to_stats<F: FnOnce(&mut TiStats)>(f: F, s: &mut TiState) {
    let (_, _, _, _, stats) = s;
    f(stats);
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
    (vec![*main_addr], (), init_heap, globals, 0)
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
    res
}

fn step(state: &mut TiState) {
    let (stack, _, heap, _, _) = state;

    match heap
        .lookup(*stack.last().expect("Empty stack"))
        .expect("cannot be found on heap")
    {
        Node::Ap(a1, a2) => stack.push(*a1),
        Node::SuperComb(sc, args, body) => {
            let (sc, args, body) = (sc.clone(), args.clone(), body.clone());
            sc_step(state, sc, args, body)
        }
        Node::Num(n) => panic!("Number applied as a function")
    }
}

fn sc_step(
    state: &mut TiState,
    sc_name: lang::Name,
    arg_names: Vec<lang::Name>,
    body: lang::CoreExpr,
) {
    let (stack, _, heap, globals, _) = state;
    let arg_names_len = arg_names.len();
    let arg_bindings = arg_names
        .into_iter()
        .zip(heap.get_args(stack))
        .collect::<Vec<_>>();
    let env = arg_bindings
        .into_iter()
        .chain(globals.iter().cloned())
        .collect();
    let result_addr = heap.instantiate(body, &env);
    let mut i = 0;
    while let Some(_) = stack.pop() {
        if i > arg_names_len {
            break;
        }
        i += 1;
    }
    stack.push(result_addr);
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
    // apply_to_stats(ti_stat_inc_steps, state)
}

fn ti_stat_inc_steps(state: &mut TiStats) {
    todo!()
}

pub(crate) fn show_results(states: Vec<TiState>) -> String {
    format!("{:?}", states)
}
