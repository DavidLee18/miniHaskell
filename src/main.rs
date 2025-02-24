use crate::compiler::primitives::Primitive;
use crate::compiler::{Node, ResultError, TiStats};
use crate::core::{Addr, Heap, HeapError, ASSOC};
use crate::lang::{CoreExpr, Expr, Name, SyntaxError};
use signal_hook::consts::{SIGINT, SIGTERM};
use std::io::Write;
use std::path::PathBuf;
use std::sync::atomic::AtomicBool;
use std::sync::Arc;
use termion::color;

pub mod compiler;
pub mod core;
pub mod lang;
#[cfg(test)]
mod test;

fn run_file(
    p: PathBuf,
) -> Result<Vec<Result<(Vec<i64>, Vec<Node>, TiStats), ResultError>>, ResultError> {
    let state = lang::parse(p)
        .map_err(ResultError::Syntax)?
        .into_iter()
        .map(|p| compiler::compile(p).map_err(ResultError::Compile))
        .collect::<Vec<_>>();
    let states = state
        .into_iter()
        .map(|ss| ss.and_then(|s| compiler::eval(s).map_err(ResultError::Eval)))
        .collect::<Vec<_>>();
    Ok(states
        .into_iter()
        .map(|r| r.and_then(compiler::show_results))
        .collect::<Vec<_>>())
}

fn run(s: String) -> Result<Vec<Result<(Vec<i64>, Vec<Node>, TiStats), ResultError>>, SyntaxError> {
    let state = lang::parse_raw(s)?
        .into_iter()
        .map(|p| compiler::compile(p).map_err(ResultError::Compile))
        .collect::<Vec<_>>();
    let states = state
        .into_iter()
        .map(|ss| ss.and_then(|s| compiler::eval(s).map_err(ResultError::Eval)))
        .collect::<Vec<_>>();
    Ok(states
        .into_iter()
        .map(|r| r.and_then(compiler::show_results))
        .collect::<Vec<_>>())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let term = Arc::new(AtomicBool::new(false));
    let logo = text_to_ascii_art::to_art(String::from("miniHaskell"), "", 0, 0, 0)?;
    println!("{}", logo);
    println!("type :quit to quit");
    signal_hook::flag::register(SIGINT, Arc::clone(&term))?;
    signal_hook::flag::register(SIGTERM, Arc::clone(&term))?;
    let mut temp_programs = lang::parse_raw(String::from(lang::PRELUDE_DEFS))?;
    let mut sc_defs = vec![];
    let mut temp_program = temp_programs.remove(0);
    sc_defs.append(&mut temp_program);
    temp_programs.clear();
    temp_programs = lang::parse_raw(String::from(compiler::EXTRA_PRELUDE_DEFS))?;
    temp_program = temp_programs.remove(0);
    sc_defs.append(&mut temp_program);
    let (init_heap, globals) = compiler::build_init_heap(sc_defs);
    let heap_alloc_count = init_heap.alloc_count();
    let mut state = (
        vec![],
        vec![],
        vec![],
        init_heap,
        globals,
        TiStats {
            heap_alloc_count,
            ..TiStats::default()
        },
    );
    let mut states;
    let mut prev_it = None;
    loop {
        state.1.clear();
        let mut input = String::new();
        print!("miniHaskell>");
        std::io::stdout().flush()?;
        std::io::stdin().read_line(&mut input)?;
        input = input.trim().to_string();
        if input == ":quit" {
            println!("exiting miniHaskell");
            break;
        } else if input == ":state" {
            println!("Output: {:?}", state.0);
            println!("Stack: {:?}", state.1);
            println!("Dump: {:?}", state.2);
            println!("{:?}", state.3);
            println!("Globals: {:?}", state.4);
            println!("Stats: {:?}", state.5);
            continue;
        }
        let tokens = lang::clex(input);
        match compiler::lookup(&state.4, &String::from("it")) {
            None => (),
            Some(it_addr) => {
                let it_addr = *it_addr;
                for i in 0..state.4.len() {
                    if state.4[i].1 == it_addr {
                        state.4.remove(i);
                        break;
                    }
                }
                match state.3.lookup(it_addr) {
                    Ok(_) => {
                        prev_it = Some(it_addr);
                    }
                    Err(_) => {
                        eprintln!(
                            "{}found 'it' in globals but not in heap{}",
                            color::Fg(color::Red),
                            color::Fg(color::Reset)
                        );
                        break;
                    }
                }
            }
        }
        let mut sc_def = match lang::parser::sc()(tokens.clone()) {
            v => {
                let mut ress = v
                    .into_iter()
                    .filter(|(_, v)| v.is_empty())
                    .map(|(p, _)| p)
                    .collect::<Vec<_>>();
                if ress.len() == 1 {
                    let mut res = ress.remove(0);
                    if res.0 == "it" && res.2.get_var_mut("it").is_some() {
                        eprintln!(
                            "{}recursive definition of 'it'{}",
                            color::Fg(color::Red),
                            color::Fg(color::Reset)
                        );
                        continue;
                    }
                    res
                } else {
                    match lang::parser::expr()(tokens) {
                        v => {
                            let mut ress = v
                                .into_iter()
                                .filter(|(_, v)| v.is_empty())
                                .map(|(p, _)| p)
                                .collect::<Vec<_>>();
                            if ress.len() == 1 {
                                let mut res = ress.remove(0);
                                match res.get_var_mut("it") {
                                    None => {
                                        let mut i = 0;
                                        while i + 1 != state.4.len() {
                                            if state.4[i].0 == "it" {
                                                state.4.remove(i);
                                                i = 0;
                                            }
                                            i += 1;
                                        }
                                        prev_it = None;
                                    }
                                    Some(_) => match prev_it {
                                        Some(p_it) => {
                                            let res_addr = allocate_expr_using_prev_it(
                                                &mut state.3,
                                                &mut state.4,
                                                p_it,
                                                res.clone(),
                                            );
                                            prev_it = match res_addr {
                                                Ok(rddr) => Some(rddr),
                                                Err(HeapError::UndefinedName(n)) => {
                                                    eprintln!(
                                                        "{}undefined name '{}'{}",
                                                        color::Fg(color::Red),
                                                        n,
                                                        color::Fg(color::Reset)
                                                    );
                                                    if let Expr::Let { defs, .. } = &res {
                                                        for (n, _) in defs {
                                                            for i in 0..state.4.len() {
                                                                if state.4[i].0 == *n {
                                                                    state.4.remove(i);
                                                                    break;
                                                                }
                                                            }
                                                        }
                                                    };
                                                    continue;
                                                }
                                                Err(HeapError::NotInstantiable) => {
                                                    eprintln!(
                                                        "{}unable to instantiate{}",
                                                        color::Fg(color::Red),
                                                        color::Fg(color::Reset)
                                                    );
                                                    break;
                                                }
                                                Err(e) => {
                                                    eprintln!(
                                                        "{}unknown heap error: {}{}",
                                                        color::Fg(color::Red),
                                                        e,
                                                        color::Fg(color::Reset)
                                                    );
                                                    break;
                                                }
                                            };
                                        }
                                        None => {
                                            eprintln!(
                                                "{}previous definition of 'it' does not exist{}",
                                                color::Fg(color::Red),
                                                color::Fg(color::Reset)
                                            );
                                            continue;
                                        }
                                    },
                                }
                                (String::from("it"), vec![], res)
                            } else {
                                eprintln!(
                                    "{}invalid or ambiguous syntax{}",
                                    color::Fg(color::Red),
                                    color::Fg(color::Reset)
                                );
                                continue;
                            }
                        }
                    }
                }
            }
        };
        let sc_addr = match compiler::lookup(&state.4, &sc_def.0) {
            Some(ad) => {
                let ad = *ad;
                if sc_def.2.get_var_mut("it").is_none() {
                    state
                        .3
                        .update(ad, Node::SuperComb(sc_def.0.clone(), sc_def.1, sc_def.2))?;
                } else {
                    match prev_it {
                        Some(p_it) => {
                            let res_addr = allocate_expr_using_prev_it(
                                &mut state.3,
                                &mut state.4,
                                p_it,
                                sc_def.2.clone(),
                            );
                            match res_addr {
                                Ok(rddr) => {
                                    state.3.update(ad, Node::Ind(rddr))?;
                                }
                                Err(HeapError::UndefinedName(n)) => {
                                    eprintln!(
                                        "{}undefined name '{}'{}",
                                        color::Fg(color::Red),
                                        n,
                                        color::Fg(color::Reset)
                                    );
                                    if let Expr::Let { defs, .. } = &sc_def.2 {
                                        for (n, _) in defs {
                                            for i in 0..state.4.len() {
                                                if state.4[i].0 == *n {
                                                    state.4.remove(i);
                                                    break;
                                                }
                                            }
                                        }
                                    };
                                    continue;
                                }
                                Err(HeapError::NotInstantiable) => {
                                    eprintln!(
                                        "{}unable to instantiate{}",
                                        color::Fg(color::Red),
                                        color::Fg(color::Reset)
                                    );
                                    break;
                                }
                                Err(e) => {
                                    eprintln!(
                                        "{}unknown heap error: {}{}",
                                        color::Fg(color::Red),
                                        e,
                                        color::Fg(color::Reset)
                                    );
                                    break;
                                }
                            }
                        }
                        None => {
                            eprintln!(
                                "{}previous definition of 'it' does not exist{}",
                                color::Fg(color::Red),
                                color::Fg(color::Reset)
                            );
                            continue;
                        }
                    }
                }
                ad
            }
            None => match prev_it {
                Some(res_addr) => res_addr,
                None => state.3.alloc(Node::SuperComb(
                    sc_def.0.clone(),
                    sc_def.1,
                    sc_def.2.clone(),
                )),
            },
        };
        let look_up = (sc_def.0.clone(), sc_addr);
        if !state.4.contains(&look_up) {
            state.4.push_back(look_up);
        }
        if sc_def.0 == "it" {
            state.1.push(sc_addr);
            states = match compiler::eval(state.clone()) {
                Ok(ss) => ss,
                Err(ee) => {
                    eprintln!("{}{}{}", color::Fg(color::Red), ee, color::Fg(color::Reset));
                    continue;
                }
            };
            state = match states.pop() {
                Some(v) => v,
                None => {
                    eprintln!(
                        "{}empty state{}",
                        color::Fg(color::Red),
                        color::Fg(color::Reset)
                    );
                    break;
                }
            };
            match compiler::get_stack_results(&state) {
                Ok(nodes) => match nodes.len() {
                    1 => match &nodes[0] {
                        Node::Prim(_, Primitive::Stop) => println!("{:?}", state.0),
                        Node::Num(n) => println!("{}", n),
                        Node::Data(_, _) => println!("{:?}", nodes[0]),
                        n => {
                            eprintln!(
                                "{}invalid node: {:?}{}",
                                color::Fg(color::Red),
                                n,
                                color::Fg(color::Reset)
                            );
                            continue;
                        }
                    },
                    0 => println!("{:?}", state.0),
                    _ => {
                        eprintln!(
                            "{}invalid stack state: {:?} {}",
                            color::Fg(color::Red),
                            state.1,
                            color::Fg(color::Reset)
                        );
                        continue;
                    }
                },
                Err(e) => {
                    eprintln!(
                        "{}invalid state: {}{}",
                        color::Fg(color::Red),
                        e,
                        color::Fg(color::Reset)
                    );
                    continue;
                }
            };
        }
    }
    Ok(())
}

fn allocate_expr_using_prev_it(
    heap: &mut Heap<Node>,
    env: &mut ASSOC<Name, Addr>,
    prev_it_addr: Addr,
    expr: CoreExpr,
) -> Result<Addr, HeapError> {
    match expr {
        CoreExpr::Var(v) => {
            if v == "it" {
                Ok(heap.alloc(Node::Ind(prev_it_addr)))
            } else {
                Err(HeapError::UndefinedName(v))
            }
        }
        CoreExpr::Num(n) => Ok(heap.alloc(Node::Num(n))),
        CoreExpr::Constr { tag, arity } => Ok(heap.alloc(Node::Prim(
            String::from("Pack"),
            Primitive::Constr(tag, arity),
        ))),
        CoreExpr::Ap(e1, e2) => {
            let a1 = allocate_expr_using_prev_it(heap, env, prev_it_addr, *e1)?;
            let a2 = allocate_expr_using_prev_it(heap, env, prev_it_addr, *e2)?;
            Ok(heap.alloc(Node::Ap(a1, a2)))
        }
        CoreExpr::Let { defs, body } => {
            for (n, mut e) in defs {
                if n != "it" && e.get_var_mut("it").is_none() {
                    let addr = heap.alloc(Node::SuperComb(n.clone(), vec![], e));
                    env.push_front((n, addr));
                } else if n == "it" {
                    panic!("conflicting 'it'");
                } else {
                    let addr = allocate_expr_using_prev_it(heap, env, prev_it_addr, e)?;
                    env.push_front((n, addr));
                }
            }
            allocate_expr_using_prev_it(heap, env, prev_it_addr, *body)
        }
        CoreExpr::Case(_, _) => Err(HeapError::NotInstantiable),
        CoreExpr::Lam(_, _) => Err(HeapError::NotInstantiable),
    }
}
