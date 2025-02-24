use crate::compiler::primitives::Primitive;
use crate::compiler::{Node, ResultError, TiStats};
use crate::lang::SyntaxError;
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
        let sc_def = match lang::parser::sc()(tokens.clone()) {
            v => {
                let mut ress = v
                    .into_iter()
                    .filter(|(_, v)| v.is_empty())
                    .map(|(p, _)| p)
                    .collect::<Vec<_>>();
                if ress.len() == 1 {
                    ress.remove(0)
                } else {
                    match lang::parser::expr()(tokens) {
                        v => {
                            let mut ress = v
                                .into_iter()
                                .filter(|(_, v)| v.is_empty())
                                .map(|(p, _)| p)
                                .collect::<Vec<_>>();
                            if ress.len() == 1 {
                                (String::from("it"), vec![], ress.remove(0))
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
                if sc_def.0 != "it" {
                    state
                        .3
                        .update(*ad, Node::SuperComb(sc_def.0.clone(), sc_def.1, sc_def.2))?;
                }
                *ad
            }
            None => state.3.alloc(Node::SuperComb(
                sc_def.0.clone(),
                sc_def.1,
                sc_def.2.clone(),
            )),
        };
        state.4.push_back((sc_def.0.clone(), sc_addr));
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
