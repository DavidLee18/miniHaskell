use crate::compiler::{Node, ResultError, TiStats};
use crate::lang::SyntaxError;
use std::path::PathBuf;

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

fn main() {
    let program = String::from(
        r#"
            main = printList (take 60 fibs);
            fibs = Cons 0 (Cons 1 (zipWith add fibs (tail fibs)));
            add a b = a + b;
            zipWith f xs ys = caseList xs Nil (zipWith_ f ys);
            zipWith_ f ys x xs = caseList ys Nil (zipWith__ f x xs);
            zipWith__ f x xs y ys = Cons (f x y) (zipWith f xs ys);
            take n xs = if (n == 0) Nil (caseList xs Nil (takeCons n));
            takeCons n x xs = Cons x (take (n-1) xs);
        "#,
    );
    println!("{:?}", run(program));
}
