use std::path::PathBuf;

pub mod core;
pub mod lang;
pub mod compiler;
#[cfg(test)]
mod test;

fn run(p: PathBuf) -> String {
    compiler::show_results(compiler::eval(compiler::compile(lang::parse(p))))
}

fn main() {
    let m = (2, 3, 0, 0);
    println!("{:?}", m);
    let m = lang::eval_mult(m);
    println!("{:?}", m);
}
