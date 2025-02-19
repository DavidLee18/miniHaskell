use std::path::PathBuf;

pub mod compiler;
pub mod core;
pub mod lang;
#[cfg(test)]
mod test;

fn run_file(p: PathBuf) -> String {
    compiler::show_results(compiler::eval(compiler::compile(lang::parse(p))))
}

fn run(s: String) -> String {
    compiler::show_results(compiler::eval(compiler::compile(lang::parse_raw(s))))
}

fn main() {
    let program = String::from("id x = x; main = twice twice id 3");
    println!("{}", run(program));
}
