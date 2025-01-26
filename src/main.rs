use crate::lang::SIMPLE_PROGRAM;

pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::syntax(lang::clex(String::from(SIMPLE_PROGRAM)))
    );
}
