use crate::lang::PRELUDE_DEFS;

pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::parser::program()(lang::clex(String::from(PRELUDE_DEFS)))
    );
}
