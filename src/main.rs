pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::parser::program()(lang::clex(String::from("main = let x = 42 in x")))
    );
}
