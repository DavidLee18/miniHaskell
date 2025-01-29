pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::parser::program()(lang::clex(String::from(
            "double x = x + x; main = let y = 42 in double y"
        )))
    );
}
