pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::parser::program()(
            lang::clex(String::from("main = double 21;\ndouble x = x + x"))
        )
    );
}
