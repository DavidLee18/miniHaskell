pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::parser::greeting()(vec![
            (0, String::from("goodbye")),
            (0, String::from("James")),
            (0, String::from("!"))
        ])
    );
}
