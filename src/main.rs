pub mod lang;

fn main() {
    println!(
        "{:?}",
        lang::parser::greetings_with_comma()(vec![
            (0, String::from("goodbye")),
            (0, String::from("James")),
            (0, String::from("!")),
            (0, String::from(",")),
            (0, String::from("hello")),
            (0, String::from("Chris")),
            (0, String::from("!")),
            (0, String::from(",")),
            (0, String::from("goodbye")),
            (0, String::from("Yeager")),
            (0, String::from("!")),
        ])
    );
}
