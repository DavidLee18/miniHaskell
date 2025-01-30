pub mod lang;
#[cfg(test)]
mod test;

fn main() {
    println!(
        "{:?}",
        lang::syntax(lang::clex(String::from(
            "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2"
        )))
    );
}
