use crate::lang::Token;

type Parser<A> = Box<dyn Fn(Vec<Token>) -> Vec<(A, Vec<Token>)>>;

fn lit(s: String) -> Parser<String> {
    Box::new(move |toks: Vec<Token>| {
        if toks.is_empty() {
            vec![]
        } else if toks[0].1 == s {
            vec![(s.clone(), toks[1..].to_vec())]
        } else {
            vec![]
        }
    })
}

fn var() -> Parser<String> {
    Box::new(|toks| {
        if toks.is_empty() {
            vec![]
        } else {
            vec![(toks[0].1.clone(), toks[1..].to_vec())]
        }
    })
}

fn alt<A: 'static>(a: Parser<A>, b: Parser<A>) -> Parser<A> {
    Box::new(move |toks: Vec<Token>| {
        let mut res = a(toks.clone());
        res.append(&mut b(toks));
        res
    })
}

fn hello_or_goodbye() -> Parser<String> {
    alt(lit(String::from("hello")), lit(String::from("goodbye")))
}

fn then<A: Clone + 'static, B: 'static, C, F: Fn(A, B) -> C + 'static>(
    comb: F,
    a: Parser<A>,
    b: Parser<B>,
) -> Parser<C> {
    Box::new(move |toks| {
        let mut res = vec![];
        for (v1, toks1) in a(toks) {
            for (v2, toks2) in b(toks1) {
                res.push((comb(v1.clone(), v2), toks2));
            }
        }
        res
    })
}

pub(crate) fn greeting() -> Parser<(String, String)> {
    then(move |hg, name| (hg, name), hello_or_goodbye(), var())
}
