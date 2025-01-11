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

fn then3<A: Clone + 'static, B: Clone + 'static, C: 'static, D, F: Fn(A, B, C) -> D + 'static>(
    comb: F,
    a: Parser<A>,
    b: Parser<B>,
    c: Parser<C>,
) -> Parser<D> {
    Box::new(move |toks| {
        let mut res = vec![];
        for (v1, toks1) in a(toks) {
            for (v2, toks2) in b(toks1) {
                for (v3, toks3) in c(toks2) {
                    res.push((comb(v1.clone(), v2.clone(), v3), toks3));
                }
            }
        }
        res
    })
}

fn then4<
    A: Clone + 'static,
    B: Clone + 'static,
    C: Clone + 'static,
    D: 'static,
    E,
    F: Fn(A, B, C, D) -> E + 'static,
>(
    comb: F,
    a: Parser<A>,
    b: Parser<B>,
    c: Parser<C>,
    d: Parser<D>,
) -> Parser<E> {
    Box::new(move |toks| {
        let mut res = vec![];
        for (v1, toks1) in a(toks) {
            for (v2, toks2) in b(toks1) {
                for (v3, toks3) in c(toks2) {
                    for (v4, toks4) in d(toks3) {
                        res.push((comb(v1.clone(), v2.clone(), v3.clone(), v4), toks4));
                    }
                }
            }
        }
        res
    })
}

pub(crate) fn greeting() -> Parser<(String, String)> {
    then3(
        move |hg, name, _| (hg, name),
        hello_or_goodbye(),
        var(),
        lit(String::from("!")),
    )
}

fn zero_or_more<A: 'static + Clone>(p: Parser<A>) -> Parser<Vec<A>> {
    alt(one_or_more(p), empty(vec![]))
}

fn one_or_more<A: Clone + 'static>(p: Parser<A>) -> Parser<Vec<A>> {
    then(
        |p_, mut ps| {
            ps.insert(0, p_);
            ps
        },
        p,
        zero_or_more(p),
    )
}

fn empty<A: 'static + Clone>(a: A) -> Parser<A> {
    Box::new(move |toks| vec![(a.clone(), toks)])
}

fn greetings() -> Parser<Vec<(String, String)>> {
    zero_or_more(greeting())
}
