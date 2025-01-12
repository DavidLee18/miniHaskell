use crate::lang::Token;
use std::str::FromStr;

type Parser<A> = Box<dyn Fn(Vec<Token>) -> Vec<(A, Vec<Token>)>>;

fn lit(s: String) -> Parser<String> {
    sat(move |s_| s_ == s)
}

fn var() -> Parser<String> {
    sat(move |s| KEYWORDS.iter().all(|&k| k != s))
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
    Box::new(move |toks| {
        let mut res_mid = p(toks);
        let mut temp_vals: Vec<A> = vec![];
        let mut temp_rest = None;
        while !res_mid.is_empty() {
            res_mid.iter().for_each(|(v, _)| temp_vals.push(v.clone()));
            let rest: Vec<Token> = res_mid.iter().flat_map(|(_, tks)| tks.clone()).collect();
            temp_rest = Some(rest.clone());
            res_mid = p(rest);
        }
        vec![(temp_vals, temp_rest.unwrap_or(vec![]))]
    })
}

fn empty<A: 'static + Clone>(a: A) -> Parser<A> {
    Box::new(move |toks| vec![(a.clone(), toks)])
}

pub(crate) fn greetings() -> Parser<Vec<(String, String)>> {
    one_or_more(greeting())
}

fn apply<A: 'static, B, F: Fn(A) -> B + 'static>(a: Parser<A>, f: F) -> Parser<B> {
    Box::new(move |toks| {
        let mut res = vec![];
        for (v, toks2) in a(toks) {
            res.push((f(v), toks2));
        }
        res
    })
}

pub(crate) fn greetings_n() -> Parser<usize> {
    apply(zero_or_more(greeting()), |v| v.len())
}

fn one_or_more_with_sep<A: Clone + 'static, B: 'static>(
    a: Parser<A>,
    b: Parser<B>,
) -> Parser<Vec<A>> {
    Box::new(move |toks| {
        let mut res_mid = a(toks);
        let mut res_mid2;
        let mut temp_vals: Vec<A> = vec![];
        let mut temp_rest = None;
        let mut first = true;
        loop {
            if res_mid.is_empty() {
                if first {
                    break;
                } else {
                    return vec![];
                }
            }
            res_mid.iter().for_each(|(v, _)| temp_vals.push(v.clone()));
            let rest: Vec<Token> = res_mid.iter().flat_map(|(_, tks)| tks.clone()).collect();
            temp_rest = Some(rest.clone());
            res_mid2 = b(rest);
            if res_mid2.is_empty() {
                break;
            }
            let rest: Vec<Token> = res_mid2.iter().flat_map(|(_, tks)| tks.clone()).collect();
            temp_rest = Some(rest.clone());
            res_mid = a(rest);
            first = false;
        }
        vec![(temp_vals, temp_rest.unwrap_or(vec![]))]
    })
}

pub(crate) fn greetings_with_comma() -> Parser<Vec<(String, String)>> {
    one_or_more_with_sep(greeting(), lit(String::from(",")))
}

fn sat<F: Fn(String) -> bool + 'static>(f: F) -> Parser<String> {
    Box::new(move |toks| {
        if toks.is_empty() {
            vec![]
        } else if f(toks[0].1.clone()) {
            vec![(toks[0].1.clone(), toks[1..].to_vec())]
        } else {
            vec![]
        }
    })
}

const KEYWORDS: [&'static str; 6] = ["let", "letrec", "case", "in", "of", "Pack"];

fn num() -> Parser<u32> {
    apply(sat(|s| s.chars().all(|c| c.is_digit(10))), |s| {
        u32::from_str(&s).unwrap()
    })
}
