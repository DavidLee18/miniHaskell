use crate::lang::{CoreAlt, CoreExpr, CoreProgram, CoreScDefn, Expr, PartialExpr, Token};
use std::str::FromStr;

type Parser<A> = Box<dyn Fn(Vec<Token>) -> Vec<(A, Vec<Token>)>>;

fn lit(s: String) -> Parser<String> {
    sat(move |s_| s_ == s)
}

fn var() -> Parser<String> {
    sat(move |s| {
        KEYWORDS.iter().all(|&k| k != s)
            && s.chars().nth(0).map_or(false, |c| c.is_alphabetic())
            && s.chars().all(|c| c.is_alphanumeric() || c == '_')
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

pub(crate) fn program() -> Parser<CoreProgram> {
    one_or_more_with_sep(sc(), lit(String::from(";")))
}

fn sc() -> Parser<CoreScDefn> {
    let mk_sc = |n, ns, _, e| (n, ns, e);
    then4(
        mk_sc,
        var(),
        zero_or_more(var()),
        lit(String::from("=")),
        expr(),
    )
}

pub(crate) fn expr6() -> Parser<CoreExpr> {
    let mk_ap_chain: fn(Vec<CoreExpr>) -> CoreExpr = |axs| {
        assert!(axs.len() >= 1, "Syntax Error");
        if axs.len() == 1 {
            axs[0].clone()
        } else {
            axs.into_iter()
                .rev()
                .reduce(|e1, e2| Expr::Ap(Box::new(e2.clone()), Box::new(e1.clone())))
                .expect("Syntax Error")
        }
    };
    apply(one_or_more(aexpr()), mk_ap_chain)
}

fn aexpr() -> Parser<CoreExpr> {
    alt(
        apply(var(), Expr::Var),
        alt(
            apply(num(), |n| Expr::Num(n as i64)),
            alt(
                then(|_, n| Expr::Num(-(n as i64)), lit(String::from("-")), num()),
                alt(
                    then6(
                        |_, _, n1, _, n2, _| Expr::Constr { tag: n1, arity: n2 },
                        lit(String::from("Pack")),
                        lit(String::from("{")),
                        num(),
                        lit(String::from(",")),
                        num(),
                        lit(String::from("}")),
                    ),
                    then3(
                        |_, e, _| e,
                        lit(String::from("(")),
                        expr(),
                        lit(String::from(")")),
                    ),
                ),
            ),
        ),
    )
}

fn then6<
    A: Clone + 'static,
    B: Clone + 'static,
    C: Clone + 'static,
    D: 'static + Clone,
    E: Clone + 'static,
    F_: 'static,
    G,
    F: Fn(A, B, C, D, E, F_) -> G + 'static,
>(
    comb: F,
    a: Parser<A>,
    b: Parser<B>,
    c: Parser<C>,
    d: Parser<D>,
    e: Parser<E>,
    f: Parser<F_>,
) -> Parser<G> {
    Box::new(move |toks| {
        let mut res = vec![];
        for (v1, toks1) in a(toks) {
            for (v2, toks2) in b(toks1) {
                for (v3, toks3) in c(toks2) {
                    for (v4, toks4) in d(toks3) {
                        for (v5, toks5) in e(toks4) {
                            for (v6, tok6) in f(toks5) {
                                res.push((
                                    comb(
                                        v1.clone(),
                                        v2.clone(),
                                        v3.clone(),
                                        v4.clone(),
                                        v5.clone(),
                                        v6,
                                    ),
                                    tok6,
                                ));
                            }
                        }
                    }
                }
            }
        }
        res
    })
}

macro_rules! partial_expr {
    ($e: ident, $pe: ident, $p: expr, $n: ident) => {
        fn $pe() -> Parser<PartialExpr> {
            alt(
                then(PartialExpr::FoundOp, $p, $e()),
                empty(PartialExpr::NoOp),
            )
        }

        fn $e() -> Parser<CoreExpr> {
            let assemble_op = |e, pe| match pe {
                PartialExpr::NoOp => e,
                PartialExpr::FoundOp(op, e2) => Expr::Ap(
                    Box::new(Expr::Ap(Box::new(Expr::Var(op)), Box::new(e))),
                    Box::new(e2),
                ),
            };
            then(assemble_op, $n(), $pe())
        }
    };
}

partial_expr!(expr1, expr1c, lit(String::from("|")), expr2);
partial_expr!(expr2, expr2c, lit(String::from("&")), expr3);

fn expr4c() -> Parser<PartialExpr> {
    alt(
        then(PartialExpr::FoundOp, relop(), expr4()),
        empty(PartialExpr::NoOp),
    )
}

fn relop() -> Parser<String> {
    alt(
        lit(String::from("<")),
        alt(
            lit(String::from("<=")),
            alt(
                lit(String::from("==")),
                alt(
                    lit(String::from("~=")),
                    alt(lit(String::from(">=")), lit(String::from(">"))),
                ),
            ),
        ),
    )
}

fn expr3() -> Parser<CoreExpr> {
    then(assemble_op, expr4(), expr4c())
}

fn expr45c() -> Parser<PartialExpr> {
    alt(
        alt(
            then(PartialExpr::FoundOp, lit(String::from("+")), expr4()),
            then(PartialExpr::FoundOp, lit(String::from("-")), expr5()),
        ),
        empty(PartialExpr::NoOp),
    )
}

fn assemble_op(e: CoreExpr, pe: PartialExpr) -> CoreExpr {
    match pe {
        PartialExpr::NoOp => e,
        PartialExpr::FoundOp(op, e2) => Expr::Ap(
            Box::new(Expr::Ap(Box::new(Expr::Var(op)), Box::new(e))),
            Box::new(e2),
        ),
    }
}

fn expr4() -> Parser<CoreExpr> {
    then(assemble_op, expr5(), expr45c())
}

fn expr56c() -> Parser<PartialExpr> {
    alt(
        alt(
            then(PartialExpr::FoundOp, lit(String::from("*")), expr5()),
            then(PartialExpr::FoundOp, lit(String::from("/")), expr6()),
        ),
        empty(PartialExpr::NoOp),
    )
}

fn expr5() -> Parser<CoreExpr> {
    then(assemble_op, expr6(), expr56c())
}

fn expr() -> Parser<CoreExpr> {
    alt(
        then4(
            |_, dfs, _, e| Expr::Let {
                is_rec: false,
                defs: dfs,
                body: Box::new(e),
            },
            lit(String::from("let")),
            one_or_more_with_sep(defn(), lit(String::from(";"))),
            lit(String::from("in")),
            expr1(),
        ),
        alt(
            then4(
                |_, dfs, _, e| Expr::Let {
                    is_rec: true,
                    defs: dfs,
                    body: Box::new(e),
                },
                lit(String::from("letrec")),
                one_or_more_with_sep(defn(), lit(String::from(";"))),
                lit(String::from("in")),
                expr1(),
            ),
            alt(
                then4(
                    |_, e, _, alts| Expr::Case(Box::new(e), alts),
                    lit(String::from("case")),
                    expr1(),
                    lit(String::from("of")),
                    one_or_more_with_sep(alter(), lit(String::from(";"))),
                ),
                alt(
                    then4(
                        |_, vars, _, e| Expr::Lam(vars, Box::new(e)),
                        lit(String::from("\\")),
                        one_or_more(var()),
                        lit(String::from(".")),
                        expr1(),
                    ),
                    expr1(),
                ),
            ),
        ),
    )
}

fn alter() -> Parser<CoreAlt> {
    then6(
        |_, n, _, vs, _, e| (n, vs, e),
        lit(String::from("<")),
        num(),
        lit(String::from(">")),
        zero_or_more(var()),
        lit(String::from("->")),
        expr(),
    )
}

fn defn() -> Parser<(String, CoreExpr)> {
    then3(|v, _, e| (v, e), var(), lit(String::from("=")), expr())
}
