use crate::lang::combinators::{
    alt, apply, one_or_more, one_or_more_with_sep, then, then3, then4, then6, zero_or_more,
};
use crate::lang::{CoreAlt, CoreExpr, CoreProgram, CoreScDefn, Expr, PartialExpr, Token};
use std::str::FromStr;

pub type Parser<A> = Box<dyn Fn(Vec<Token>) -> Vec<(A, Vec<Token>)>>;

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

pub(crate) fn empty<A: 'static + Clone>(a: A) -> Parser<A> {
    Box::new(move |toks| vec![(a.clone(), toks)])
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

const KEYWORDS: [&'static str; 5] = ["let", "case", "in", "of", "Pack"];

fn num() -> Parser<u32> {
    apply(
        || sat(|s| s.chars().all(|c| c.is_digit(10))),
        |s| u32::from_str(&s).unwrap(),
    )
}

pub fn program() -> Parser<CoreProgram> {
    one_or_more_with_sep(sc, || lit(String::from(";")))
}

pub(crate) fn sc() -> Parser<CoreScDefn> {
    then4(
        |n, ns, _, e| (n, ns, e),
        var,
        || zero_or_more(var),
        || lit(String::from("=")),
        expr,
    )
}

pub(crate) fn expr6() -> Parser<CoreExpr> {
    let mk_ap_chain: fn(Vec<CoreExpr>) -> CoreExpr = |axs| {
        if axs.len() == 1 {
            axs[0].clone()
        } else {
            axs.into_iter()
                .reduce(|e1, e2| Expr::Ap(Box::new(e1.clone()), Box::new(e2.clone())))
                .expect("Syntax Error")
        }
    };
    apply(|| one_or_more(aexpr), mk_ap_chain)
}

fn aexpr() -> Parser<CoreExpr> {
    alt(
        || apply(var, Expr::Var),
        || {
            alt(
                || apply(num, |n| Expr::Num(n as i64)),
                || {
                    alt(
                        || {
                            then(
                                |_, n| Expr::Num(-(n as i64)),
                                || lit(String::from("-")),
                                num,
                            )
                        },
                        || {
                            alt(
                                || {
                                    then6(
                                        |_, _, n1, _, n2, _| Expr::Constr { tag: n1, arity: n2 },
                                        || lit(String::from("Pack")),
                                        || lit(String::from("{")),
                                        num,
                                        || lit(String::from(",")),
                                        num,
                                        || lit(String::from("}")),
                                    )
                                },
                                || {
                                    then3(
                                        |_, e, _| e,
                                        || lit(String::from("(")),
                                        expr,
                                        || lit(String::from(")")),
                                    )
                                },
                            )
                        },
                    )
                },
            )
        },
    )
}

fn expr1c() -> Parser<PartialExpr> {
    alt(
        || then(PartialExpr::FoundOp, || lit(String::from("|")), expr1),
        || empty(PartialExpr::NoOp),
    )
}
fn expr1() -> Parser<CoreExpr> {
    then(assemble_op, expr2, expr1c)
}

fn expr2c() -> Parser<PartialExpr> {
    alt(
        || then(PartialExpr::FoundOp, || lit(String::from("&")), expr2),
        || empty(PartialExpr::NoOp),
    )
}
fn expr2() -> Parser<CoreExpr> {
    then(assemble_op, expr3, expr2c)
}

fn expr4c() -> Parser<PartialExpr> {
    alt(
        || then(PartialExpr::FoundOp, relop, expr4),
        || empty(PartialExpr::NoOp),
    )
}

fn relop() -> Parser<String> {
    alt(
        || lit(String::from("<")),
        || {
            alt(
                || lit(String::from("<=")),
                || {
                    alt(
                        || lit(String::from("==")),
                        || {
                            alt(
                                || lit(String::from("~=")),
                                || alt(|| lit(String::from(">=")), || lit(String::from(">"))),
                            )
                        },
                    )
                },
            )
        },
    )
}

fn expr3() -> Parser<CoreExpr> {
    then(assemble_op, expr4, expr4c)
}

fn expr5c() -> Parser<PartialExpr> {
    alt(
        || {
            alt(
                || then(PartialExpr::FoundOp, || lit(String::from("+")), expr5),
                || then(PartialExpr::FoundOp, || lit(String::from("-")), expr5),
            )
        },
        || empty(PartialExpr::NoOp),
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
    then(assemble_op, expr5, expr5c)
}

fn expr6c() -> Parser<PartialExpr> {
    alt(
        || {
            alt(
                || then(PartialExpr::FoundOp, || lit(String::from("*")), expr6),
                || then(PartialExpr::FoundOp, || lit(String::from("/")), expr6),
            )
        },
        || empty(PartialExpr::NoOp),
    )
}

fn expr5() -> Parser<CoreExpr> {
    then(assemble_op, expr6, expr6c)
}

fn let_in() -> Parser<CoreExpr> {
    then4(
        |_, dfs, _, e| Expr::Let {
            defs: dfs,
            body: Box::new(e),
        },
        || lit(String::from("let")),
        || one_or_more_with_sep(defn, || lit(String::from(";"))),
        || lit(String::from("in")),
        expr,
    )
}

fn case_of() -> Parser<CoreExpr> {
    then4(
        |_, e, _, alts| Expr::Case(Box::new(e), alts),
        || lit(String::from("case")),
        expr,
        || lit(String::from("of")),
        || one_or_more_with_sep(alter, || lit(String::from(";"))),
    )
}

fn lambda() -> Parser<CoreExpr> {
    then4(
        |_, vars, _, e| Expr::Lam(vars, Box::new(e)),
        || lit(String::from("\\")),
        || one_or_more(var),
        || lit(String::from(".")),
        expr,
    )
}

pub(crate) fn expr() -> Parser<CoreExpr> {
    alt(let_in, || alt(case_of, || alt(lambda, expr1)))
}

fn alter() -> Parser<CoreAlt> {
    then6(
        |_, n, _, vs, _, e| (n, vs, e),
        || lit(String::from("<")),
        num,
        || lit(String::from(">")),
        || zero_or_more(var),
        || lit(String::from("->")),
        expr,
    )
}

fn defn() -> Parser<(String, CoreExpr)> {
    then3(|v, _, e| (v, e), var, || lit(String::from("=")), expr)
}
