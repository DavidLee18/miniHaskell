use crate::alt;
use crate::lang::combinators::{
    alt, apply, one_or_more, one_or_more_with_sep, then, then3, then4, then6, zero_or_more,
};
use crate::lang::{CoreAlt, CoreExpr, CoreProgram, CoreScDefn, Expr, PartialExpr, Token};
use std::str::FromStr;

pub type Parser<A> = Box<dyn FnOnce(Vec<Token>) -> Vec<(A, Vec<Token>)>>;

fn lit(s: &'static str) -> Parser<String> {
    sat(move |s_| s_ == s)
}

fn var() -> Parser<String> {
    sat(|s| {
        KEYWORDS.iter().all(|&k| k != s)
            && s.chars().nth(0).map_or(false, |c| c.is_alphabetic())
            && s.chars().all(|c| c.is_alphanumeric() || c == '_')
    })
}

pub(crate) fn empty<A: 'static>(a: A) -> Parser<A> {
    Box::new(|toks| vec![(a, toks)])
}

fn sat<F: FnOnce(&str) -> bool + 'static>(f: F) -> Parser<String> {
    Box::new(move |toks| {
        let mut toks = toks.into_iter();
        match toks.next() {
            Some((_, t)) if f(&t) => vec![(t, toks.collect())],
            _ => vec![],
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
    one_or_more_with_sep(sc, || lit(";"))
}

pub(crate) fn sc() -> Parser<CoreScDefn> {
    then4(
        |n, ns: &Vec<String>, _, e| (n.clone(), ns.clone(), e),
        var,
        || zero_or_more(var),
        || lit("="),
        expr,
    )
}

pub(crate) fn expr6() -> Parser<CoreExpr> {
    let mk_ap_chain: fn(Vec<CoreExpr>) -> CoreExpr = |axs| {
        if axs.len() == 1 {
            axs.into_iter().next().unwrap()
        } else {
            axs.into_iter()
                .reduce(|e1, e2| Expr::Ap(Box::new(e1), Box::new(e2)))
                .expect("Syntax Error")
        }
    };
    apply(|| one_or_more(aexpr), mk_ap_chain)
}

fn aexpr() -> Parser<CoreExpr> {
    alt!(
        || apply(var, Expr::Var),
        || apply(num, |n| Expr::Num(n as i64)),
        || then(|_, n| Expr::Num(-(n as i64)), || lit("-"), num),
        || then6(
            |_, _, n1, _, n2, _| Expr::Constr {
                tag: *n1,
                arity: *n2,
            },
            || lit("Pack"),
            || lit("{"),
            num,
            || lit(","),
            num,
            || lit("}"),
        ),
        || then3(|_, e, _| e.clone(), || lit("("), expr, || lit(")"))
    )
}

fn expr1c() -> Parser<PartialExpr> {
    alt(
        || {
            then(
                |n: &String, e| PartialExpr::FoundOp(n.clone(), e),
                || lit("|"),
                expr1,
            )
        },
        || empty(PartialExpr::NoOp),
    )
}
fn expr1() -> Parser<CoreExpr> {
    then(assemble_op, expr2, expr1c)
}

fn expr2c() -> Parser<PartialExpr> {
    alt(
        || {
            then(
                |o: &String, e| PartialExpr::FoundOp(o.clone(), e),
                || lit("&"),
                expr2,
            )
        },
        || empty(PartialExpr::NoOp),
    )
}
fn expr2() -> Parser<CoreExpr> {
    then(assemble_op, expr3, expr2c)
}

fn expr4c() -> Parser<PartialExpr> {
    alt(
        || then(|o, e| PartialExpr::FoundOp(o.clone(), e), relop, expr4),
        || empty(PartialExpr::NoOp),
    )
}

fn relop() -> Parser<String> {
    alt!(
        || lit("<"),
        || lit("<="),
        || lit("=="),
        || lit("~="),
        || lit(">="),
        || lit(">")
    )
}

fn expr3() -> Parser<CoreExpr> {
    then(assemble_op, expr4, expr4c)
}

fn expr5c() -> Parser<PartialExpr> {
    alt!(
        || then(
            |o: &String, e| PartialExpr::FoundOp(o.clone(), e),
            || lit("+"),
            expr5
        ),
        || then(
            |o: &String, e| PartialExpr::FoundOp(o.clone(), e),
            || lit("-"),
            expr5
        ),
        || empty(PartialExpr::NoOp)
    )
}

fn assemble_op(e: &CoreExpr, pe: PartialExpr) -> CoreExpr {
    match pe {
        PartialExpr::NoOp => e.clone(),
        PartialExpr::FoundOp(op, e2) => Expr::Ap(
            Box::new(Expr::Ap(Box::new(Expr::Var(op)), Box::new(e.clone()))),
            Box::new(e2),
        ),
    }
}

fn expr4() -> Parser<CoreExpr> {
    then(assemble_op, expr5, expr5c)
}

fn expr6c() -> Parser<PartialExpr> {
    alt!(
        || then(
            |o: &String, e| PartialExpr::FoundOp(o.clone(), e),
            || lit("*"),
            expr6
        ),
        || then(
            |o: &String, e| PartialExpr::FoundOp(o.clone(), e),
            || lit("/"),
            expr6
        ),
        || empty(PartialExpr::NoOp)
    )
}

fn expr5() -> Parser<CoreExpr> {
    then(assemble_op, expr6, expr6c)
}

fn let_in() -> Parser<CoreExpr> {
    then4(
        |_, dfs: &Vec<(String, CoreExpr)>, _, e| Expr::Let {
            defs: dfs.clone(),
            body: Box::new(e),
        },
        || lit("let"),
        || one_or_more_with_sep(defn, || lit(";")),
        || lit("in"),
        expr,
    )
}

fn case_of() -> Parser<CoreExpr> {
    then4(
        |_, e, _, alts| Expr::Case(Box::new(e.clone()), alts),
        || lit("case"),
        expr,
        || lit("of"),
        || one_or_more_with_sep(alter, || lit(";")),
    )
}

fn lambda() -> Parser<CoreExpr> {
    then4(
        |_, vars: &Vec<String>, _, e| Expr::Lam(vars.clone(), Box::new(e)),
        || lit("\\"),
        || one_or_more(var),
        || lit("."),
        expr,
    )
}

pub(crate) fn expr() -> Parser<CoreExpr> {
    alt!(let_in, case_of, lambda, expr1)
}

fn alter() -> Parser<CoreAlt> {
    then6(
        |_, n, _, vs: &Vec<String>, _, e| (*n, vs.clone(), e),
        || lit("<"),
        num,
        || lit(">"),
        || zero_or_more(var),
        || lit("->"),
        expr,
    )
}

fn defn() -> Parser<(String, CoreExpr)> {
    then3(|v, _, e| (v.clone(), e), var, || lit("="), expr)
}
