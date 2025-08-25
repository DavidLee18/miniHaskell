use crate::lang::parser::{empty, Parser};
use crate::lang::Token;

pub(crate) fn alt<A, F: FnOnce() -> Parser<A> + 'static, F_: FnOnce() -> Parser<A> + 'static>(
    a: F,
    b: F_,
) -> Parser<A> {
    Box::new(move |toks: Vec<Token>| {
        let mut res = a()(toks.clone());
        res.append(&mut b()(toks));
        res
    })
}

#[macro_export]
macro_rules! alt {
    ($f:expr, $g:expr) => {
        alt($f, $g)
    };
    ($f:expr, $g:expr$(, $h:expr)+) => {
        alt($f, || { alt!($g$(, $h)+) })
    }
}

pub(crate) fn apply<A, B, F: FnMut(A) -> B + 'static, F2: FnOnce() -> Parser<A> + 'static>(
    a: F2,
    mut f: F,
) -> Parser<B> {
    Box::new(move |toks| {
        a()(toks)
            .into_iter()
            .map(|(v, toks2)| (f(v), toks2))
            .collect()
    })
}

pub(crate) fn then<
    A,
    B,
    C,
    F: FnMut(&A, B) -> C + 'static,
    F1: FnOnce() -> Parser<A> + 'static,
    F2: FnMut() -> Parser<B> + 'static,
>(
    mut comb: F,
    a: F1,
    mut b: F2,
) -> Parser<C> {
    Box::new(move |toks| {
        let res1 = a()(toks);
        if res1.is_empty() {
            vec![]
        } else {
            res1.into_iter()
                .map(|(v1, toks1)| {
                    let res2 = b()(toks1);
                    if res2.is_empty() {
                        vec![]
                    } else {
                        res2.into_iter()
                            .map(|(v2, toks2)| (comb(&v1, v2), toks2))
                            .collect::<Vec<_>>()
                    }
                })
                .flatten()
                .collect()
        }
    })
}

pub(crate) fn then3<
    A,
    B,
    C,
    D,
    F: FnMut(&A, &B, C) -> D + 'static,
    FA: FnOnce() -> Parser<A> + 'static,
    FB: FnMut() -> Parser<B> + 'static,
    FC: FnMut() -> Parser<C> + 'static,
>(
    mut comb: F,
    a: FA,
    mut b: FB,
    mut c: FC,
) -> Parser<D> {
    Box::new(move |toks| {
        let res1 = a()(toks);
        if res1.is_empty() {
            vec![]
        } else {
            res1.into_iter()
                .map(|(v1, toks1)| {
                    let res2 = b()(toks1);
                    if res2.is_empty() {
                        vec![]
                    } else {
                        res2.into_iter()
                            .map(|(v2, toks2)| {
                                let res3 = c()(toks2);
                                if res3.is_empty() {
                                    vec![]
                                } else {
                                    res3.into_iter()
                                        .map(|(v3, toks3)| (comb(&v1, &v2, v3), toks3))
                                        .collect::<Vec<_>>()
                                }
                            })
                            .flatten()
                            .collect()
                    }
                })
                .flatten()
                .collect()
        }
    })
}

pub(crate) fn then4<
    A,
    B,
    C,
    D,
    E,
    F: FnMut(&A, &B, &C, D) -> E + 'static,
    FA: FnOnce() -> Parser<A> + 'static,
    FB: FnMut() -> Parser<B> + 'static,
    FC: FnMut() -> Parser<C> + 'static,
    FD: FnMut() -> Parser<D> + 'static,
>(
    mut comb: F,
    a: FA,
    mut b: FB,
    mut c: FC,
    mut d: FD,
) -> Parser<E> {
    Box::new(move |toks| {
        let res1 = a()(toks);
        if res1.is_empty() {
            vec![]
        } else {
            res1.into_iter()
                .map(|(v1, toks1)| {
                    let res2 = b()(toks1);
                    if res2.is_empty() {
                        vec![]
                    } else {
                        res2.into_iter()
                            .map(|(v2, toks2)| {
                                let res3 = c()(toks2);
                                if res3.is_empty() {
                                    vec![]
                                } else {
                                    res3.into_iter()
                                        .map(|(v3, toks3)| {
                                            let res4 = d()(toks3);
                                            if res4.is_empty() {
                                                vec![]
                                            } else {
                                                res4.into_iter()
                                                    .map(|(v4, toks4)| {
                                                        (comb(&v1, &v2, &v3, v4), toks4)
                                                    })
                                                    .collect::<Vec<_>>()
                                            }
                                        })
                                        .flatten()
                                        .collect()
                                }
                            })
                            .flatten()
                            .collect()
                    }
                })
                .flatten()
                .collect()
        }
    })
}

pub(crate) fn then6<
    A,
    B,
    C,
    D,
    E,
    F_,
    G,
    F: FnMut(&A, &B, &C, &D, &E, F_) -> G + 'static,
    FA: FnOnce() -> Parser<A> + 'static,
    FB: FnMut() -> Parser<B> + 'static,
    FC: FnMut() -> Parser<C> + 'static,
    FD: FnMut() -> Parser<D> + 'static,
    FE: FnMut() -> Parser<E> + 'static,
    FF: FnMut() -> Parser<F_> + 'static,
>(
    mut comb: F,
    a: FA,
    mut b: FB,
    mut c: FC,
    mut d: FD,
    mut e: FE,
    mut f: FF,
) -> Parser<G> {
    Box::new(move |toks| {
        let res1 = a()(toks);
        if res1.is_empty() {
            vec![]
        } else {
            res1.into_iter()
                .map(|(v1, toks1)| {
                    let res2 = b()(toks1);
                    if res2.is_empty() {
                        vec![]
                    } else {
                        res2.into_iter()
                            .map(|(v2, toks2)| {
                                let res3 = c()(toks2);
                                if res3.is_empty() {
                                    vec![]
                                } else {
                                    res3.into_iter()
                                        .map(|(v3, toks3)| {
                                            let res4 = d()(toks3);
                                            if res4.is_empty() {
                                                vec![]
                                            } else {
                                                res4.into_iter()
                                                    .map(|(v4, toks4)| {
                                                        let res5 = e()(toks4);
                                                        if res5.is_empty() {
                                                            vec![]
                                                        } else {
                                                            res5.into_iter()
                                                                .map(|(v5, toks5)| {
                                                                    let res6 = f()(toks5);
                                                                    if res6.is_empty() {
                                                                        vec![]
                                                                    } else {
                                                                        res6.into_iter()
                                                                            .map(|(v6, toks6)| {
                                                                                (
                                                                                    comb(
                                                                                        &v1, &v2,
                                                                                        &v3, &v4,
                                                                                        &v5, v6,
                                                                                    ),
                                                                                    toks6,
                                                                                )
                                                                            })
                                                                            .collect::<Vec<_>>()
                                                                    }
                                                                })
                                                                .flatten()
                                                                .collect()
                                                        }
                                                    })
                                                    .flatten()
                                                    .collect()
                                            }
                                        })
                                        .flatten()
                                        .collect()
                                }
                            })
                            .flatten()
                            .collect()
                    }
                })
                .flatten()
                .collect()
        }
    })
}

pub(crate) fn zero_or_more<A: Clone + 'static, F: FnOnce() -> Parser<A> + 'static + Clone>(
    p: F,
) -> Parser<Vec<A>> {
    alt(move || one_or_more(p), || empty(vec![]))
}

pub(crate) fn one_or_more<A: Clone + 'static, F: FnOnce() -> Parser<A> + 'static + Clone>(
    p: F,
) -> Parser<Vec<A>> {
    then(
        move |a, mut v: Vec<A>| {
            v.insert(0, a.clone());
            v
        },
        p.clone(),
        move || zero_or_more(p.clone()),
    )
}

pub(crate) fn one_or_more_with_sep<
    A: Clone + 'static,
    B: Clone + 'static,
    FA: FnOnce() -> Parser<A> + 'static + Clone,
    FB: FnOnce() -> Parser<B> + 'static + Clone,
>(
    a: FA,
    b: FB,
) -> Parser<Vec<A>> {
    then(
        |x, xs: PartialExpr2<Vec<A>, B>| match xs {
            PartialExpr2::FoundOp(_, mut as_) => {
                as_.insert(0, x.clone());
                as_
            }
            PartialExpr2::NoOp => vec![x.clone()],
        },
        a.clone(),
        move || one_with_sep(a.clone(), b.clone()),
    )
}

fn one_with_sep<
    A: Clone + 'static,
    B: Clone + 'static,
    F: FnOnce() -> Parser<A> + 'static + Clone,
    F_: FnOnce() -> Parser<B> + 'static + Clone,
>(
    a: F,
    sep: F_,
) -> Parser<PartialExpr2<Vec<A>, B>> {
    alt(
        move || {
            then(
                |b, a| PartialExpr2::FoundOp(b.clone(), a),
                sep.clone(),
                move || zero_or_more_with_sep(a.clone(), sep.clone()),
            )
        },
        || empty(PartialExpr2::NoOp),
    )
}

fn zero_or_more_with_sep<
    A: Clone + 'static,
    B: Clone + 'static,
    F: FnOnce() -> Parser<A> + Clone + 'static,
    F_: FnOnce() -> Parser<B> + Clone + 'static,
>(
    f: F,
    sep: F_,
) -> Parser<Vec<A>> {
    alt(move || one_or_more_with_sep(f, sep), || empty(vec![]))
}

// #[derive(Clone)]
enum PartialExpr2<A, B> {
    FoundOp(B, A),
    NoOp,
}
