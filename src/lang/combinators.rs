use crate::lang::parser::{empty, Parser};
use crate::lang::Token;

pub(crate) fn alt<A, F: Fn() -> Parser<A> + 'static, F_: Fn() -> Parser<A> + 'static>(
    a: F,
    b: F_,
) -> Parser<A> {
    Box::new(move |toks: Vec<Token>| {
        let mut res = a()(toks.clone());
        res.append(&mut b()(toks));
        res
    })
}

pub(crate) fn apply<A, B, F: Fn(A) -> B + 'static, F2: Fn() -> Parser<A> + 'static>(
    a: F2,
    f: F,
) -> Parser<B> {
    Box::new(move |toks| {
        let res = a()(toks);
        if res.is_empty() {
            vec![]
        } else {
            res.into_iter().map(|(v, toks2)| (f(v), toks2)).collect()
        }
    })
}

pub(crate) fn then<
    A: Clone,
    B,
    C,
    F: Fn(A, B) -> C + 'static,
    F1: Fn() -> Parser<A> + 'static,
    F2: Fn() -> Parser<B> + 'static,
>(
    comb: F,
    a: F1,
    b: F2,
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
                            .map(|(v2, toks2)| (comb(v1.clone(), v2), toks2))
                            .collect::<Vec<_>>()
                    }
                })
                .flatten()
                .collect()
        }
    })
}

pub(crate) fn then3<
    A: Clone,
    B: Clone,
    C,
    D,
    F: Fn(A, B, C) -> D + 'static,
    FA: Fn() -> Parser<A> + 'static,
    FB: Fn() -> Parser<B> + 'static,
    FC: Fn() -> Parser<C> + 'static,
>(
    comb: F,
    a: FA,
    b: FB,
    c: FC,
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
                                        .map(|(v3, toks3)| {
                                            (comb(v1.clone(), v2.clone(), v3), toks3)
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
}

pub(crate) fn then4<
    A: Clone,
    B: Clone,
    C: Clone,
    D,
    E,
    F: Fn(A, B, C, D) -> E + 'static,
    FA: Fn() -> Parser<A> + 'static,
    FB: Fn() -> Parser<B> + 'static,
    FC: Fn() -> Parser<C> + 'static,
    FD: Fn() -> Parser<D> + 'static,
>(
    comb: F,
    a: FA,
    b: FB,
    c: FC,
    d: FD,
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
                                                        (
                                                            comb(
                                                                v1.clone(),
                                                                v2.clone(),
                                                                v3.clone(),
                                                                v4,
                                                            ),
                                                            toks4,
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
}

pub(crate) fn then6<
    A: Clone,
    B: Clone,
    C: Clone,
    D: Clone,
    E: Clone,
    F_,
    G,
    F: Fn(A, B, C, D, E, F_) -> G + 'static,
    FA: Fn() -> Parser<A> + 'static,
    FB: Fn() -> Parser<B> + 'static,
    FC: Fn() -> Parser<C> + 'static,
    FD: Fn() -> Parser<D> + 'static,
    FE: Fn() -> Parser<E> + 'static,
    FF: Fn() -> Parser<F_> + 'static,
>(
    comb: F,
    a: FA,
    b: FB,
    c: FC,
    d: FD,
    e: FE,
    f: FF,
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
                                                                                        v1.clone(),
                                                                                        v2.clone(),
                                                                                        v3.clone(),
                                                                                        v4.clone(),
                                                                                        v5.clone(),
                                                                                        v6,
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

pub(crate) fn zero_or_more<A: Clone + 'static, F: Fn() -> Parser<A> + 'static + Clone>(
    p: F,
) -> Parser<Vec<A>> {
    alt(move || one_or_more(p.clone()), || empty(vec![]))
}

pub(crate) fn one_or_more<A: Clone + 'static, F: Fn() -> Parser<A> + 'static + Clone>(
    p: F,
) -> Parser<Vec<A>> {
    then(
        |a, mut v: Vec<A>| {
            v.insert(0, a);
            v
        },
        p.clone(),
        move || zero_or_more(p.clone()),
    )
}

pub(crate) fn one_or_more_with_sep<
    A: Clone + 'static,
    B: Clone + 'static,
    FA: Fn() -> Parser<A> + 'static + Clone,
    FB: Fn() -> Parser<B> + 'static + Clone,
>(
    a: FA,
    b: FB,
) -> Parser<Vec<A>> {
    then(
        |x, xs: PartialExpr2<Vec<A>, B>| match xs {
            PartialExpr2::FoundOp(_, mut as_) => {
                as_.insert(0, x);
                as_
            }
            PartialExpr2::NoOp => {
                vec![x]
            }
        },
        a.clone(),
        move || one_with_sep(a.clone(), b.clone()),
    )
}

fn one_with_sep<
    A: Clone + 'static,
    B: Clone + 'static,
    F: Fn() -> Parser<A> + 'static + Clone,
    F_: Fn() -> Parser<B> + 'static + Clone,
>(
    a: F,
    sep: F_,
) -> Parser<PartialExpr2<Vec<A>, B>> {
    alt(
        move || {
            let a_ = a.clone();
            let sep_ = sep.clone();
            then(PartialExpr2::FoundOp, sep.clone(), move || {
                zero_or_more_with_sep(a_.clone(), sep_.clone())
            })
        },
        || empty(PartialExpr2::NoOp),
    )
}

fn zero_or_more_with_sep<
    A: Clone + 'static,
    B: Clone + 'static,
    F: Fn() -> Parser<A> + Clone + 'static,
    F_: Fn() -> Parser<B> + Clone + 'static,
>(
    f: F,
    sep: F_,
) -> Parser<Vec<A>> {
    alt(
        move || one_or_more_with_sep(f.clone(), sep.clone()),
        || empty(vec![]),
    )
}

#[derive(Clone)]
enum PartialExpr2<A, B> {
    FoundOp(B, A),
    NoOp,
}
