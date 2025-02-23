use super::*;
use crate::compiler::EvalError;
use crate::lang::Expr;

#[test]
fn let_case() {
    let res = lang::syntax(lang::clex(String::from(
        "f = 3 ; g x y = let z = x in z ; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5",
    )))
        .expect("expected to be parsed");

    assert_eq!(res.len(), 1);

    assert_eq!(res[0][0].0, "f");
    assert!(res[0][0].1.is_empty());
    assert_eq!(res[0][0].2, Expr::Num(3));

    assert_eq!(res[0][1].0, "g");
    assert_eq!(res[0][1].1[0], "x");
    assert_eq!(res[0][1].1[1], "y");
    match &res[0][1].2 {
        Expr::Let { defs, body } => {
            assert_eq!(defs[0].0, "z");
            assert_eq!(defs[0].1, Expr::Var("x".to_string()));
            assert_eq!(**body, Expr::Var("z".to_string()));
        }
        _ => panic!("expected let"),
    }

    assert_eq!(res[0][2].0, "h");
    assert_eq!(res[0][2].1[0], "x");
    match &res[0][2].2 {
        Expr::Case(be, als) => match &**be {
            Expr::Let { defs, body } => {
                assert_eq!(defs[0].0, "y");
                assert_eq!(defs[0].1, Expr::Var("x".to_string()));
                assert_eq!(**body, Expr::Var("y".to_string()));
                assert_eq!(als[0], (1, vec![], Expr::Num(2)));
                assert_eq!(als[1], (2, vec![], Expr::Num(5)));
            }
            _ => panic!("expected let"),
        },
        _ => panic!("expected case"),
    }
}

#[test]
fn dangling_else() {
    let res = lang::syntax(lang::clex(String::from(
        "f x y = case x of <1> -> case y of <1> -> 1; <2> -> 2",
    )))
        .expect("expected to be parsed");

    assert_eq!(res.len(), 2);

    assert_eq!(res[0][0].0, "f");
    assert_eq!(res[0][0].1[0], "x");
    assert_eq!(res[0][0].1[1], "y");
    match &res[0][0].2 {
        Expr::Case(be, als) => {
            assert_eq!(**be, Expr::Var("x".to_string()));
            assert_eq!(als[0].0, 1);
            assert!(als[0].1.is_empty());
            assert_eq!(als.len(), 1);
            match &als[0].2 {
                Expr::Case(y, als2) => {
                    assert_eq!(**y, Expr::Var("y".to_string()));
                    assert_eq!(als2[0], (1, vec![], Expr::Num(1)));
                    assert_eq!(als2[1], (2, vec![], Expr::Num(2)));
                    assert_eq!(als2.len(), 2);
                }
                _ => panic!("expected case"),
            }
        }
        _ => panic!("expected case"),
    }

    assert_eq!(res[1][0].0, "f");
    assert_eq!(res[1][0].1[0], "x");
    assert_eq!(res[1][0].1[1], "y");
    match &res[1][0].2 {
        Expr::Case(be, als) => {
            assert_eq!(**be, Expr::Var("x".to_string()));
            assert_eq!(als[0].0, 1);
            assert!(als[0].1.is_empty());
            assert_eq!(als.len(), 2);
            match &als[0].2 {
                Expr::Case(y, als2) => {
                    assert_eq!(**y, Expr::Var("y".to_string()));
                    assert_eq!(als2[0], (1, vec![], Expr::Num(1)));
                    assert_eq!(als2.len(), 1);
                }
                _ => panic!("expected case"),
            }
            assert_eq!(als[1], (2, vec![], Expr::Num(2)));
        }
        _ => panic!("expected case"),
    }
}

#[test]
fn i3() {
    let res = run(String::from("main = I 3"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn skk3() {
    let res = run(String::from("main = S K K 3"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn id_skk3() {
    let res = run(String::from("id = S K K; main = id 3"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn twice3_id_skk3() {
    let res = run(String::from("id = S K K; main = twice twice twice id 3"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn let_iii2() {
    let res = run(String::from("main = let id1 = I I I in id1 id1 3"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn nested_let() {
    let res = run(String::from(
        "oct g x = let h = twice g in let k = twice h in k (k x); main = oct I 4",
    ));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(4));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn letrec() {
    let res = run(String::from("pair x y f = f x y; fst p = p K; snd p = p K1; f x y = let a = pair x b; b = pair y a in fst (snd (snd (snd a))); main = f 3 4"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(4));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn negate() {
    let res = run(String::from("main = negate 3"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(-3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn negate_ind() {
    let res = run(String::from("main = negate (I 3)"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(-3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn simple_arithmetic() {
    let res = run(String::from("main = 4*5+(2-5)"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 2);
            match &v[0] {
                Err(ResultError::Eval(EvalError::NumAp)) => (),
                _ => panic!("expected to fail"),
            }
            match &v[1] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(17));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn logical_operations() {
    let res = run(String::from("main = xor False True"));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Data(2, vec![]));
                }
                _ => panic!("expected a Data Node"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn factorial() {
    let res = run(String::from(
        r#"
        fac n = if (n == 0) 1 (n * fac (n-1));
        main = fac 5
    "#,
    ));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 2);
            match &v[0] {
                Err(ResultError::Eval(EvalError::NumAp)) => (),
                _ => panic!("expected to fail"),
            }
            match &v[1] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(120));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn gcd() {
    let res = run(String::from(
        r#"
        gcd a b = if (a == b) a (if (a < b) (gcd b a) (gcd b (a-b)));
        main = gcd 6 10
    "#,
    ));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(2));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn fibonacci() {
    let res = run(String::from(
        r#"
            fib n = if (n <= 1) n (fib (n-1) + fib (n-2));
            main = fib 4
        "#,
    ));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 4);
            for i in 0..3 {
                match &v[i] {
                    Err(ResultError::Eval(EvalError::NumAp)) => (),
                    _ => panic!("expected to fail"),
                }
            }
            match &v[3] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(3));
                }
                _ => panic!("expected a number"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn pair() {
    let res = run(String::from(
        r#"
            main = fst (snd (fst (Pair (Pair 1 (Pair 2 3)) 4)))
        "#,
    ));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(2));
                }
                _ => panic!("expected to be evaluated"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}

#[test]
fn head_and_tail() {
    let res = run(String::from(
        r#"
            main = head (tail (tail (Cons 3 (Cons 2 (Cons 1 Nil)))))
        "#,
    ));
    match res {
        Ok(v) => {
            assert_eq!(v.len(), 1);
            match &v[0] {
                Ok((ns, _)) => {
                    assert_eq!(ns.len(), 1);
                    assert_eq!(ns[0], Node::Num(1));
                }
                _ => panic!("expected to be evaluated"),
            }
        }
        _ => panic!("expected to be evaluated"),
    }
}
