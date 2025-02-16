use super::*;
use crate::lang::Expr;

#[test]
fn let_case() {
    let res = lang::syntax(lang::clex(String::from(
        "f = 3 ; g x y = let z = x in z ; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5",
    )));

    assert_eq!(res[0].0, "f");
    assert!(res[0].1.is_empty());
    assert_eq!(res[0].2, Expr::Num(3));

    assert_eq!(res[1].0, "g");
    assert_eq!(res[1].1[0], "x");
    assert_eq!(res[1].1[1], "y");
    match &res[1].2 {
        Expr::Let { is_rec, defs, body } => {
            assert!(!is_rec);
            assert_eq!(defs[0].0, "z");
            assert_eq!(defs[0].1, Expr::Var("x".to_string()));
            assert_eq!(**body, Expr::Var("z".to_string()));
        }
        _ => panic!("expected let"),
    }

    assert_eq!(res[2].0, "h");
    assert_eq!(res[2].1[0], "x");
    match &res[2].2 {
        Expr::Case(be, als) => match &**be {
            Expr::Let { is_rec, defs, body } => {
                assert!(!is_rec);
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
    )));

    assert_eq!(res[0].0, "f");
    assert_eq!(res[0].1[0], "x");
    assert_eq!(res[0].1[1], "y");
    match &res[0].2 {
        Expr::Case(be, als) => {
            assert_eq!(**be, Expr::Var("x".to_string()));
            assert_eq!(als[0].0, 1);
            assert!(als[0].1.is_empty());
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
}

#[test]
fn i3() {
    let res = compiler::eval(compiler::compile(lang::parse_raw(String::from(
        "main = I 3",
    ))));
    let res_stack = compiler::get_stack_results(res.last().expect("Empty states"));
    assert_eq!(res_stack, vec![compiler::Node::Num(3)])
}

#[test]
fn skk3() {
    let res = compiler::eval(compiler::compile(lang::parse_raw(String::from(
        "main = S K K 3",
    ))));
    let res_stack = compiler::get_stack_results(res.last().expect("Empty states"));
    assert_eq!(res_stack, vec![compiler::Node::Num(3)])
}

#[test]
fn id_skk3() {
    let res = compiler::eval(compiler::compile(lang::parse_raw(String::from(
        "id = S K K; main = id 3",
    ))));
    let res_stack = compiler::get_stack_results(res.last().expect("Empty states"));
    assert_eq!(res_stack, vec![compiler::Node::Num(3)])
}

#[test]
fn twice3_id_skk3() {
    let res = compiler::eval(compiler::compile(lang::parse_raw(String::from(
        "id = S K K; main = id 3",
    ))));
    let res_stack = compiler::get_stack_results(res.last().expect("Empty states"));
    assert_eq!(res_stack, vec![compiler::Node::Num(3)])
}