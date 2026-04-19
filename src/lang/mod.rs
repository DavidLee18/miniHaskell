use crate::lang::parser::Parser;
use std::fs::read_to_string;
use std::path::PathBuf;
use unicode_segmentation::UnicodeSegmentation;

pub(crate) mod combinators;
pub(crate) mod parser;

pub(crate) type Name = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<A> {
    Var(Name),
    Num(i64),
    Constr {
        tag: u32,
        arity: u32,
    },
    Ap(Box<Expr<A>>, Box<Expr<A>>),
    Let {
        is_rec: bool,
        defns: Vec<(A, Expr<A>)>,
        body: Box<Expr<A>>,
    },
    Case(Box<Expr<A>>, Vec<Alter<A>>),
    Lam(Vec<A>, Box<Expr<A>>),
}

pub type Alter<A> = (u32, Vec<A>, Expr<A>);
pub type ScDefn<A> = (Name, Vec<A>, Expr<A>);
pub type Program<A> = Vec<ScDefn<A>>;
pub(crate) type CoreExpr = Expr<Name>;
pub type CoreAlt = Alter<Name>;
pub(crate) type CoreProgram = Program<Name>;
pub(crate) type CoreScDefn = ScDefn<Name>;

pub enum Associativity {
    Left,
    Right,
    None,
}

impl<A> Expr<A> {
    pub fn is_atomic(&self) -> bool {
        match self {
            Expr::Var(_) | Expr::Num(_) => true,
            _ => false,
        }
    }
    pub fn is_let(&self) -> bool {
        match self {
            Expr::Let { .. } => true,
            Expr::Ap(a, b) => a.is_let() || b.is_let(),
            _ => false,
        }
    }

    pub fn get_var_mut(&mut self, var_name: &str) -> Option<&mut Expr<A>> {
        match self {
            Expr::Var(n) if n == var_name => Some(self),
            Expr::Var(_) => None,
            Expr::Ap(left, right) => left
                .get_var_mut(var_name)
                .or_else(|| right.get_var_mut(var_name)),
            _ => None,
        }
    }

    fn get_precedence(&self) -> u32 {
        match self {
            Expr::Var(n) => match n.as_str() {
                "|" => 1,
                "&" => 2,
                "==" | "~=" | ">" | ">=" | "<" | "<=" => 3,
                "+" | "-" => 4,
                "*" | "/" => 5,
                _ => 6,
            },
            Expr::Num(_) | Expr::Constr { .. } => 7,
            Expr::Ap(a, _) => a.get_precedence(),
            Expr::Let { .. } | Expr::Case(_, _) | Expr::Lam(_, _) => 0,
        }
    }
}

impl<A: std::fmt::Display> Expr<A> {
    fn pprint(&self) -> ISeq {
        match self {
            Expr::Num(n) => ISeq::Str(n.to_string()),
            Expr::Var(v) => ISeq::Str(v.clone()),
            Expr::Ap(a, b) => match &**a {
                Expr::Ap(c, d) => match &**c {
                    Expr::Var(n)
                        if !n.chars().any(char::is_alphanumeric)
                            && c.get_precedence() > b.get_precedence()
                            && c.get_precedence() > d.get_precedence() =>
                    {
                        i_concat(vec![
                            ISeq::Str(String::from("(")),
                            d.pprint(),
                            ISeq::Str(format!(") {} (", n)),
                            b.pprint(),
                            ISeq::Str(String::from(")")),
                        ])
                    }
                    Expr::Var(n)
                        if !n.chars().any(char::is_alphanumeric)
                            && c.get_precedence() > b.get_precedence() =>
                    {
                        i_concat(vec![
                            d.pprint(),
                            ISeq::Str(format!(" {} (", n)),
                            b.pprint(),
                            ISeq::Str(String::from(")")),
                        ])
                    }
                    Expr::Var(n)
                        if !n.chars().any(char::is_alphanumeric)
                            && c.get_precedence() > d.get_precedence() =>
                    {
                        i_concat(vec![
                            ISeq::Str(String::from("(")),
                            d.pprint(),
                            ISeq::Str(format!(") {} ", n)),
                            b.pprint(),
                        ])
                    }
                    Expr::Var(n) if !n.chars().any(char::is_alphanumeric) => {
                        i_concat(vec![d.pprint(), ISeq::Str(format!(" {} ", n)), b.pprint()])
                    }
                    _ if c.get_precedence() > b.get_precedence()
                        && c.get_precedence() > d.get_precedence() =>
                    {
                        i_concat(vec![
                            c.pprint(),
                            ISeq::Str(String::from(" (")),
                            d.pprint(),
                            ISeq::Str(String::from(") (")),
                            b.pprint(),
                            ISeq::Str(String::from(")")),
                        ])
                    }
                    _ if c.get_precedence() > b.get_precedence() => i_concat(vec![
                        c.pprint(),
                        ISeq::Str(String::from(" ")),
                        d.pprint(),
                        ISeq::Str(String::from(" (")),
                        b.pprint(),
                        ISeq::Str(String::from(")")),
                    ]),
                    _ if c.get_precedence() > d.get_precedence() => i_concat(vec![
                        c.pprint(),
                        ISeq::Str(String::from(" (")),
                        d.pprint(),
                        ISeq::Str(String::from(") ")),
                        b.pprint(),
                    ]),
                    _ => i_concat(vec![
                        c.pprint(),
                        ISeq::Str(String::from(" ")),
                        d.pprint(),
                        ISeq::Str(String::from(" ")),
                        b.pprint(),
                    ]),
                },
                _ if a.get_precedence() > b.get_precedence() => i_concat(vec![
                    a.pprint(),
                    ISeq::Str(String::from(" (")),
                    b.pprint(),
                    ISeq::Str(String::from(")")),
                ]),
                _ => i_concat(vec![a.pprint(), ISeq::Str(String::from(" ")), b.pprint()]),
            },
            Expr::Let {
                is_rec,
                defns,
                body,
            } => i_concat(vec![
                ISeq::Str(String::from(if *is_rec { "letrec" } else { "let" })),
                ISeq::Indent(Box::new(ISeq::Newline)),
                Self::pprint_defns(defns),
                ISeq::Newline,
                ISeq::Str(String::from("in ")),
                ISeq::Indent(Box::new(ISeq::Newline)),
                body.pprint(),
            ]),
            Expr::Case(pat, alts) => i_concat(vec![
                ISeq::Str(String::from("case ")),
                pat.pprint(),
                ISeq::Str(String::from(" of ")),
                ISeq::Indent(Box::new(ISeq::Newline)),
                Self::pprint_alts(alts),
            ]),
            Expr::Lam(params, expr) => i_concat(vec![
                ISeq::Str(String::from("\\")),
                i_interleave(
                    ISeq::Str(String::from(" ")),
                    params.iter().map(|p| ISeq::Str(p.to_string())),
                ),
                ISeq::Str(String::from(" -> ")),
                ISeq::Indent(Box::new(ISeq::Newline)),
                expr.pprint(),
            ]),
            Expr::Constr { tag, arity } => ISeq::Str(format!("Pack{{{}, {}}}", tag, arity)),
        }
    }

    fn pprint_defns(defns: &[(A, Expr<A>)]) -> ISeq {
        i_interleave(
            i_concat(vec![ISeq::Str(String::from(";")), ISeq::Newline]),
            defns.iter().map(Self::pprint_defn),
        )
    }

    fn pprint_defn(defn: &(A, Expr<A>)) -> ISeq {
        let (name, expr) = defn;
        i_concat(vec![
            ISeq::Str(name.to_string()),
            ISeq::Str(String::from(" = ")),
            ISeq::Indent(Box::new(expr.pprint())),
            ISeq::Newline,
        ])
    }

    fn pprint_alts(alts: &[Alter<A>]) -> ISeq {
        i_interleave(ISeq::Newline, alts.iter().map(Self::pprint_alt))
    }

    fn pprint_alt(alts: &Alter<A>) -> ISeq {
        let (tag, params, expr) = alts;
        i_concat(vec![
            ISeq::Str(format!(
                "<{}>{}",
                tag,
                if params.is_empty() { "" } else { " " }
            )),
            i_interleave(
                ISeq::Str(String::from(" ")),
                params.iter().map(|p| ISeq::Str(p.to_string())),
            ),
            ISeq::Str(String::from(" -> ")),
            expr.pprint(),
        ])
    }
}

/// a simple program in heap lang.
pub const SIMPLE_PROGRAM: &'static str = r#"
    main = double 21;
    double x = x + x
"#;

pub const PRELUDE_DEFS: &'static str = r#"
    I x = x;
    K x y = x;
    K1 x y = y;
    S f g x = f x (g x);
    compose f g x = f (g x);
    twice f = compose f f
"#;

impl<A: std::fmt::Display> std::fmt::Display for Expr<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.pprint())
    }
}

#[derive(Debug, Clone)]
pub enum ISeq {
    Nil,
    Str(String),
    Append(Box<ISeq>, Box<ISeq>),
    Indent(Box<ISeq>),
    Newline,
}

pub fn i_concat<F: IntoIterator<Item = ISeq>>(it: F) -> ISeq {
    it.into_iter()
        .fold(ISeq::Nil, |a, b| ISeq::Append(Box::new(a), Box::new(b)))
}

pub fn i_interleave<F: IntoIterator<Item = ISeq>>(s: ISeq, ss: F) -> ISeq {
    ss.into_iter().fold(ISeq::Nil, |a, b| match (a, b) {
        (ISeq::Nil, b) => b,
        (a, ISeq::Nil) => a,
        (a, b) => ISeq::Append(
            Box::new(ISeq::Append(Box::new(a), Box::new(s.clone()))),
            Box::new(b),
        ),
    })
}

pub fn pprint_prog<A: std::fmt::Display>(p: Program<A>) -> ISeq {
    i_interleave(
        i_concat(vec![ISeq::Str(String::from(";")), ISeq::Newline]),
        p.into_iter().map(pprint_scdefn),
    )
}

pub fn pprint_scdefn<A: std::fmt::Display>(sc: ScDefn<A>) -> ISeq {
    let (name, params, expr) = sc;
    i_concat(vec![
        ISeq::Str(format!("{} ", name)),
        i_interleave(
            ISeq::Str(String::from(" ")),
            params.iter().map(|p| ISeq::Str(p.to_string())),
        ),
        ISeq::Str(String::from(" = ")),
        ISeq::Indent(Box::new(ISeq::Newline)),
        expr.pprint(),
    ])
}

impl std::fmt::Display for ISeq {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", i_flatten(0, vec![(self, 0)]))
    }
}

fn i_flatten(curr_col: u32, isep_reps: Vec<(&ISeq, u32)>) -> String {
    match isep_reps.split_first() {
        None => String::from(""),
        Some(((ISeq::Nil, _), seqs)) => i_flatten(curr_col, seqs.to_vec()),
        Some(((ISeq::Newline, indent), seqs)) => {
            let mut res = String::from(" ").repeat(*indent as usize);
            res.insert(0, '\n');
            res.push_str(&i_flatten(*indent, seqs.to_vec()));
            res
        }
        Some(((ISeq::Indent(seq), _), seqs)) => {
            let mut res = seqs.to_vec();
            res.insert(0, (seq, curr_col));
            i_flatten(curr_col, res)
        }
        Some(((ISeq::Str(s), _), seqs)) => format!(
            "{}{}",
            s,
            i_flatten(curr_col + s.len() as u32, seqs.to_vec())
        ),
        Some(((ISeq::Append(a, b), indent), seqs)) => {
            let mut res = seqs.to_vec();
            res.insert(0, (b, *indent));
            res.insert(0, (a, *indent));
            i_flatten(curr_col, res)
        }
    }
}

type Token = ((u32, u32), String);

pub(crate) fn clex(input: String) -> Vec<Token> {
    todo!()
    // let mut tokens: Vec<((u32, u32), String)> = Vec::new();
    // let mut chars = input.grapheme_indices(true).peekable();
    // let mut line = 0;
    // let mut last_idx = 0;
    // while let Some((i, c)) = chars.peek() {
    //     match *c {
    //         "|" => {
    //             let _ = chars.next();
    //             let c_next = chars.next();
    //             match c_next {
    //                 Some((_, "|")) => {
    //                     while let Some((k, s)) = chars.next()
    //                         && s.contains('\n')
    //                     {
    //                         last_idx = k;
    //                         line += 1;
    //                     }
    //                 }
    //                 Some((j, _)) => tokens.push(((line, j as u32), String::from("|"))),
    //                 None => break,
    //             }
    //         }
    //         c__ if TWO_CHAR_OPS.iter().find(|&&s| s.starts_with(c__)).is_some() => {
    //             match (c__, chars.peek()) {
    //                 ("=", Some((_, "="))) => {
    //                     tokens.push(((line, *i as u32), String::from("==")));
    //                     chars.next();
    //                 }
    //                 ("~", Some((_, "="))) => {
    //                     tokens.push(((line, *i as u32), String::from("~=")));
    //                     chars.next();
    //                 }
    //                 (">", Some((_, "="))) => {
    //                     tokens.push(((line, *i as u32), String::from(">=")));
    //                     chars.next();
    //                 }
    //                 ("<", Some((_, "="))) => {
    //                     tokens.push(((line, *i as u32), String::from("<=")));
    //                     chars.next();
    //                 }
    //                 ("-", Some((_, ">"))) => tokens.push(((line, *i as u32), String::from("->"))),
    //                 (_, None) => tokens.push(((line, *i as u32), String::from(c__))),
    //                 (_, Some((_, _))) => {
    //                     tokens.push(((line, *i as u32), String::from(c__)));
    //                     let _ = chars.next_back();
    //                 }
    //             }
    //         }
    //         c__ if c__ == "\n" => {
    //             line += 1;
    //         }
    //         c__ if c__.chars().all(|c| c.is_digit(10)) => {
    //             let mut rest = chars
    //                 .clone()
    //                 .take_while(|c| c.1.chars().all(|c| c.is_digit(10)))
    //                 .map(|c| c.1)
    //                 .collect::<String>();
    //             rest.insert(0, c__);
    //             tokens.push(((line, *i as u32), rest));
    //             while let Some(c_) = chars.next()
    //                 && c_.is_digit(10)
    //             {}
    //             let _ = chars.next_back();
    //         }
    //         c__ if c__.is_alphabetic() => {
    //             let mut rest = chars
    //                 .clone()
    //                 .take_while(|c| c.is_alphanumeric() || *c == '_')
    //                 .collect::<String>();
    //             rest.insert(0, c__);
    //             tokens.push((line, rest));
    //             while let Some(c_) = chars.next()
    //                 && (c_.is_alphanumeric() || c_ == '_')
    //             {}
    //             let _ = chars.next_back();
    //         }
    //         c__ => tokens.push((line, String::from(c__))),
    //     }
    // }
    // tokens
}

const TWO_CHAR_OPS: [&'static str; 5] = ["==", "~=", ">=", "<=", "->"];

pub(crate) fn parse_with<A>(parser: Parser<A>, tokens: Vec<Token>) -> Result<Vec<A>, SyntaxError> {
    Ok(parser(tokens)
        .into_iter()
        .filter(|(_, v)| v.is_empty())
        .map(|(p, _)| p)
        .collect())
}

pub(crate) fn syntax(tokens: Vec<Token>) -> Result<Vec<CoreProgram>, SyntaxError> {
    parse_with(parser::program(), tokens)
}

#[derive(Debug, Clone)]
enum PartialExpr {
    NoOp,
    FoundOp(Name, CoreExpr),
}

pub fn parse(p: PathBuf) -> Result<Vec<CoreProgram>, SyntaxError> {
    let s = read_to_string(p).expect("Failed to read file");
    syntax(clex(s))
}

pub fn parse_raw(s: String) -> Result<Vec<CoreProgram>, SyntaxError> {
    let tokens = clex(s);
    syntax(tokens)
}

#[derive(Debug, thiserror::Error)]
#[error("syntax error: {0:?}")]
pub struct SyntaxError(pub Token);
