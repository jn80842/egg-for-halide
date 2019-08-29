use egg::{
    egraph::EGraph,
    expr::{Expr, Language, Name, QuestionMarkName},
    parse::ParsableLanguage,
    pattern::Rewrite,
};
use log::*;
use strum_macros::{Display, EnumString};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct HalideExpr;

#[derive(Debug, PartialEq, Eq, Hash, Clone, EnumString, Display)]
enum Op {
  #[strum(serialize = "+")]
  Add,
  #[strum(serialize = "-")]
  Sub,
  #[strum(serialize = "*")]
  Mul,
  #[strum(serialize = "/")]
  Div,
  #[strum(serialize = "<")]
  Lt,
  #[strum(serialize = ">")]
  Gt,
  #[strum(serialize = "%")]
  Mod,
  #[strum(serialize = ">=")]
  Ge,
  #[strum(serialize = "<=")]
  Le,
  #[strum(serialize = "==")]
  Eqv,
  #[strum(serialize = "!=")]
  Neq,
  #[strum(serialize = "not")]
  Not,
  #[strum(serialize = "||")]
  Or,
  #[strum(serialize = "&&")]
  And,
  #[strum(serialize = "max")]
  Max,
  #[strum(serialize = "min")]
  Min,
  #[strum(serialize = "select")]
  Select,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, EnumString, Display)]
enum Bool {
    #[strum(serialize = "T")]
    True,
    #[strum(serialize = "F")]
    False,
    #[strum(serialize = "1")]
    One, // this is janky as hell but there's no semantics here! these are just arbitrary symbols!
}

impl Language for HalideExpr {
    type Constant = Bool; // for right now the only kind of constants are bools
    type Operator = Op;
    type Variable = Name;
    type Wildcard = QuestionMarkName;

    fn cost(_node: &Expr<HalideExpr, u64>) -> u64 {
        unimplemented!()
    }

    fn eval(_op: Self::Operator, _args: &[Self::Constant]) -> Self::Constant {
        unimplemented!()
    }
}

macro_rules! rule {
    ($name:ident, $left:expr, $right:expr) => {
        #[allow(dead_code)]
        fn $name() -> Rewrite<HalideExpr> {
            trace!(
                "Building rule {} ==> {}",
                stringify!($left),
                stringify!($right)
            );
            HalideExpr.parse_rewrite(stringify!($name), $left, $right)
                .unwrap()
        }
    };
}

rule! {andrule0, "(&& ?x T)", "?x"}
rule! {andrule1, "(&& ?x F)", "F"}
rule! {andrule2, "(&& ?x ?x)", "?x"}
rule! {andrule4, "(&& (&& ?x ?y) ?x)", "(&& ?x ?y)"}
rule! {andrule5, "(&& ?x (&& ?x ?y))", "(&& ?x ?y)"}
rule! {andrule6, "(&& (&& ?x ?y) ?y)", "(&& ?x ?y)"}
rule! {andrule7, "(&& ?y (&& ?x ?y))", "(&& ?x ?y)"}
rule! {andrule9, "(&& (&& (&& ?x ?y) ?z) ?x)", "(&& (&& ?x ?y) ?z)"}
rule! {andrule10, "(&& ?x (&& (&& ?x ?y) ?z))", "(&& (&& ?x ?y) ?z)"}
rule! {andrule11, "(&& (&& ?z (&& ?x ?y)) ?x)", "(&& ?z (&& ?x ?y))"}
rule! {andrule12, "(&& ?x (&& ?z (&& ?x ?y)))", "(&& ?z (&& ?x ?y))"}
rule! {andrule13, "(&& (&& (&& ?x ?y) ?z) ?y)", "(&& (&& ?x ?y) ?z)"}
rule! {andrule14, "(&& ?y (&& (&& ?x ?y) ?z))", "(&& (&& ?x ?y) ?z)"}
rule! {andrule15, "(&& (&& ?z (&& ?x ?y)) ?y)", "(&& ?z (&& ?x ?y))"}
rule! {andrule16, "(&& ?y (&& ?z (&& ?x ?y)))", "(&& ?z (&& ?x ?y))"}
rule! {andrule18, "(&& (|| ?x ?y) ?x)", "?x"}
rule! {andrule19, "(&& ?x (|| ?x ?y))", "?x"}
rule! {andrule20, "(&& (|| ?x ?y) ?y)", "?y"}
rule! {andrule21, "(&& ?y (|| ?x ?y))", "?y"}
rule! {andrule23, "(!= ?x (= (&& ?y ?x) ?y))", "F"}
rule! {andrule24, "(!= ?x (= (&& ?y ?y) ?x))", "F"}
rule! {andrule25, "(= (&& (!= (&& ?z ?x) ?y) ?x) ?y)", "F"}
rule! {andrule26, "(= (&& (!= (&& ?z ?x) ?y) ?y) ?x)", "F"}
rule! {andrule27, "(= (&& (!= ?x (&& ?y ?z)) ?x) ?y)", "F"}
rule! {andrule28, "(= (&& (!= ?x (&& ?y ?z)) ?y) ?x)", "F"}
rule! {andrule29, "(!= (&& (= (&& ?z ?x) ?y) ?x) ?y)", "F"}
rule! {andrule30, "(!= (&& (= (&& ?z ?x) ?y) ?y) ?x)", "F"}
rule! {andrule31, "(!= (&& (= ?x (&& ?y ?z)) ?x) ?y)", "F"}
rule! {andrule32, "(!= (&& (= ?x (&& ?y ?z)) ?y) ?x)", "F"}
rule! {andrule33, "(&& ?x (not ?x))", "F"}
rule! {andrule34, "(&& (not ?x) ?x)", "F"}
rule! {andrule35, "(< (<= ?y (&& ?x ?x)) ?y)", "F"}
rule! {andrule44, "(< (< ?c0 (&& ?x ?c1)) ?x)", "(< (max ?c0 ?c1) ?x)"}
rule! {andrule45, "(<= (<= ?c0 (&& ?x ?c1)) ?x)", "(<= (max ?c0 ?c1) ?x)"}
rule! {andrule46, "(< (< ?x (&& ?c0 ?x)) ?c1)", "(< ?x (min ?c0 ?c1))"}
rule! {andrule47, "(<= (<= ?x (&& ?c0 ?x)) ?c1)", "(<= ?x (min ?c0 ?c1))"}
rule! {andrule51, "(&& (|| ?x (&& ?y ?z)) ?y)", "(&& (|| ?x ?z) ?y)"}
rule! {andrule52, "(&& (|| ?x (&& ?z ?y)) ?y)", "(&& (|| ?x ?z) ?y)"}
rule! {andrule53, "(&& ?y (|| ?x (&& ?y ?z)))", "(&& ?y (|| ?x ?z))"}
rule! {andrule54, "(&& ?y (|| ?x (&& ?z ?y)))", "(&& ?y (|| ?x ?z))"}
rule! {andrule56, "(&& (|| (&& ?y ?z) ?x) ?y)", "(&& (|| ?z ?x) ?y)"}
rule! {andrule57, "(&& (|| (&& ?z ?y) ?x) ?y)", "(&& (|| ?z ?x) ?y)"}
rule! {andrule58, "(&& ?y (|| (&& ?y ?z) ?x))", "(&& ?y (|| ?z ?x))"}
rule! {andrule59, "(&& ?y (|| (&& ?z ?y) ?x))", "(&& ?y (|| ?z ?x))"}
rule! {andrule61, "(&& (&& ?x (|| ?y ?z)) ?y)", "(&& ?x ?y)"}
rule! {andrule62, "(&& (&& ?x (|| ?z ?y)) ?y)", "(&& ?x ?y)"}
rule! {andrule63, "(&& ?y (&& ?x (|| ?y ?z)))", "(&& ?y ?x)"}
rule! {andrule64, "(&& ?y (&& ?x (|| ?z ?y)))", "(&& ?y ?x)"}
rule! {andrule66, "(&& (&& (|| ?y ?z) ?x) ?y)", "(&& ?x ?y)"}
rule! {andrule67, "(&& (&& (|| ?z ?y) ?x) ?y)", "(&& ?x ?y)"}
rule! {andrule68, "(&& ?y (&& (|| ?y ?z) ?x))", "(&& ?y ?x)"}
rule! {andrule69, "(&& ?y (&& (|| ?z ?y) ?x))", "(&& ?y ?x)"}
rule! {andrule71, "(&& (|| ?x ?y) (|| ?x ?z))", "(|| ?x (&& ?y ?z))"}
rule! {andrule72, "(&& (|| ?x ?y) (|| ?z ?x))", "(|| ?x (&& ?y ?z))"}
rule! {andrule73, "(&& (|| ?y ?x) (|| ?x ?z))", "(|| ?x (&& ?y ?z))"}
rule! {andrule74, "(&& (|| ?y ?x) (|| ?z ?x))", "(|| ?x (&& ?y ?z))"}
rule! {andrule76, "(< (< ?x (&& ?y ?x)) ?z)", "(< ?x (min ?y ?z))"}
rule! {andrule77, "(< (< ?y (&& ?x ?z)) ?x)", "(< (max ?y ?z) ?x)"}
rule! {andrule78, "(<= (<= ?x (&& ?y ?x)) ?z)", "(<= ?x (min ?y ?z))"}
rule! {andrule79, "(<= (<= ?y (&& ?x ?z)) ?x)", "(<= (max ?y ?z) ?x)"}

