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
  #[strum(serialize = "=")]
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
enum ConstantSymbols {
    #[strum(serialize = "T")]
    True,
    #[strum(serialize = "F")]
    False,
    #[strum(serialize = "1")]
    One,
    #[strum(serialize = "0")]
    Zero,
}

impl Language for HalideExpr {
    type Constant = ConstantSymbols;
    type Operator = Op;
    type Variable = Name;
    type Wildcard = QuestionMarkName;

    fn cost(_node: &Expr<HalideExpr, u64>) -> u64 {
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

macro_rules! conditional_rule {
  ($name:ident, $left:expr, $right:expr, $p:expr, $q:expr) => {
    #[allow(dead_code)]
    fn $name() -> Rewrite<HalideExpr> {
      trace!(
        "Building conditional rule {} ==> {} if {} implies {}",
        stringify!($left),
        stringify!($right),
        stringify!($p),
        stringify!($q)
      );
      HalideExpr.parse_conditional_rewrite(stringify!($name), $left, $right, $p, $q)
        .unwrap()
    }
  };
}

rule! {addrule39, "(+ ?x 0)", "?x"}
rule! {addrule45, "(+ ?x ?x)", "(* ?x 2)"}
rule! {addrule49, "(+ (select ?x ?y ?z) (select ?x ?w ?u))", "(select ?x (+ ?y ?w) (+ ?z ?u))"}
rule! {addrule50, "(+ (select ?x ?c0 ?c1) ?c2)", "(select ?x (+ ?c0 ?c2) (+ ?c1 ?c2))"}
rule! {addrule51, "(+ (select ?x ?y ?c1) ?c2)", "(select ?x (+ ?y ?c2) (+ ?c1 ?c2))"}
rule! {addrule52, "(+ (select ?x ?c0 ?y) ?c2)", "(select ?x (+ ?c0 ?c2) (+ ?y ?c2))"}
rule! {addrule54, "(+ (+ (select ?x ?y ?z) ?w) (select ?x ?u ?v))", "(+ (select ?x (+ ?y ?u) (+ ?z ?v)) ?w)"}
rule! {addrule55, "(+ (+ ?w (select ?x ?y ?z)) (select ?x ?u ?v))", "(+ (select ?x (+ ?y ?u) (+ ?z ?v)) ?w)"}
rule! {addrule56, "(+ (select ?x ?y ?z) (+ (select ?x ?u ?v) ?w))", "(+ (select ?x (+ ?y ?u) (+ ?z ?v)) ?w)"}
rule! {addrule57, "(+ (select ?x ?y ?z) (+ ?w (select ?x ?u ?v)))", "(+ (select ?x (+ ?y ?u) (+ ?z ?v)) ?w)"}
rule! {addrule58, "(+ (- (select ?x ?y ?z) ?w) (select ?x ?u ?v))", "(- (select ?x (+ ?y ?u) (+ ?z ?v)) ?w)"}
rule! {addrule59, "(+ (select ?x ?y ?z) (- (select ?x ?u ?v) ?w))", "(- (select ?x (+ ?y ?u) (+ ?z ?v)) ?w)"}
rule! {addrule60, "(+ (- ?w (select ?x ?y ?z)) (select ?x ?u ?v))", "(+ (select ?x (- ?u ?y) (- ?v ?z)) ?w)"}
rule! {addrule61, "(+ (select ?x ?y ?z) (- ?w (select ?x ?u ?v)))", "(+ (select ?x (- ?y ?u) (- ?z ?v)) ?w)"}
rule! {addrule63, "(+ (+ ?x ?c0) ?c1)", "(+ ?x (+ ?c0 ?c1))"}
rule! {addrule64, "(+ (+ ?x ?c0) ?y)", "(+ (+ ?x ?y) ?c0)"}
rule! {addrule65, "(+ ?x (+ ?y ?c0))", "(+ (+ ?x ?y) ?c0)"}
rule! {addrule66, "(+ (- ?c0 ?x) ?c1)", "(- (+ ?c0 ?c1) ?x)"}
rule! {addrule67, "(+ (- ?c0 ?x) ?y)", "(+ (- ?y ?x) ?c0)"}
rule! {addrule68, "(+ (- ?x ?y) ?y)", "?x"}
rule! {addrule69, "(+ ?x (- ?y ?x))", "?y"}
rule! {addrule70, "(+ ?x (- ?c0 ?y))", "(+ (- ?x ?y) ?c0)"}
rule! {addrule71, "(+ (- ?x ?y) (- ?y ?z))", "(- ?x ?z)"}
rule! {addrule72, "(+ (- ?x ?y) (- ?z ?x))", "(- ?z ?y)"}
conditional_rule! {addrule73, "(+ ?x (* ?y ?c0))", "(- ?x (* ?y (- ?c0)))", "(> (< ?c0 (&& 0 (- ?c0))) 0)", "T"}
rule! {addrule75, "(+ (* ?x ?y) (* ?z ?y))", "(* (+ ?x ?z) ?y)"}
rule! {addrule76, "(+ (* ?x ?y) (* ?y ?z))", "(* (+ ?x ?z) ?y)"}
rule! {addrule77, "(+ (* ?y ?x) (* ?z ?y))", "(* ?y (+ ?x ?z))"}
rule! {addrule78, "(+ (* ?y ?x) (* ?y ?z))", "(* ?y (+ ?x ?z))"}
conditional_rule! {addrule79, "(+ (* ?x ?c0) (* ?y ?c1))", "(* (+ ?x (* ?y (/ ?c1 ?c0))) ?c0)", "(= (% ?c1 ?c0) 0)", "T"}
conditional_rule! {addrule80, "(+ (* ?x ?c0) (* ?y ?c1))", "(* (+ (* ?x (/ ?c0 ?c1)) ?y) ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
rule! {addrule82, "(+ ?x (* ?x ?y))", "(* ?x (+ ?y 1))"}
rule! {addrule83, "(+ ?x (* ?y ?x))", "(* (+ ?y 1) ?x)"}
rule! {addrule84, "(+ (* ?x ?y) ?x)", "(* ?x (+ ?y 1))"}
rule! {addrule86, "(+ (/ (+ ?x ?c0) ?c1) ?c2)", "(/ (+ ?x (+ ?c0 (* ?c1 ?c2))) ?c1)"}
rule! {addrule87, "(+ (+ ?x (/ (+ ?y ?c0) ?c1)) ?c2)", "(+ ?x (/ (+ ?y (+ ?c0 (* ?c1 ?c2))) ?c1))"}
rule! {addrule88, "(+ (+ (/ (+ ?y ?c0) ?c1) ?x) ?c2)", "(+ ?x (/ (+ ?y (+ ?c0 (* ?c1 ?c2))) ?c1))"}
rule! {addrule90, "(+ ?x (/ (+ ?x ?y) ?c0))", "(/ (+ (* (+ ?c0 1) ?x) ?y) ?c0)"}
rule! {addrule91, "(+ ?x (/ (+ ?y ?x) ?c0))", "(/ (+ (* (+ ?c0 1) ?x) ?y) ?c0)"}
rule! {addrule92, "(+ ?x (/ (- ?y ?x) ?c0))", "(/ (+ (* (- ?c0 1) ?x) ?y) ?c0)"}
rule! {addrule93, "(+ ?x (/ (- ?x ?y) ?c0))", "(/ (- (* (+ ?c0 1) ?x) ?y) ?c0)"}
rule! {addrule94, "(+ (/ (- ?x ?y) ?c0) ?x)", "(/ (- (* (+ ?c0 1) ?x) ?y) ?c0)"}
rule! {addrule95, "(+ (/ (- ?y ?x) ?c0) ?x)", "(/ (+ ?y (* (- ?c0 1) ?x)) ?c0)"}
rule! {addrule96, "(+ (/ (+ ?x ?y) ?c0) ?x)", "(/ (+ (* (+ ?c0 1) ?x) ?y) ?c0)"}
rule! {addrule97, "(+ (/ (+ ?y ?x) ?c0) ?x)", "(/ (+ ?y (* (+ ?c0 1) ?x)) ?c0)"}
rule! {addrule98, "(+ (min ?x (- ?y ?z)) ?z)", "(min (+ ?x ?z) ?y)"}
rule! {addrule99, "(+ (min (- ?y ?z) ?x) ?z)", "(min ?y (+ ?x ?z))"}
conditional_rule! {addrule100, "(+ (min ?x (+ ?y ?c0)) ?c1)", "(min (+ ?x ?c1) ?y)", "(= (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {addrule101, "(+ (min (+ ?y ?c0) ?x) ?c1)", "(min ?y (+ ?x ?c1))", "(= (+ ?c0 ?c1) 0)", "T"}
rule! {addrule102, "(+ ?z (min ?x (- ?y ?z)))", "(min (+ ?z ?x) ?y)"}
rule! {addrule103, "(+ ?z (min (- ?y ?z) ?x))", "(min ?y (+ ?z ?x))"}
rule! {addrule104, "(+ ?z (max ?x (- ?y ?z)))", "(max (+ ?z ?x) ?y)"}
rule! {addrule105, "(+ ?z (max (- ?y ?z) ?x))", "(max ?y (+ ?z ?x))"}
rule! {addrule106, "(+ (max ?x (- ?y ?z)) ?z)", "(max (+ ?x ?z) ?y)"}
rule! {addrule107, "(+ (max (- ?y ?z) ?x) ?z)", "(max ?y (+ ?x ?z))"}
conditional_rule! {addrule108, "(+ (max ?x (+ ?y ?c0)) ?c1)", "(max (+ ?x ?c1) ?y)", "(= (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {addrule109, "(+ (max (+ ?y ?c0) ?x) ?c1)", "(max ?y (+ ?x ?c1))", "(= (+ ?c0 ?c1) 0)", "T"}
rule! {addrule110, "(+ (max ?x ?y) (min ?x ?y))", "(+ ?x ?y)"}
conditional_rule! {addrule113, "(+ (* (/ ?x ?c0) ?c0) (% ?x ?c0))", "?x", "(!= ?c0 0)", "T"}
conditional_rule! {addrule114, "(+ (* (+ ?z (/ ?x ?c0)) ?c0) (% ?x ?c0))", "(+ (* ?z ?c0) ?x)", "(!= ?c0 0)", "T"}
conditional_rule! {addrule115, "(+ (* (+ (/ ?x ?c0) ?z) ?c0) (% ?x ?c0))", "(+ ?x (* ?z ?c0))", "(!= ?c0 0)", "T"}
conditional_rule! {addrule116, "(+ (% ?x ?c0) (+ (* (/ ?x ?c0) ?c0) ?z))", "(+ ?x ?z)", "(!= ?c0 0)", "T"}
conditional_rule! {addrule117, "(+ (% ?x ?c0) (- (* (/ ?x ?c0) ?c0) ?z))", "(- ?x ?z)", "(!= ?c0 0)", "T"}
conditional_rule! {addrule118, "(+ (% ?x ?c0) (+ ?z (* (/ ?x ?c0) ?c0)))", "(+ ?x ?z)", "(!= ?c0 0)", "T"}
conditional_rule! {addrule119, "(+ (* (/ ?x ?c0) ?c0) (+ (% ?x ?c0) ?z))", "(+ ?x ?z)", "(!= ?c0 0)", "T"}
conditional_rule! {addrule120, "(+ (* (/ ?x ?c0) ?c0) (- (% ?x ?c0) ?z))", "(- ?x ?z)", "(!= ?c0 0)", "T"}
conditional_rule! {addrule121, "(+ (* (/ ?x ?c0) ?c0) (+ ?z (% ?x ?c0)))", "(+ ?x ?z)", "(!= ?c0 0)", "T"}
rule! {addrule122, "(+ (/ ?x 2) (% ?x 2))", "(/ (+ ?x 1) 2)"}
conditional_rule! {addrule124, "(+ ?x (* (/ (- ?c0 ?x) ?c1) ?c1))", "(- ?c0 (% (- ?c0 ?x) ?c1))", "(> ?c1 0)", "T"}
conditional_rule! {addrule125, "(+ ?x (* (+ (/ (- ?c0 ?x) ?c1) ?y) ?c1))", "(+ (- (* ?y ?c1) (% (- ?c0 ?x) ?c1)) ?c0)", "(> ?c1 0)", "T"}
conditional_rule! {addrule126, "(+ ?x (* (+ ?y (/ (- ?c0 ?x) ?c1)) ?c1))", "(+ (- (* ?y ?c1) (% (- ?c0 ?x) ?c1)) ?c0)", "(> ?c1 0)", "T"}
rule! {andrule22, "(&& ?x T)", "?x"}
rule! {andrule23, "(&& ?x F)", "F"}
rule! {andrule24, "(&& ?x ?x)", "?x"}
rule! {andrule26, "(&& (&& ?x ?y) ?x)", "(&& ?x ?y)"}
rule! {andrule27, "(&& ?x (&& ?x ?y))", "(&& ?x ?y)"}
rule! {andrule28, "(&& (&& ?x ?y) ?y)", "(&& ?x ?y)"}
rule! {andrule29, "(&& ?y (&& ?x ?y))", "(&& ?x ?y)"}
rule! {andrule31, "(&& (&& (&& ?x ?y) ?z) ?x)", "(&& (&& ?x ?y) ?z)"}
rule! {andrule32, "(&& ?x (&& (&& ?x ?y) ?z))", "(&& (&& ?x ?y) ?z)"}
rule! {andrule33, "(&& (&& ?z (&& ?x ?y)) ?x)", "(&& ?z (&& ?x ?y))"}
rule! {andrule34, "(&& ?x (&& ?z (&& ?x ?y)))", "(&& ?z (&& ?x ?y))"}
rule! {andrule35, "(&& (&& (&& ?x ?y) ?z) ?y)", "(&& (&& ?x ?y) ?z)"}
rule! {andrule36, "(&& ?y (&& (&& ?x ?y) ?z))", "(&& (&& ?x ?y) ?z)"}
rule! {andrule37, "(&& (&& ?z (&& ?x ?y)) ?y)", "(&& ?z (&& ?x ?y))"}
rule! {andrule38, "(&& ?y (&& ?z (&& ?x ?y)))", "(&& ?z (&& ?x ?y))"}
rule! {andrule40, "(&& (|| ?x ?y) ?x)", "?x"}
rule! {andrule41, "(&& ?x (|| ?x ?y))", "?x"}
rule! {andrule42, "(&& (|| ?x ?y) ?y)", "?y"}
rule! {andrule43, "(&& ?y (|| ?x ?y))", "?y"}
rule! {andrule45, "(!= ?x (= (&& ?y ?x) ?y))", "F"}
rule! {andrule46, "(!= ?x (= (&& ?y ?y) ?x))", "F"}
rule! {andrule47, "(= (&& (!= (&& ?z ?x) ?y) ?x) ?y)", "F"}
rule! {andrule48, "(= (&& (!= (&& ?z ?x) ?y) ?y) ?x)", "F"}
rule! {andrule49, "(= (&& (!= ?x (&& ?y ?z)) ?x) ?y)", "F"}
rule! {andrule50, "(= (&& (!= ?x (&& ?y ?z)) ?y) ?x)", "F"}
rule! {andrule51, "(!= (&& (= (&& ?z ?x) ?y) ?x) ?y)", "F"}
rule! {andrule52, "(!= (&& (= (&& ?z ?x) ?y) ?y) ?x)", "F"}
rule! {andrule53, "(!= (&& (= ?x (&& ?y ?z)) ?x) ?y)", "F"}
rule! {andrule54, "(!= (&& (= ?x (&& ?y ?z)) ?y) ?x)", "F"}
rule! {andrule55, "(&& ?x (not ?x))", "F"}
rule! {andrule56, "(&& (not ?x) ?x)", "F"}
rule! {andrule57, "(< (<= ?y (&& ?x ?x)) ?y)", "F"}
conditional_rule! {andrule66, "(< (<= ?x (&& ?c1 ?c0)) ?x)", "F", "(<= ?c1 ?c0)", "T"}
conditional_rule! {andrule67, "(< (<= ?c0 (&& ?x ?x)) ?c1)", "F", "(<= ?c1 ?c0)", "T"}
conditional_rule! {andrule68, "(<= (<= ?c0 (&& ?x ?x)) ?c1)", "F", "(< ?c1 ?c0)", "T"}
conditional_rule! {andrule69, "(<= (<= ?x (&& ?c1 ?c0)) ?x)", "F", "(< ?c1 ?c0)", "T"}
rule! {andrule70, "(< (< ?c0 (&& ?x ?c1)) ?x)", "(< (max ?c0 ?c1) ?x)"}
rule! {andrule71, "(<= (<= ?c0 (&& ?x ?c1)) ?x)", "(<= (max ?c0 ?c1) ?x)"}
rule! {andrule72, "(< (< ?x (&& ?c0 ?x)) ?c1)", "(< ?x (min ?c0 ?c1))"}
rule! {andrule73, "(<= (<= ?x (&& ?c0 ?x)) ?c1)", "(<= ?x (min ?c0 ?c1))"}
rule! {andrule79, "(&& (|| ?x (&& ?y ?z)) ?y)", "(&& (|| ?x ?z) ?y)"}
rule! {andrule80, "(&& (|| ?x (&& ?z ?y)) ?y)", "(&& (|| ?x ?z) ?y)"}
rule! {andrule81, "(&& ?y (|| ?x (&& ?y ?z)))", "(&& ?y (|| ?x ?z))"}
rule! {andrule82, "(&& ?y (|| ?x (&& ?z ?y)))", "(&& ?y (|| ?x ?z))"}
rule! {andrule84, "(&& (|| (&& ?y ?z) ?x) ?y)", "(&& (|| ?z ?x) ?y)"}
rule! {andrule85, "(&& (|| (&& ?z ?y) ?x) ?y)", "(&& (|| ?z ?x) ?y)"}
rule! {andrule86, "(&& ?y (|| (&& ?y ?z) ?x))", "(&& ?y (|| ?z ?x))"}
rule! {andrule87, "(&& ?y (|| (&& ?z ?y) ?x))", "(&& ?y (|| ?z ?x))"}
rule! {andrule89, "(&& (&& ?x (|| ?y ?z)) ?y)", "(&& ?x ?y)"}
rule! {andrule90, "(&& (&& ?x (|| ?z ?y)) ?y)", "(&& ?x ?y)"}
rule! {andrule91, "(&& ?y (&& ?x (|| ?y ?z)))", "(&& ?y ?x)"}
rule! {andrule92, "(&& ?y (&& ?x (|| ?z ?y)))", "(&& ?y ?x)"}
rule! {andrule94, "(&& (&& (|| ?y ?z) ?x) ?y)", "(&& ?x ?y)"}
rule! {andrule95, "(&& (&& (|| ?z ?y) ?x) ?y)", "(&& ?x ?y)"}
rule! {andrule96, "(&& ?y (&& (|| ?y ?z) ?x))", "(&& ?y ?x)"}
rule! {andrule97, "(&& ?y (&& (|| ?z ?y) ?x))", "(&& ?y ?x)"}
rule! {andrule99, "(&& (|| ?x ?y) (|| ?x ?z))", "(|| ?x (&& ?y ?z))"}
rule! {andrule100, "(&& (|| ?x ?y) (|| ?z ?x))", "(|| ?x (&& ?y ?z))"}
rule! {andrule101, "(&& (|| ?y ?x) (|| ?x ?z))", "(|| ?x (&& ?y ?z))"}
rule! {andrule102, "(&& (|| ?y ?x) (|| ?z ?x))", "(|| ?x (&& ?y ?z))"}
rule! {andrule104, "(< (< ?x (&& ?y ?x)) ?z)", "(< ?x (min ?y ?z))"}
rule! {andrule105, "(< (< ?y (&& ?x ?z)) ?x)", "(< (max ?y ?z) ?x)"}
rule! {andrule106, "(<= (<= ?x (&& ?y ?x)) ?z)", "(<= ?x (min ?y ?z))"}
rule! {andrule107, "(<= (<= ?y (&& ?x ?z)) ?x)", "(<= (max ?y ?z) ?x)"}
rule! {divrule96, "(/ ?x 1)", "?x"}
rule! {divrule100, "(/ ?x ?x)", "1"}
rule! {divrule107, "(/ (select ?x ?c0 ?c1) ?c2)", "(select ?x (/ ?c0 ?c2) (/ ?c1 ?c2))"}
conditional_rule! {divrule112, "(/ (* ?x ?c0) ?c1)", "(/ ?x (/ ?c1 ?c0))", "(= (% ?c1 ?c0) (!= (> (&& 0 ?c0) (&& 0 (/ ?c1 ?c0))) 0))", "T"}
conditional_rule! {divrule114, "(/ (* ?x ?c0) ?c1)", "(* ?x (/ ?c0 ?c1))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule116, "(/ (+ (* ?x ?c0) ?y) ?c1)", "(+ (/ ?y ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule117, "(/ (- (* ?x ?c0) ?y) ?c1)", "(+ (/ (- ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule118, "(/ (+ ?y (* ?x ?c0)) ?c1)", "(+ (/ ?y ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule119, "(/ (- ?y (* ?x ?c0)) ?c1)", "(- (/ ?y ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule121, "(/ (+ (+ (* ?x ?c0) ?y) ?z) ?c1)", "(+ (/ (+ ?y ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule122, "(/ (+ (- (* ?x ?c0) ?y) ?z) ?c1)", "(+ (/ (- ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule123, "(/ (- (+ (* ?x ?c0) ?y) ?z) ?c1)", "(+ (/ (- ?y ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule124, "(/ (- (- (* ?x ?c0) ?y) ?z) ?c1)", "(+ (/ (- (- ?y) ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule126, "(/ (+ (+ ?y (* ?x ?c0)) ?z) ?c1)", "(+ (/ (+ ?y ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule127, "(/ (- (+ ?y (* ?x ?c0)) ?z) ?c1)", "(+ (/ (- ?y ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule128, "(/ (- (- ?y (* ?x ?c0)) ?z) ?c1)", "(- (/ (- ?y ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule129, "(/ (+ (- ?y (* ?x ?c0)) ?z) ?c1)", "(- (/ (+ ?y ?z) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule131, "(/ (+ ?z (+ (* ?x ?c0) ?y)) ?c1)", "(+ (/ (+ ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule132, "(/ (+ ?z (- (* ?x ?c0) ?y)) ?c1)", "(+ (/ (- ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule133, "(/ (- ?z (- (* ?x ?c0) ?y)) ?c1)", "(- (/ (+ ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule134, "(/ (- ?z (+ (* ?x ?c0) ?y)) ?c1)", "(- (/ (- ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule136, "(/ (+ ?z (+ ?y (* ?x ?c0))) ?c1)", "(+ (/ (+ ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule137, "(/ (- ?z (+ ?y (* ?x ?c0))) ?c1)", "(- (/ (- ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule138, "(/ (+ ?z (- ?y (* ?x ?c0))) ?c1)", "(- (/ (+ ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule139, "(/ (- ?z (- ?y (* ?x ?c0))) ?c1)", "(+ (/ (- ?z ?y) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule142, "(/ (+ (+ (+ (* ?x ?c0) ?y) ?z) ?w) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule143, "(/ (+ (+ (+ ?y (* ?x ?c0)) ?z) ?w) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule144, "(/ (+ (+ ?z (+ (* ?x ?c0) ?y)) ?w) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule145, "(/ (+ (+ ?z (+ ?y (* ?x ?c0))) ?w) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule146, "(/ (+ ?w (+ (+ (* ?x ?c0) ?y) ?z)) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule147, "(/ (+ ?w (+ (+ ?y (* ?x ?c0)) ?z)) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule148, "(/ (+ ?w (+ ?z (+ (* ?x ?c0) ?y))) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule149, "(/ (+ ?w (+ ?z (+ ?y (* ?x ?c0)))) ?c1)", "(+ (/ (+ (+ ?y ?z) ?w) ?c1) (* ?x (/ ?c0 ?c1)))", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {divrule151, "(/ (+ ?x ?c0) ?c1)", "(+ (/ ?x ?c1) (/ ?c0 ?c1))", "(= (% ?c0 ?c1) 0)", "T"}
rule! {divrule152, "(/ (+ ?x ?y) ?x)", "(+ (/ ?y ?x) 1)"}
rule! {divrule153, "(/ (+ ?y ?x) ?x)", "(+ (/ ?y ?x) 1)"}
rule! {divrule154, "(/ (- ?x ?y) ?x)", "(+ (/ (- ?y) ?x) 1)"}
rule! {divrule155, "(/ (- ?y ?x) ?x)", "(- (/ ?y ?x) 1)"}
rule! {divrule156, "(/ (+ (+ ?x ?y) ?z) ?x)", "(+ (/ (+ ?y ?z) ?x) 1)"}
rule! {divrule157, "(/ (+ (+ ?y ?x) ?z) ?x)", "(+ (/ (+ ?y ?z) ?x) 1)"}
rule! {divrule158, "(/ (+ ?z (+ ?x ?y)) ?x)", "(+ (/ (+ ?z ?y) ?x) 1)"}
rule! {divrule159, "(/ (+ ?z (+ ?y ?x)) ?x)", "(+ (/ (+ ?z ?y) ?x) 1)"}
rule! {divrule162, "(/ (+ (* ?x ?y) ?z) ?x)", "(+ ?y (/ ?z ?x))"}
rule! {divrule163, "(/ (+ (* ?y ?x) ?z) ?x)", "(+ ?y (/ ?z ?x))"}
rule! {divrule164, "(/ (+ ?z (* ?x ?y)) ?x)", "(+ (/ ?z ?x) ?y)"}
rule! {divrule165, "(/ (+ ?z (* ?y ?x)) ?x)", "(+ (/ ?z ?x) ?y)"}
rule! {divrule166, "(/ (- (* ?x ?y) ?z) ?x)", "(+ ?y (/ (- ?z) ?x))"}
rule! {divrule167, "(/ (- (* ?y ?x) ?z) ?x)", "(+ ?y (/ (- ?z) ?x))"}
rule! {divrule168, "(/ (- ?z (* ?x ?y)) ?x)", "(- (/ ?z ?x) ?y)"}
rule! {divrule169, "(/ (- ?z (* ?y ?x)) ?x)", "(- (/ ?z ?x) ?y)"}
rule! {divrule177, "(/ ?x -1)", "(- ?x)"}
conditional_rule! {divrule178, "(/ ?c0 ?y)", "(select (< ?y 0) (- ?c0) ?c0)", "(= ?c0 -1)", "T"}
conditional_rule! {divrule179, "(/ (+ (* ?x ?c0) ?c1) ?c2)", "(/ (+ ?x (/ ?c1 ?c0)) (/ ?c2 ?c0))", "(= (> (> ?c2 (&& 0 ?c0)) (&& 0 (% ?c2 ?c0))) 0)", "T"}
conditional_rule! {divrule180, "(/ (+ (* ?x ?c0) ?c1) ?c2)", "(+ (* ?x (/ ?c0 ?c2)) (/ ?c1 ?c2))", "(= (> ?c2 (&& 0 (% ?c0 ?c2))) 0)", "T"}
conditional_rule! {divrule182, "(/ (+ (% ?x 2) ?c0) 2)", "(+ (% ?x 2) (/ ?c0 2))", "(= (% ?c0 2) 1)", "T"}
rule! {eqrule67, "(= (select ?x 0 ?y) 0)", "(|| ?x (= ?y 0))"}
conditional_rule! {eqrule68, "(= (select ?x ?c0 ?y) 0)", "(&& (not ?x) (= ?y 0))", "(!= ?c0 0)", "T"}
rule! {eqrule69, "(= (select ?x ?y 0) 0)", "(|| (not ?x) (= ?y 0))"}
conditional_rule! {eqrule70, "(= (select ?x ?y ?c0) 0)", "(&& ?x (= ?y 0))", "(!= ?c0 0)", "T"}
rule! {eqrule71, "(= (- (max ?x ?y) ?y) 0)", "(<= ?x ?y)"}
rule! {eqrule72, "(= (- (min ?x ?y) ?y) 0)", "(<= ?y ?x)"}
rule! {eqrule73, "(= (- (max ?y ?x) ?y) 0)", "(<= ?x ?y)"}
rule! {eqrule74, "(= (- (min ?y ?x) ?y) 0)", "(<= ?y ?x)"}
rule! {eqrule75, "(= (- ?y (max ?x ?y)) 0)", "(<= ?x ?y)"}
rule! {eqrule76, "(= (- ?y (min ?x ?y)) 0)", "(<= ?y ?x)"}
rule! {eqrule77, "(= (- ?y (max ?y ?x)) 0)", "(<= ?x ?y)"}
rule! {eqrule78, "(= (- ?y (min ?y ?x)) 0)", "(<= ?y ?x)"}
conditional_rule! {eqrule79, "(= (+ (max ?x ?c0) ?c1) 0)", "(= ?x (- ?c1))", "(< (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {eqrule80, "(= (+ (min ?x ?c0) ?c1) 0)", "(= ?x (- ?c1))", "(> (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {eqrule81, "(= (+ (max ?x ?c0) ?c1) 0)", "(<= ?x ?c0)", "(= (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {eqrule82, "(= (+ (min ?x ?c0) ?c1) 0)", "(<= ?c0 ?x)", "(= (+ ?c0 ?c1) 0)", "T"}
rule! {eqrule83, "(= (max ?x 0) 0)", "(<= ?x 0)"}
rule! {eqrule84, "(= (min ?x 0) 0)", "(<= 0 ?x)"}
rule! {eqrule90, "(= (+ (- ?x ?y) ?c0) 0)", "(= ?x (+ ?y (- ?c0)))"}
rule! {eqrule91, "(= (+ ?x ?c0) 0)", "(= ?x (- ?c0))"}
rule! {ltrule33, "(< ?c0 ?c1)", "(< ?c0 ?c1)"}
rule! {ltrule34, "(< ?x ?x)", "F"}
rule! {ltrule38, "(< (max ?x ?y) ?x)", "F"}
rule! {ltrule39, "(< (max ?y ?x) ?x)", "F"}
rule! {ltrule40, "(< ?x (min ?x ?y))", "F"}
rule! {ltrule41, "(< ?x (min ?y ?x))", "F"}
rule! {ltrule58, "(< (+ ?x ?c0) ?y)", "(< ?x (+ ?y (- ?c0)))"}
rule! {ltrule61, "(< ?c0 (+ ?x ?c1))", "(< (- ?c0 ?c1) ?x)"}
rule! {ltrule64, "(< (- ?x ?y) ?z)", "(< ?x (+ ?z ?y))"}
rule! {ltrule65, "(< ?z (- ?x ?y))", "(< (+ ?z ?y) ?x)"}
rule! {ltrule67, "(< (+ (- ?x ?y) ?z) ?w)", "(< (+ ?x ?z) (+ ?y ?w))"}
rule! {ltrule68, "(< (+ ?z (- ?x ?y)) ?w)", "(< (+ ?x ?z) (+ ?y ?w))"}
rule! {ltrule69, "(< ?w (+ (- ?x ?y) ?z))", "(< (+ ?w ?y) (+ ?x ?z))"}
rule! {ltrule70, "(< ?w (+ ?z (- ?x ?y)))", "(< (+ ?w ?y) (+ ?x ?z))"}
rule! {ltrule72, "(< (+ (+ (- ?x ?y) ?z) ?u) ?w)", "(< (+ (+ ?x ?z) ?u) (+ ?w ?y))"}
rule! {ltrule73, "(< (+ (+ ?z (- ?x ?y)) ?u) ?w)", "(< (+ (+ ?x ?z) ?u) (+ ?w ?y))"}
rule! {ltrule74, "(< (+ ?u (+ (- ?x ?y) ?z)) ?w)", "(< (+ (+ ?x ?z) ?u) (+ ?w ?y))"}
rule! {ltrule75, "(< (+ ?u (+ ?z (- ?x ?y))) ?w)", "(< (+ (+ ?x ?z) ?u) (+ ?w ?y))"}
rule! {ltrule77, "(< ?w (+ (+ (- ?x ?y) ?z) ?u))", "(< (+ ?w ?y) (+ (+ ?x ?z) ?u))"}
rule! {ltrule78, "(< ?w (+ (+ ?z (- ?x ?y)) ?u))", "(< (+ ?w ?y) (+ (+ ?x ?z) ?u))"}
rule! {ltrule79, "(< ?w (+ ?u (+ (- ?x ?y) ?z)))", "(< (+ ?w ?y) (+ (+ ?x ?z) ?u))"}
rule! {ltrule80, "(< ?w (+ ?u (+ ?z (- ?x ?y))))", "(< (+ ?w ?y) (+ (+ ?x ?z) ?u))"}
rule! {ltrule84, "(< ?x (+ ?x ?y))", "(< 0 ?y)"}
rule! {ltrule85, "(< ?x (+ ?y ?x))", "(< 0 ?y)"}
rule! {ltrule88, "(< (+ ?x ?y) ?x)", "(< ?y 0)"}
rule! {ltrule89, "(< (+ ?y ?x) ?x)", "(< ?y 0)"}
rule! {ltrule92, "(< (+ ?x ?y) (+ ?x ?z))", "(< ?y ?z)"}
rule! {ltrule93, "(< (+ ?x ?y) (+ ?z ?x))", "(< ?y ?z)"}
rule! {ltrule94, "(< (+ ?y ?x) (+ ?x ?z))", "(< ?y ?z)"}
rule! {ltrule95, "(< (+ ?y ?x) (+ ?z ?x))", "(< ?y ?z)"}
rule! {ltrule98, "(< (+ (+ ?x ?y) ?w) (+ ?x ?z))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule99, "(< (+ (+ ?y ?x) ?w) (+ ?x ?z))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule100, "(< (+ ?w (+ ?x ?y)) (+ ?x ?z))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule101, "(< (+ ?w (+ ?y ?x)) (+ ?x ?z))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule102, "(< (+ (+ ?x ?y) ?w) (+ ?z ?x))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule103, "(< (+ (+ ?y ?x) ?w) (+ ?z ?x))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule104, "(< (+ ?w (+ ?x ?y)) (+ ?z ?x))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule105, "(< (+ ?w (+ ?y ?x)) (+ ?z ?x))", "(< (+ ?y ?w) ?z)"}
rule! {ltrule108, "(< (+ ?x ?z) (+ (+ ?x ?y) ?w))", "(< ?z (+ ?y ?w))"}
rule! {ltrule109, "(< (+ ?x ?z) (+ (+ ?y ?x) ?w))", "(< ?z (+ ?y ?w))"}
rule! {ltrule110, "(< (+ ?x ?z) (+ ?w (+ ?x ?y)))", "(< ?z (+ ?y ?w))"}
rule! {ltrule111, "(< (+ ?x ?z) (+ ?w (+ ?y ?x)))", "(< ?z (+ ?y ?w))"}
rule! {ltrule112, "(< (+ ?z ?x) (+ (+ ?x ?y) ?w))", "(< ?z (+ ?y ?w))"}
rule! {ltrule113, "(< (+ ?z ?x) (+ (+ ?y ?x) ?w))", "(< ?z (+ ?y ?w))"}
rule! {ltrule114, "(< (+ ?z ?x) (+ ?w (+ ?x ?y)))", "(< ?z (+ ?y ?w))"}
rule! {ltrule115, "(< (+ ?z ?x) (+ ?w (+ ?y ?x)))", "(< ?z (+ ?y ?w))"}
rule! {ltrule118, "(< (+ (+ ?x ?y) ?w) (+ (+ ?x ?z) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule119, "(< (+ (+ ?y ?x) ?w) (+ (+ ?x ?z) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule120, "(< (+ (+ ?x ?y) ?w) (+ (+ ?z ?x) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule121, "(< (+ (+ ?y ?x) ?w) (+ (+ ?z ?x) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule122, "(< (+ ?w (+ ?x ?y)) (+ (+ ?x ?z) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule123, "(< (+ ?w (+ ?y ?x)) (+ (+ ?x ?z) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule124, "(< (+ ?w (+ ?x ?y)) (+ (+ ?z ?x) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule125, "(< (+ ?w (+ ?y ?x)) (+ (+ ?z ?x) ?u))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule126, "(< (+ (+ ?x ?y) ?w) (+ ?u (+ ?x ?z)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule127, "(< (+ (+ ?y ?x) ?w) (+ ?u (+ ?x ?z)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule128, "(< (+ (+ ?x ?y) ?w) (+ ?u (+ ?z ?x)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule129, "(< (+ (+ ?y ?x) ?w) (+ ?u (+ ?z ?x)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule130, "(< (+ ?w (+ ?x ?y)) (+ ?u (+ ?x ?z)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule131, "(< (+ ?w (+ ?y ?x)) (+ ?u (+ ?x ?z)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule132, "(< (+ ?w (+ ?x ?y)) (+ ?u (+ ?z ?x)))", "(< (+ ?y ?w) (+ ?z ?u))"}
rule! {ltrule133, "(< (+ ?w (+ ?y ?x)) (+ ?u (+ ?z ?x)))", "(< (+ ?y ?w) (+ ?z ?u))"}
conditional_rule! {ltrule136, "(< (* ?x ?c0) (* ?y ?c0))", "(< ?x ?y)", "(> ?c0 0)", "T"}
conditional_rule! {ltrule137, "(< (* ?x ?c0) (* ?y ?c0))", "(< ?y ?x)", "(< ?c0 0)", "T"}
conditional_rule! {ltrule141, "(< ?c1 (* ?x ?c0))", "(< (/ ?c1 ?c0) ?x)", "(> ?c0 0)", "T"}
conditional_rule! {ltrule144, "(< (/ ?x ?c0) ?c1)", "(< ?x (* ?c1 ?c0))", "(> ?c0 0)", "T"}
rule! {ltrule151, "(< (min (+ ?x ?c0) ?y) (+ ?x ?c1))", "(< (|| (< ?c0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule152, "(< (min ?y (+ ?x ?c0)) (+ ?x ?c1))", "(< (|| (< ?c0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule153, "(< (max (+ ?x ?c0) ?y) (+ ?x ?c1))", "(< (&& (< ?c0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule154, "(< (max ?y (+ ?x ?c0)) (+ ?x ?c1))", "(< (&& (< ?c0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule156, "(< ?x (+ (min (+ ?x ?c0) ?y) ?c1))", "(< (&& (< 0 (+ ?c0 ?c1)) ?x) (+ ?y ?c1))"}
rule! {ltrule157, "(< ?x (+ (min ?y (+ ?x ?c0)) ?c1))", "(< (&& (< 0 (+ ?c0 ?c1)) ?x) (+ ?y ?c1))"}
rule! {ltrule158, "(< ?x (+ (max (+ ?x ?c0) ?y) ?c1))", "(< (|| (< 0 (+ ?c0 ?c1)) ?x) (+ ?y ?c1))"}
rule! {ltrule159, "(< ?x (+ (max ?y (+ ?x ?c0)) ?c1))", "(< (|| (< 0 (+ ?c0 ?c1)) ?x) (+ ?y ?c1))"}
rule! {ltrule162, "(< (min ?x ?y) (+ ?x ?c1))", "(< (|| (< 0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule163, "(< (min ?y ?x) (+ ?x ?c1))", "(< (|| (< 0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule164, "(< (max ?x ?y) (+ ?x ?c1))", "(< (&& (< 0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule165, "(< (max ?y ?x) (+ ?x ?c1))", "(< (&& (< 0 ?c1) ?y) (+ ?x ?c1))"}
rule! {ltrule167, "(< ?x (+ (min ?x ?y) ?c1))", "(< (&& (< 0 ?c1) ?x) (+ ?y ?c1))"}
rule! {ltrule168, "(< ?x (+ (min ?y ?x) ?c1))", "(< (&& (< 0 ?c1) ?x) (+ ?y ?c1))"}
rule! {ltrule169, "(< ?x (+ (max ?x ?y) ?c1))", "(< (|| (< 0 ?c1) ?x) (+ ?y ?c1))"}
rule! {ltrule170, "(< ?x (+ (max ?y ?x) ?c1))", "(< (|| (< 0 ?c1) ?x) (+ ?y ?c1))"}
rule! {ltrule173, "(< (min (+ ?x ?c0) ?y) ?x)", "(< (|| (< ?c0 0) ?y) ?x)"}
rule! {ltrule174, "(< (min ?y (+ ?x ?c0)) ?x)", "(< (|| (< ?c0 0) ?y) ?x)"}
rule! {ltrule175, "(< (max (+ ?x ?c0) ?y) ?x)", "(< (&& (< ?c0 0) ?y) ?x)"}
rule! {ltrule176, "(< (max ?y (+ ?x ?c0)) ?x)", "(< (&& (< ?c0 0) ?y) ?x)"}
rule! {ltrule178, "(< ?x (min (+ ?x ?c0) ?y))", "(< (&& (< 0 ?c0) ?x) ?y)"}
rule! {ltrule179, "(< ?x (min ?y (+ ?x ?c0)))", "(< (&& (< 0 ?c0) ?x) ?y)"}
rule! {ltrule180, "(< ?x (max (+ ?x ?c0) ?y))", "(< (|| (< 0 ?c0) ?x) ?y)"}
rule! {ltrule181, "(< ?x (max ?y (+ ?x ?c0)))", "(< (|| (< 0 ?c0) ?x) ?y)"}
rule! {ltrule184, "(< (min ?x ?y) ?x)", "(< ?y ?x)"}
rule! {ltrule185, "(< (min ?y ?x) ?x)", "(< ?y ?x)"}
rule! {ltrule186, "(< ?x (max ?x ?y))", "(< ?x ?y)"}
rule! {ltrule187, "(< ?x (max ?y ?x))", "(< ?x ?y)"}
rule! {ltrule190, "(< (min ?y ?c0) ?c1)", "(< (|| (< ?c0 ?c1) ?y) ?c1)"}
rule! {ltrule191, "(< (max ?y ?c0) ?c1)", "(< (&& (< ?c0 ?c1) ?y) ?c1)"}
rule! {ltrule192, "(< ?c1 (min ?y ?c0))", "(< (&& (< ?c1 ?c0) ?c1) ?y)"}
rule! {ltrule193, "(< ?c1 (max ?y ?c0))", "(< (|| (< ?c1 ?c0) ?c1) ?y)"}
conditional_rule! {ltrule198, "(< ?x (select ?y (+ ?x ?c0) ?z))", "(&& (not ?y) (< ?x ?z))", "(<= ?c0 0)", "T"}
conditional_rule! {ltrule199, "(< ?x (select ?y (+ ?x ?c0) ?z))", "(|| ?y (< ?x ?z))", "(> ?c0 0)", "T"}
conditional_rule! {ltrule200, "(< ?x (select ?y ?z (+ ?x ?c0)))", "(&& ?y (< ?x ?z))", "(<= ?c0 0)", "T"}
conditional_rule! {ltrule201, "(< ?x (select ?y ?z (+ ?x ?c0)))", "(|| (not ?y) (< ?x ?z))", "(> ?c0 0)", "T"}
conditional_rule! {ltrule203, "(< ?x (+ (select ?y (+ ?x ?c0) ?z) ?c1))", "(&& (not ?y) (< ?x (+ ?z ?c1)))", "(<= (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {ltrule204, "(< ?x (+ (select ?y (+ ?x ?c0) ?z) ?c1))", "(|| ?y (< ?x (+ ?z ?c1)))", "(> (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {ltrule205, "(< ?x (+ (select ?y ?z (+ ?x ?c0)) ?c1))", "(&& ?y (< ?x (+ ?z ?c1)))", "(<= (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {ltrule206, "(< ?x (+ (select ?y ?z (+ ?x ?c0)) ?c1))", "(|| (not ?y) (< ?x (+ ?z ?c1)))", "(> (+ ?c0 ?c1) 0)", "T"}
conditional_rule! {ltrule208, "(< (select ?y (+ ?x ?c0) ?z) ?x)", "(&& (not ?y) (< ?z ?x))", "(>= ?c0 0)", "T"}
conditional_rule! {ltrule209, "(< (select ?y (+ ?x ?c0) ?z) ?x)", "(|| ?y (< ?z ?x))", "(< ?c0 0)", "T"}
conditional_rule! {ltrule210, "(< (select ?y ?z (+ ?x ?c0)) ?x)", "(&& ?y (< ?z ?x))", "(>= ?c0 0)", "T"}
conditional_rule! {ltrule211, "(< (select ?y ?z (+ ?x ?c0)) ?x)", "(|| (not ?y) (< ?z ?x))", "(< ?c0 0)", "T"}
conditional_rule! {ltrule213, "(< (select ?y (+ ?x ?c0) ?z) (+ ?x ?c1))", "(&& (not ?y) (< ?z (+ ?x ?c1)))", "(>= ?c0 ?c1)", "T"}
conditional_rule! {ltrule214, "(< (select ?y (+ ?x ?c0) ?z) (+ ?x ?c1))", "(|| ?y (< ?z (+ ?x ?c1)))", "(< ?c0 ?c1)", "T"}
conditional_rule! {ltrule215, "(< (select ?y ?z (+ ?x ?c0)) (+ ?x ?c1))", "(&& ?y (< ?z (+ ?x ?c1)))", "(>= ?c0 ?c1)", "T"}
conditional_rule! {ltrule216, "(< (select ?y ?z (+ ?x ?c0)) (+ ?x ?c1))", "(|| (not ?y) (< ?z (+ ?x ?c1)))", "(< ?c0 ?c1)", "T"}
conditional_rule! {ltrule222, "(< (* ?x ?c0) (* ?y ?c1))", "(< ?x (* ?y (/ ?c1 ?c0)))", "(= (% ?c1 ?c0) (> (&& 0 ?c0) 0))", "T"}
conditional_rule! {ltrule223, "(< (* ?x ?c0) (* ?y ?c1))", "(< (* ?x (/ ?c0 ?c1)) ?y)", "(= (% ?c0 ?c1) (> (&& 0 ?c1) 0))", "T"}
conditional_rule! {ltrule225, "(< (* ?x ?c0) (+ (* ?y ?c0) ?c1))", "(< ?x (+ ?y (/ (- (+ ?c1 ?c0) 1) ?c0)))", "(> ?c0 0)", "T"}
conditional_rule! {ltrule226, "(< (+ (* ?x ?c0) ?c1) (* ?y ?c0))", "(< (+ ?x (/ ?c1 ?c0)) ?y)", "(> ?c0 0)", "T"}
conditional_rule! {ltrule236, "(< (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?w) (+ ?x ?z))", "(< (+ ?w ?c0) (+ (% (+ ?x ?c0) ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule237, "(< (+ ?w (* (/ (+ ?x ?c0) ?c1) ?c1)) (+ ?x ?z))", "(< (+ ?w ?c0) (+ (% (+ ?x ?c0) ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule238, "(< (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?w) (+ ?z ?x))", "(< (+ ?w ?c0) (+ (% (+ ?x ?c0) ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule239, "(< (+ ?w (* (/ (+ ?x ?c0) ?c1) ?c1)) (+ ?z ?x))", "(< (+ ?w ?c0) (+ (% (+ ?x ?c0) ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule240, "(< (+ ?x ?z) (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?w))", "(< (+ (% (+ ?x ?c0) ?c1) ?z) (+ ?w ?c0))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule241, "(< (+ ?x ?z) (+ ?w (* (/ (+ ?x ?c0) ?c1) ?c1)))", "(< (+ (% (+ ?x ?c0) ?c1) ?z) (+ ?w ?c0))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule242, "(< (+ ?z ?x) (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?w))", "(< (+ (% (+ ?x ?c0) ?c1) ?z) (+ ?w ?c0))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule243, "(< (+ ?z ?x) (+ ?w (* (/ (+ ?x ?c0) ?c1) ?c1)))", "(< (+ (% (+ ?x ?c0) ?c1) ?z) (+ ?w ?c0))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule246, "(< (* (/ (+ ?x ?c0) ?c1) ?c1) (+ ?x ?z))", "(< ?c0 (+ (% (+ ?x ?c0) ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule247, "(< (* (/ (+ ?x ?c0) ?c1) ?c1) (+ ?z ?x))", "(< ?c0 (+ (% (+ ?x ?c0) ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule248, "(< (+ ?x ?z) (* (/ (+ ?x ?c0) ?c1) ?c1))", "(< (+ (% (+ ?x ?c0) ?c1) ?z) ?c0)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule249, "(< (+ ?z ?x) (* (/ (+ ?x ?c0) ?c1) ?c1))", "(< (+ (% (+ ?x ?c0) ?c1) ?z) ?c0)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule252, "(< (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?w) ?x)", "(< (+ ?w ?c0) (% (+ ?x ?c0) ?c1))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule253, "(< (+ ?w (* (/ (+ ?x ?c0) ?c1) ?c1)) ?x)", "(< (+ ?w ?c0) (% (+ ?x ?c0) ?c1))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule254, "(< ?x (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?w))", "(< (% (+ ?x ?c0) ?c1) (+ ?w ?c0))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule255, "(< ?x (+ ?w (* (/ (+ ?x ?c0) ?c1) ?c1)))", "(< (% (+ ?x ?c0) ?c1) (+ ?w ?c0))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule258, "(< (+ (* (/ ?x ?c1) ?c1) ?w) (+ ?x ?z))", "(< ?w (+ (% ?x ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule259, "(< (+ ?w (* (/ ?x ?c1) ?c1)) (+ ?x ?z))", "(< ?w (+ (% ?x ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule260, "(< (+ (* (/ ?x ?c1) ?c1) ?w) (+ ?z ?x))", "(< ?w (+ (% ?x ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule261, "(< (+ ?w (* (/ ?x ?c1) ?c1)) (+ ?z ?x))", "(< ?w (+ (% ?x ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule262, "(< (+ ?x ?z) (+ (* (/ ?x ?c1) ?c1) ?w))", "(< (+ (% ?x ?c1) ?z) ?w)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule263, "(< (+ ?x ?z) (+ ?w (* (/ ?x ?c1) ?c1)))", "(< (+ (% ?x ?c1) ?z) ?w)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule264, "(< (+ ?z ?x) (+ (* (/ ?x ?c1) ?c1) ?w))", "(< (+ (% ?x ?c1) ?z) ?w)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule265, "(< (+ ?z ?x) (+ ?w (* (/ ?x ?c1) ?c1)))", "(< (+ (% ?x ?c1) ?z) ?w)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule268, "(< (* (/ (+ ?x ?c0) ?c1) ?c1) ?x)", "(< ?c0 (% (+ ?x ?c0) ?c1))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule269, "(< ?x (* (/ (+ ?x ?c0) ?c1) ?c1))", "(< (% (+ ?x ?c0) ?c1) ?c0)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule272, "(< (* (/ ?x ?c1) ?c1) (+ ?x ?z))", "(< 0 (+ (% ?x ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule273, "(< (* (/ ?x ?c1) ?c1) (+ ?z ?x))", "(< 0 (+ (% ?x ?c1) ?z))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule274, "(< (+ ?x ?z) (* (/ ?x ?c1) ?c1))", "(< (+ (% ?x ?c1) ?z) 0)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule275, "(< (+ ?z ?x) (* (/ ?x ?c1) ?c1))", "(< (+ (% ?x ?c1) ?z) 0)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule278, "(< (+ (* (/ ?x ?c1) ?c1) ?w) ?x)", "(< ?w (% ?x ?c1))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule279, "(< (+ ?w (* (/ ?x ?c1) ?c1)) ?x)", "(< ?w (% ?x ?c1))", "(> ?c1 0)", "T"}
conditional_rule! {ltrule280, "(< ?x (+ (* (/ ?x ?c1) ?c1) ?w))", "(< (% ?x ?c1) ?w)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule281, "(< ?x (+ ?w (* (/ ?x ?c1) ?c1)))", "(< (% ?x ?c1) ?w)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule284, "(< (* (/ ?x ?c1) ?c1) ?x)", "(!= (% ?x ?c1) 0)", "(> ?c1 0)", "T"}
conditional_rule! {ltrule285, "(< ?x (* (/ ?x ?c1) ?c1))", "F", "(> ?c1 0)", "T"}
conditional_rule! {ltrule288, "(< (/ (+ ?x ?c1) ?c0) (/ (+ ?x ?c2) ?c0))", "F", "(>= (> ?c0 (&& 0 ?c1)) ?c2)", "T"}
conditional_rule! {ltrule289, "(< (/ (+ ?x ?c1) ?c0) (/ (+ ?x ?c2) ?c0))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- ?c2 ?c0))", "T"}
conditional_rule! {ltrule291, "(< (/ ?x ?c0) (/ (+ ?x ?c2) ?c0))", "F", "(>= (> ?c0 (&& 0 0)) ?c2)", "T"}
conditional_rule! {ltrule292, "(< (/ ?x ?c0) (/ (+ ?x ?c2) ?c0))", "T", "(<= (> ?c0 (&& 0 0)) (- ?c2 ?c0))", "T"}
conditional_rule! {ltrule294, "(< (/ (+ ?x ?c1) ?c0) (/ ?x ?c0))", "F", "(>= (> ?c0 (&& 0 ?c1)) 0)", "T"}
conditional_rule! {ltrule295, "(< (/ (+ ?x ?c1) ?c0) (/ ?x ?c0))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- 0 ?c0))", "T"}
conditional_rule! {ltrule298, "(< (/ (+ ?x ?c1) ?c0) (+ (/ ?x ?c0) ?c2))", "F", "(>= (> ?c0 (&& 0 ?c1)) (* ?c2 ?c0))", "T"}
conditional_rule! {ltrule299, "(< (/ (+ ?x ?c1) ?c0) (+ (/ ?x ?c0) ?c2))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- (* ?c2 ?c0) ?c0))", "T"}
conditional_rule! {ltrule302, "(< (/ (+ ?x ?c1) ?c0) (+ (min (/ ?x ?c0) ?y) ?c2))", "F", "(>= (> ?c0 (&& 0 ?c1)) (* ?c2 ?c0))", "T"}
conditional_rule! {ltrule303, "(< (/ (+ ?x ?c1) ?c0) (+ (max (/ ?x ?c0) ?y) ?c2))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- (* ?c2 ?c0) ?c0))", "T"}
conditional_rule! {ltrule304, "(< (/ (+ ?x ?c1) ?c0) (min (/ (+ ?x ?c2) ?c0) ?y))", "F", "(>= (> ?c0 (&& 0 ?c1)) ?c2)", "T"}
conditional_rule! {ltrule305, "(< (/ (+ ?x ?c1) ?c0) (max (/ (+ ?x ?c2) ?c0) ?y))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- ?c2 ?c0))", "T"}
conditional_rule! {ltrule306, "(< (/ (+ ?x ?c1) ?c0) (min (/ ?x ?c0) ?y))", "F", "(>= (> ?c0 (&& 0 ?c1)) 0)", "T"}
conditional_rule! {ltrule307, "(< (/ (+ ?x ?c1) ?c0) (max (/ ?x ?c0) ?y))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- 0 ?c0))", "T"}
conditional_rule! {ltrule309, "(< (/ (+ ?x ?c1) ?c0) (+ (min ?y (/ ?x ?c0)) ?c2))", "F", "(>= (> ?c0 (&& 0 ?c1)) (* ?c2 ?c0))", "T"}
conditional_rule! {ltrule310, "(< (/ (+ ?x ?c1) ?c0) (+ (max ?y (/ ?x ?c0)) ?c2))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- (* ?c2 ?c0) ?c0))", "T"}
conditional_rule! {ltrule311, "(< (/ (+ ?x ?c1) ?c0) (min ?y (/ (+ ?x ?c2) ?c0)))", "F", "(>= (> ?c0 (&& 0 ?c1)) ?c2)", "T"}
conditional_rule! {ltrule312, "(< (/ (+ ?x ?c1) ?c0) (max ?y (/ (+ ?x ?c2) ?c0)))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- ?c2 ?c0))", "T"}
conditional_rule! {ltrule313, "(< (/ (+ ?x ?c1) ?c0) (min ?y (/ ?x ?c0)))", "F", "(>= (> ?c0 (&& 0 ?c1)) 0)", "T"}
conditional_rule! {ltrule314, "(< (/ (+ ?x ?c1) ?c0) (max ?y (/ ?x ?c0)))", "T", "(<= (> ?c0 (&& 0 ?c1)) (- 0 ?c0))", "T"}
conditional_rule! {ltrule316, "(< (max (/ (+ ?x ?c2) ?c0) ?y) (/ (+ ?x ?c1) ?c0))", "F", "(>= (> ?c0 (&& 0 ?c2)) ?c1)", "T"}
conditional_rule! {ltrule317, "(< (min (/ (+ ?x ?c2) ?c0) ?y) (/ (+ ?x ?c1) ?c0))", "T", "(<= (> ?c0 (&& 0 ?c2)) (- ?c1 ?c0))", "T"}
conditional_rule! {ltrule318, "(< (max (/ ?x ?c0) ?y) (/ (+ ?x ?c1) ?c0))", "F", "(>= (> ?c0 (&& 0 0)) ?c1)", "T"}
conditional_rule! {ltrule319, "(< (min (/ ?x ?c0) ?y) (/ (+ ?x ?c1) ?c0))", "T", "(<= (> ?c0 (&& 0 0)) (- ?c1 ?c0))", "T"}
conditional_rule! {ltrule320, "(< (max ?y (/ (+ ?x ?c2) ?c0)) (/ (+ ?x ?c1) ?c0))", "F", "(>= (> ?c0 (&& 0 ?c2)) ?c1)", "T"}
conditional_rule! {ltrule321, "(< (min ?y (/ (+ ?x ?c2) ?c0)) (/ (+ ?x ?c1) ?c0))", "T", "(<= (> ?c0 (&& 0 ?c2)) (- ?c1 ?c0))", "T"}
conditional_rule! {ltrule322, "(< (max ?y (/ ?x ?c0)) (/ (+ ?x ?c1) ?c0))", "F", "(>= (> ?c0 (&& 0 0)) ?c1)", "T"}
conditional_rule! {ltrule323, "(< (min ?y (/ ?x ?c0)) (/ (+ ?x ?c1) ?c0))", "T", "(<= (> ?c0 (&& 0 0)) (- ?c1 ?c0))", "T"}
conditional_rule! {ltrule326, "(< (max (/ (+ ?x ?c2) ?c0) ?y) (+ (/ ?x ?c0) ?c1))", "F", "(>= (> ?c0 (&& 0 ?c2)) (* ?c1 ?c0))", "T"}
conditional_rule! {ltrule327, "(< (min (/ (+ ?x ?c2) ?c0) ?y) (+ (/ ?x ?c0) ?c1))", "T", "(<= (> ?c0 (&& 0 ?c2)) (- (* ?c1 ?c0) ?c0))", "T"}
conditional_rule! {ltrule328, "(< (max ?y (/ (+ ?x ?c2) ?c0)) (+ (/ ?x ?c0) ?c1))", "F", "(>= (> ?c0 (&& 0 ?c2)) (* ?c1 ?c0))", "T"}
conditional_rule! {ltrule329, "(< (min ?y (/ (+ ?x ?c2) ?c0)) (+ (/ ?x ?c0) ?c1))", "T", "(<= (> ?c0 (&& 0 ?c2)) (- (* ?c1 ?c0) ?c0))", "T"}
conditional_rule! {ltrule332, "(< (/ ?x ?c0) (min (/ (+ ?x ?c2) ?c0) ?y))", "F", "(< (> ?c0 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {ltrule333, "(< (/ ?x ?c0) (max (/ (+ ?x ?c2) ?c0) ?y))", "T", "(<= (> ?c0 (&& 0 ?c0)) ?c2)", "T"}
conditional_rule! {ltrule334, "(< (/ ?x ?c0) (min ?y (/ (+ ?x ?c2) ?c0)))", "F", "(< (> ?c0 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {ltrule335, "(< (/ ?x ?c0) (max ?y (/ (+ ?x ?c2) ?c0)))", "T", "(<= (> ?c0 (&& 0 ?c0)) ?c2)", "T"}
conditional_rule! {ltrule336, "(< (max (/ (+ ?x ?c2) ?c0) ?y) (/ ?x ?c0))", "F", "(>= (> ?c0 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {ltrule337, "(< (min (/ (+ ?x ?c2) ?c0) ?y) (/ ?x ?c0))", "T", "(<= (> ?c0 (&& 0 (+ ?c2 ?c0))) 0)", "T"}
conditional_rule! {ltrule338, "(< (max ?y (/ (+ ?x ?c2) ?c0)) (/ ?x ?c0))", "F", "(>= (> ?c0 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {ltrule339, "(< (min ?y (/ (+ ?x ?c2) ?c0)) (/ ?x ?c0))", "T", "(<= (> ?c0 (&& 0 (+ ?c2 ?c0))) 0)", "T"}
conditional_rule! {ltrule342, "(< (min ?x ?c0) (min ?x ?c1))", "F", "(>= ?c0 ?c1)", "T"}
conditional_rule! {ltrule343, "(< (min ?x ?c0) (+ (min ?x ?c1) ?c2))", "F", "(>= ?c0 (+ ?c1 ?c2))", "T"}
conditional_rule! {ltrule344, "(< (max ?x ?c0) (max ?x ?c1))", "F", "(>= ?c0 ?c1)", "T"}
conditional_rule! {ltrule345, "(< (max ?x ?c0) (+ (max ?x ?c1) ?c2))", "F", "(>= ?c0 (+ ?c1 ?c2))", "T"}
rule! {maxrule45, "(max ?x ?x)", "?x"}
rule! {maxrule46, "(max ?c0 ?c1)", "(max ?c0 ?c1)"}
conditional_rule! {maxrule60, "(max (* (/ ?x ?c0) ?c0) ?x)", "?x", "(> ?c0 0)", "T"}
conditional_rule! {maxrule62, "(max ?x (* (/ ?x ?c0) ?c0))", "?x", "(> ?c0 0)", "T"}
rule! {maxrule64, "(max (max ?x ?y) ?x)", "(max ?x ?y)"}
rule! {maxrule66, "(max (max ?x ?y) ?y)", "(max ?x ?y)"}
rule! {maxrule68, "(max (max (max ?x ?y) ?z) ?x)", "(max (max ?x ?y) ?z)"}
rule! {maxrule70, "(max (max (max ?x ?y) ?z) ?y)", "(max (max ?x ?y) ?z)"}
rule! {maxrule72, "(max (max (max (max ?x ?y) ?z) ?w) ?x)", "(max (max (max ?x ?y) ?z) ?w)"}
rule! {maxrule74, "(max (max (max (max ?x ?y) ?z) ?w) ?y)", "(max (max (max ?x ?y) ?z) ?w)"}
rule! {maxrule76, "(max (max (max (max (max ?x ?y) ?z) ?w) ?u) ?x)", "(max (max (max (max ?x ?y) ?z) ?w) ?u)"}
rule! {maxrule78, "(max (max (max (max (max ?x ?y) ?z) ?w) ?u) ?y)", "(max (max (max (max ?x ?y) ?z) ?w) ?u)"}
rule! {maxrule80, "(max ?x (min ?x ?y))", "?x"}
rule! {maxrule82, "(max ?x (min ?y ?x))", "?x"}
rule! {maxrule84, "(max (max ?x ?y) (min ?x ?y))", "(max ?x ?y)"}
rule! {maxrule86, "(max (max ?x ?y) (min ?y ?x))", "(max ?x ?y)"}
rule! {maxrule88, "(max (min ?x ?y) ?x)", "?x"}
rule! {maxrule90, "(max (min ?y ?x) ?x)", "?x"}
conditional_rule! {maxrule92, "(max (min ?x ?c0) ?c1)", "?c1", "(>= ?c1 ?c0)", "T"}
conditional_rule! {maxrule110, "(max (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2) ?x)", "(+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2)", "(>= (> ?c1 (&& 0 (+ ?c0 ?c2))) (- ?c1 1))", "T"}
conditional_rule! {maxrule112, "(max ?x (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2))", "(+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2)", "(>= (> ?c1 (&& 0 (+ ?c0 ?c2))) (- ?c1 1))", "T"}
conditional_rule! {maxrule114, "(max (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2) ?x)", "?x", "(<= (> ?c1 (&& 0 (+ ?c0 ?c2))) 0)", "T"}
conditional_rule! {maxrule116, "(max ?x (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2))", "?x", "(<= (> ?c1 (&& 0 (+ ?c0 ?c2))) 0)", "T"}
conditional_rule! {maxrule119, "(max (+ (* (/ ?x ?c1) ?c1) ?c2) ?x)", "(+ (* (/ ?x ?c1) ?c1) ?c2)", "(>= (> ?c1 (&& 0 ?c2)) (- ?c1 1))", "T"}
conditional_rule! {maxrule121, "(max ?x (+ (* (/ ?x ?c1) ?c1) ?c2))", "(+ (* (/ ?x ?c1) ?c1) ?c2)", "(>= (> ?c1 (&& 0 ?c2)) (- ?c1 1))", "T"}
conditional_rule! {maxrule123, "(max (* (/ (+ ?x ?c0) ?c1) ?c1) ?x)", "(* (/ (+ ?x ?c0) ?c1) ?c1)", "(>= (> ?c1 (&& 0 ?c0)) (- ?c1 1))", "T"}
conditional_rule! {maxrule125, "(max ?x (* (/ (+ ?x ?c0) ?c1) ?c1))", "(* (/ (+ ?x ?c0) ?c1) ?c1)", "(>= (> ?c1 (&& 0 ?c0)) (- ?c1 1))", "T"}
conditional_rule! {maxrule127, "(max (+ (* (/ ?x ?c1) ?c1) ?c2) ?x)", "?x", "(<= (> ?c1 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {maxrule129, "(max ?x (+ (* (/ ?x ?c1) ?c1) ?c2))", "?x", "(<= (> ?c1 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {maxrule131, "(max (* (/ (+ ?x ?c0) ?c1) ?c1) ?x)", "?x", "(<= (> ?c1 (&& 0 ?c0)) 0)", "T"}
rule! {maxrule138, "(max (max ?x ?c0) ?c1)", "(max ?x (max ?c0 ?c1))"}
rule! {maxrule139, "(max (max ?x ?c0) ?y)", "(max (max ?x ?y) ?c0)"}
rule! {maxrule140, "(max (max ?x ?y) (max ?x ?z))", "(max (max ?y ?z) ?x)"}
rule! {maxrule141, "(max (max ?y ?x) (max ?x ?z))", "(max (max ?y ?z) ?x)"}
rule! {maxrule142, "(max (max ?x ?y) (max ?z ?x))", "(max (max ?y ?z) ?x)"}
rule! {maxrule143, "(max (max ?y ?x) (max ?z ?x))", "(max (max ?y ?z) ?x)"}
rule! {maxrule144, "(max (max ?x ?y) (max ?z ?w))", "(max (max (max ?x ?y) ?z) ?w)"}
rule! {maxrule149, "(max (min ?x ?y) (min ?x ?z))", "(min ?x (max ?y ?z))"}
rule! {maxrule150, "(max (min ?x ?y) (min ?z ?x))", "(min ?x (max ?y ?z))"}
rule! {maxrule151, "(max (min ?y ?x) (min ?x ?z))", "(min (max ?y ?z) ?x)"}
rule! {maxrule152, "(max (min ?y ?x) (min ?z ?x))", "(min (max ?y ?z) ?x)"}
rule! {maxrule153, "(max (min (max ?x ?y) ?z) ?y)", "(max (min ?x ?z) ?y)"}
rule! {maxrule154, "(max (min (max ?y ?x) ?z) ?y)", "(max ?y (min ?x ?z))"}
rule! {maxrule155, "(max (max ?x ?c0) ?c1)", "(max ?x (max ?c0 ?c1))"}
conditional_rule! {maxrule158, "(max (+ (max ?x ?y) ?c0) ?x)", "(max ?x (+ ?y ?c0))", "(< ?c0 0)", "T"}
conditional_rule! {maxrule159, "(max (+ (max ?x ?y) ?c0) ?x)", "(+ (max ?x ?y) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {maxrule160, "(max (+ (max ?y ?x) ?c0) ?x)", "(max (+ ?y ?c0) ?x)", "(< ?c0 0)", "T"}
conditional_rule! {maxrule161, "(max (+ (max ?y ?x) ?c0) ?x)", "(+ (max ?y ?x) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {maxrule163, "(max ?x (+ (max ?x ?y) ?c0))", "(max ?x (+ ?y ?c0))", "(< ?c0 0)", "T"}
conditional_rule! {maxrule164, "(max ?x (+ (max ?x ?y) ?c0))", "(+ (max ?x ?y) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {maxrule165, "(max ?x (+ (max ?y ?x) ?c0))", "(max ?x (+ ?y ?c0))", "(< ?c0 0)", "T"}
conditional_rule! {maxrule166, "(max ?x (+ (max ?y ?x) ?c0))", "(+ (max ?x ?y) ?c0)", "(> ?c0 0)", "T"}
rule! {maxrule168, "(max (+ ?x ?c0) ?c1)", "(+ (max ?x (- ?c1 ?c0)) ?c0)"}
conditional_rule! {maxrule170, "(max (+ ?x ?c0) (+ ?y ?c1))", "(+ (max ?x (+ ?y (- ?c1 ?c0))) ?c0)", "(> ?c1 ?c0)", "T"}
conditional_rule! {maxrule171, "(max (+ ?x ?c0) (+ ?y ?c1))", "(+ (max (+ ?x (- ?c0 ?c1)) ?y) ?c1)", "(> ?c0 ?c1)", "T"}
rule! {maxrule173, "(max (+ ?x ?y) (+ ?x ?z))", "(+ ?x (max ?y ?z))"}
rule! {maxrule174, "(max (+ ?x ?y) (+ ?z ?x))", "(+ ?x (max ?y ?z))"}
rule! {maxrule175, "(max (+ ?y ?x) (+ ?x ?z))", "(+ (max ?y ?z) ?x)"}
rule! {maxrule176, "(max (+ ?y ?x) (+ ?z ?x))", "(+ (max ?y ?z) ?x)"}
rule! {maxrule177, "(max ?x (+ ?x ?z))", "(+ ?x (max ?z 0))"}
rule! {maxrule178, "(max ?x (+ ?z ?x))", "(+ ?x (max ?z 0))"}
rule! {maxrule179, "(max (+ ?y ?x) ?x)", "(+ (max ?y 0) ?x)"}
rule! {maxrule180, "(max (+ ?x ?y) ?x)", "(+ ?x (max ?y 0))"}
conditional_rule! {maxrule182, "(max (* (+ (* ?x ?c0) ?y) ?c1) (+ (* ?x ?c2) ?z))", "(+ (max (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
conditional_rule! {maxrule183, "(max (* (+ ?y (* ?x ?c0)) ?c1) (+ (* ?x ?c2) ?z))", "(+ (max (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
conditional_rule! {maxrule184, "(max (* (+ (* ?x ?c0) ?y) ?c1) (+ ?z (* ?x ?c2)))", "(+ (max (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
conditional_rule! {maxrule185, "(max (* (+ ?y (* ?x ?c0)) ?c1) (+ ?z (* ?x ?c2)))", "(+ (max (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
rule! {maxrule187, "(max (max (+ ?x ?y) ?z) (+ ?x ?w))", "(max (+ ?x (max ?y ?w)) ?z)"}
rule! {maxrule188, "(max (max ?z (+ ?x ?y)) (+ ?x ?w))", "(max (+ ?x (max ?y ?w)) ?z)"}
rule! {maxrule189, "(max (max (+ ?x ?y) ?z) (+ ?w ?x))", "(max (+ ?x (max ?y ?w)) ?z)"}
rule! {maxrule190, "(max (max ?z (+ ?x ?y)) (+ ?w ?x))", "(max (+ ?x (max ?y ?w)) ?z)"}
rule! {maxrule192, "(max (max (+ ?y ?x) ?z) (+ ?x ?w))", "(max (+ (max ?y ?w) ?x) ?z)"}
rule! {maxrule193, "(max (max ?z (+ ?y ?x)) (+ ?x ?w))", "(max (+ (max ?y ?w) ?x) ?z)"}
rule! {maxrule194, "(max (max (+ ?y ?x) ?z) (+ ?w ?x))", "(max (+ (max ?y ?w) ?x) ?z)"}
rule! {maxrule195, "(max (max ?z (+ ?y ?x)) (+ ?w ?x))", "(max (+ (max ?y ?w) ?x) ?z)"}
rule! {maxrule197, "(max (+ (+ ?x ?w) ?y) (+ ?x ?z))", "(+ ?x (max (+ ?w ?y) ?z))"}
rule! {maxrule198, "(max (+ (+ ?w ?x) ?y) (+ ?x ?z))", "(+ (max (+ ?w ?y) ?z) ?x)"}
rule! {maxrule199, "(max (+ (+ ?x ?w) ?y) (+ ?z ?x))", "(+ ?x (max (+ ?w ?y) ?z))"}
rule! {maxrule200, "(max (+ (+ ?w ?x) ?y) (+ ?z ?x))", "(+ (max (+ ?w ?y) ?z) ?x)"}
rule! {maxrule201, "(max (+ (+ ?x ?w) ?y) ?x)", "(+ ?x (max (+ ?w ?y) 0))"}
rule! {maxrule202, "(max (+ (+ ?w ?x) ?y) ?x)", "(+ ?x (max (+ ?w ?y) 0))"}
rule! {maxrule203, "(max (+ ?x ?y) (+ (+ ?w ?x) ?z))", "(+ ?x (max (+ ?w ?z) ?y))"}
rule! {maxrule204, "(max (+ ?x ?y) (+ (+ ?x ?w) ?z))", "(+ ?x (max (+ ?w ?z) ?y))"}
rule! {maxrule205, "(max (+ ?y ?x) (+ (+ ?w ?x) ?z))", "(+ (max (+ ?w ?z) ?y) ?x)"}
rule! {maxrule206, "(max (+ ?y ?x) (+ (+ ?x ?w) ?z))", "(+ (max (+ ?w ?z) ?y) ?x)"}
rule! {maxrule207, "(max ?x (+ (+ ?w ?x) ?z))", "(+ ?x (max (+ ?w ?z) 0))"}
rule! {maxrule208, "(max ?x (+ (+ ?x ?w) ?z))", "(+ ?x (max (+ ?w ?z) 0))"}
rule! {maxrule210, "(max (- ?y ?x) (- ?z ?x))", "(- (max ?y ?z) ?x)"}
rule! {maxrule211, "(max (- ?x ?y) (- ?x ?z))", "(- ?x (min ?y ?z))"}
rule! {maxrule213, "(max ?x (- ?x ?y))", "(- ?x (min 0 ?y))"}
rule! {maxrule214, "(max (- ?x ?y) ?x)", "(- ?x (min 0 ?y))"}
rule! {maxrule215, "(max ?x (+ (- ?x ?y) ?z))", "(+ ?x (max 0 (- ?z ?y)))"}
rule! {maxrule216, "(max ?x (+ ?z (- ?x ?y)))", "(+ ?x (max 0 (- ?z ?y)))"}
rule! {maxrule217, "(max ?x (- (- ?x ?y) ?z))", "(- ?x (min 0 (+ ?y ?z)))"}
rule! {maxrule218, "(max (+ (- ?x ?y) ?z) ?x)", "(+ (max 0 (- ?z ?y)) ?x)"}
rule! {maxrule219, "(max (+ ?z (- ?x ?y)) ?x)", "(+ (max 0 (- ?z ?y)) ?x)"}
rule! {maxrule220, "(max (- (- ?x ?y) ?z) ?x)", "(- ?x (min 0 (+ ?y ?z)))"}
conditional_rule! {maxrule222, "(max (* ?x ?c0) ?c1)", "(* (max ?x (/ ?c1 ?c0)) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {maxrule223, "(max (* ?x ?c0) ?c1)", "(* (min ?x (/ ?c1 ?c0)) ?c0)", "(= (< ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {maxrule225, "(max (* ?x ?c0) (* ?y ?c1))", "(* (max ?x (* ?y (/ ?c1 ?c0))) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {maxrule226, "(max (* ?x ?c0) (* ?y ?c1))", "(* (min ?x (* ?y (/ ?c1 ?c0))) ?c0)", "(= (< ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {maxrule227, "(max (* ?x ?c0) (* ?y ?c1))", "(* (max (* ?x (/ ?c0 ?c1)) ?y) ?c1)", "(= (> ?c1 (&& 0 (% ?c0 ?c1))) 0)", "T"}
conditional_rule! {maxrule228, "(max (* ?x ?c0) (* ?y ?c1))", "(* (min (* ?x (/ ?c0 ?c1)) ?y) ?c1)", "(= (< ?c1 (&& 0 (% ?c0 ?c1))) 0)", "T"}
conditional_rule! {maxrule229, "(max (* ?x ?c0) (+ (* ?y ?c0) ?c1))", "(* (max ?x (+ ?y (/ ?c1 ?c0))) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {maxrule230, "(max (* ?x ?c0) (+ (* ?y ?c0) ?c1))", "(* (min ?x (+ ?y (/ ?c1 ?c0))) ?c0)", "(= (< ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {maxrule232, "(max (/ ?x ?c0) (/ ?y ?c0))", "(/ (max ?x ?y) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {maxrule233, "(max (/ ?x ?c0) (/ ?y ?c0))", "(/ (min ?x ?y) ?c0)", "(< ?c0 0)", "T"}
rule! {maxrule243, "(max (select ?x ?y ?z) (select ?x ?w ?u))", "(select ?x (max ?y ?w) (max ?z ?u))"}
rule! {minrule45, "(min ?x ?x)", "?x"}
rule! {minrule46, "(min ?c0 ?c1)", "(min ?c0 ?c1)"}
conditional_rule! {minrule60, "(min (* (/ ?x ?c0) ?c0) ?x)", "(* (/ ?x ?c0) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {minrule62, "(min ?x (* (/ ?x ?c0) ?c0))", "(* (/ ?x ?c0) ?c0)", "(> ?c0 0)", "T"}
rule! {minrule64, "(min (min ?x ?y) ?x)", "(min ?x ?y)"}
rule! {minrule66, "(min (min ?x ?y) ?y)", "(min ?x ?y)"}
rule! {minrule68, "(min (min (min ?x ?y) ?z) ?x)", "(min (min ?x ?y) ?z)"}
rule! {minrule70, "(min (min (min ?x ?y) ?z) ?y)", "(min (min ?x ?y) ?z)"}
rule! {minrule72, "(min (min (min (min ?x ?y) ?z) ?w) ?x)", "(min (min (min ?x ?y) ?z) ?w)"}
rule! {minrule74, "(min (min (min (min ?x ?y) ?z) ?w) ?y)", "(min (min (min ?x ?y) ?z) ?w)"}
rule! {minrule76, "(min (min (min (min (min ?x ?y) ?z) ?w) ?u) ?x)", "(min (min (min (min ?x ?y) ?z) ?w) ?u)"}
rule! {minrule78, "(min (min (min (min (min ?x ?y) ?z) ?w) ?u) ?y)", "(min (min (min (min ?x ?y) ?z) ?w) ?u)"}
rule! {minrule80, "(min ?x (max ?x ?y))", "?x"}
rule! {minrule82, "(min ?x (max ?y ?x))", "?x"}
rule! {minrule84, "(min (max ?x ?y) (min ?x ?y))", "(min ?x ?y)"}
rule! {minrule86, "(min (max ?x ?y) (min ?y ?x))", "(min ?y ?x)"}
rule! {minrule88, "(min (max ?x ?y) ?x)", "?x"}
rule! {minrule90, "(min (max ?y ?x) ?x)", "?x"}
conditional_rule! {minrule92, "(min (max ?x ?c0) ?c1)", "?c1", "(<= ?c1 ?c0)", "T"}
conditional_rule! {minrule110, "(min (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2) ?x)", "?x", "(>= (> ?c1 (&& 0 (+ ?c0 ?c2))) (- ?c1 1))", "T"}
conditional_rule! {minrule112, "(min ?x (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2))", "?x", "(>= (> ?c1 (&& 0 (+ ?c0 ?c2))) (- ?c1 1))", "T"}
conditional_rule! {minrule114, "(min (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2) ?x)", "(+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2)", "(<= (> ?c1 (&& 0 (+ ?c0 ?c2))) 0)", "T"}
conditional_rule! {minrule116, "(min ?x (+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2))", "(+ (* (/ (+ ?x ?c0) ?c1) ?c1) ?c2)", "(<= (> ?c1 (&& 0 (+ ?c0 ?c2))) 0)", "T"}
conditional_rule! {minrule119, "(min (+ (* (/ ?x ?c1) ?c1) ?c2) ?x)", "?x", "(>= (> ?c1 (&& 0 ?c2)) (- ?c1 1))", "T"}
conditional_rule! {minrule121, "(min ?x (+ (* (/ ?x ?c1) ?c1) ?c2))", "?x", "(>= (> ?c1 (&& 0 ?c2)) (- ?c1 1))", "T"}
conditional_rule! {minrule123, "(min (* (/ (+ ?x ?c0) ?c1) ?c1) ?x)", "?x", "(>= (> ?c1 (&& 0 ?c0)) (- ?c1 1))", "T"}
conditional_rule! {minrule125, "(min ?x (* (/ (+ ?x ?c0) ?c1) ?c1))", "?x", "(>= (> ?c1 (&& 0 ?c0)) (- ?c1 1))", "T"}
conditional_rule! {minrule127, "(min (+ (* (/ ?x ?c1) ?c1) ?c2) ?x)", "(+ (* (/ ?x ?c1) ?c1) ?c2)", "(<= (> ?c1 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {minrule129, "(min ?x (+ (* (/ ?x ?c1) ?c1) ?c2))", "(+ (* (/ ?x ?c1) ?c1) ?c2)", "(<= (> ?c1 (&& 0 ?c2)) 0)", "T"}
conditional_rule! {minrule131, "(min (* (/ (+ ?x ?c0) ?c1) ?c1) ?x)", "(* (/ (+ ?x ?c0) ?c1) ?c1)", "(<= (> ?c1 (&& 0 ?c0)) 0)", "T"}
rule! {minrule138, "(min (min ?x ?c0) ?c1)", "(min ?x (min ?c0 ?c1))"}
rule! {minrule139, "(min (min ?x ?c0) ?y)", "(min (min ?x ?y) ?c0)"}
rule! {minrule140, "(min (min ?x ?y) (min ?x ?z))", "(min (min ?y ?z) ?x)"}
rule! {minrule141, "(min (min ?y ?x) (min ?x ?z))", "(min (min ?y ?z) ?x)"}
rule! {minrule142, "(min (min ?x ?y) (min ?z ?x))", "(min (min ?y ?z) ?x)"}
rule! {minrule143, "(min (min ?y ?x) (min ?z ?x))", "(min (min ?y ?z) ?x)"}
rule! {minrule144, "(min (min ?x ?y) (min ?z ?w))", "(min (min (min ?x ?y) ?z) ?w)"}
rule! {minrule149, "(min (max ?x ?y) (max ?x ?z))", "(max ?x (min ?y ?z))"}
rule! {minrule150, "(min (max ?x ?y) (max ?z ?x))", "(max ?x (min ?y ?z))"}
rule! {minrule151, "(min (max ?y ?x) (max ?x ?z))", "(max (min ?y ?z) ?x)"}
rule! {minrule152, "(min (max ?y ?x) (max ?z ?x))", "(max (min ?y ?z) ?x)"}
rule! {minrule153, "(min (max (min ?x ?y) ?z) ?y)", "(min (max ?x ?z) ?y)"}
rule! {minrule154, "(min (max (min ?y ?x) ?z) ?y)", "(min ?y (max ?x ?z))"}
rule! {minrule155, "(min (min ?x ?c0) ?c1)", "(min ?x (min ?c0 ?c1))"}
conditional_rule! {minrule158, "(min (max ?x ?c0) ?c1)", "(max (min ?x ?c1) ?c0)", "(<= ?c0 ?c1)", "T"}
conditional_rule! {minrule161, "(min (+ (min ?x ?y) ?c0) ?x)", "(min ?x (+ ?y ?c0))", "(> ?c0 0)", "T"}
conditional_rule! {minrule162, "(min (+ (min ?x ?y) ?c0) ?x)", "(+ (min ?x ?y) ?c0)", "(< ?c0 0)", "T"}
conditional_rule! {minrule163, "(min (+ (min ?y ?x) ?c0) ?x)", "(min (+ ?y ?c0) ?x)", "(> ?c0 0)", "T"}
conditional_rule! {minrule164, "(min (+ (min ?y ?x) ?c0) ?x)", "(+ (min ?y ?x) ?c0)", "(< ?c0 0)", "T"}
conditional_rule! {minrule166, "(min ?x (+ (min ?x ?y) ?c0))", "(min ?x (+ ?y ?c0))", "(> ?c0 0)", "T"}
conditional_rule! {minrule167, "(min ?x (+ (min ?x ?y) ?c0))", "(+ (min ?x ?y) ?c0)", "(< ?c0 0)", "T"}
conditional_rule! {minrule168, "(min ?x (+ (min ?y ?x) ?c0))", "(min ?x (+ ?y ?c0))", "(> ?c0 0)", "T"}
conditional_rule! {minrule169, "(min ?x (+ (min ?y ?x) ?c0))", "(+ (min ?x ?y) ?c0)", "(< ?c0 0)", "T"}
rule! {minrule171, "(min (+ ?x ?c0) ?c1)", "(+ (min ?x (- ?c1 ?c0)) ?c0)"}
conditional_rule! {minrule173, "(min (+ ?x ?c0) (+ ?y ?c1))", "(+ (min ?x (+ ?y (- ?c1 ?c0))) ?c0)", "(> ?c1 ?c0)", "T"}
conditional_rule! {minrule174, "(min (+ ?x ?c0) (+ ?y ?c1))", "(+ (min (+ ?x (- ?c0 ?c1)) ?y) ?c1)", "(> ?c0 ?c1)", "T"}
rule! {minrule176, "(min (+ ?x ?y) (+ ?x ?z))", "(+ ?x (min ?y ?z))"}
rule! {minrule177, "(min (+ ?x ?y) (+ ?z ?x))", "(+ ?x (min ?y ?z))"}
rule! {minrule178, "(min (+ ?y ?x) (+ ?x ?z))", "(+ (min ?y ?z) ?x)"}
rule! {minrule179, "(min (+ ?y ?x) (+ ?z ?x))", "(+ (min ?y ?z) ?x)"}
rule! {minrule180, "(min ?x (+ ?x ?z))", "(+ ?x (min ?z 0))"}
rule! {minrule181, "(min ?x (+ ?z ?x))", "(+ ?x (min ?z 0))"}
rule! {minrule182, "(min (+ ?y ?x) ?x)", "(+ (min ?y 0) ?x)"}
rule! {minrule183, "(min (+ ?x ?y) ?x)", "(+ ?x (min ?y 0))"}
conditional_rule! {minrule185, "(min (* (+ (* ?x ?c0) ?y) ?c1) (+ (* ?x ?c2) ?z))", "(+ (min (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
conditional_rule! {minrule186, "(min (* (+ ?y (* ?x ?c0)) ?c1) (+ (* ?x ?c2) ?z))", "(+ (min (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
conditional_rule! {minrule187, "(min (* (+ (* ?x ?c0) ?y) ?c1) (+ ?z (* ?x ?c2)))", "(+ (min (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
conditional_rule! {minrule188, "(min (* (+ ?y (* ?x ?c0)) ?c1) (+ ?z (* ?x ?c2)))", "(+ (min (* ?y ?c1) ?z) (* ?x ?c2))", "(= (* ?c0 ?c1) ?c2)", "T"}
rule! {minrule190, "(min (min (+ ?x ?y) ?z) (+ ?x ?w))", "(min (+ ?x (min ?y ?w)) ?z)"}
rule! {minrule191, "(min (min ?z (+ ?x ?y)) (+ ?x ?w))", "(min (+ ?x (min ?y ?w)) ?z)"}
rule! {minrule192, "(min (min (+ ?x ?y) ?z) (+ ?w ?x))", "(min (+ ?x (min ?y ?w)) ?z)"}
rule! {minrule193, "(min (min ?z (+ ?x ?y)) (+ ?w ?x))", "(min (+ ?x (min ?y ?w)) ?z)"}
rule! {minrule195, "(min (min (+ ?y ?x) ?z) (+ ?x ?w))", "(min (+ (min ?y ?w) ?x) ?z)"}
rule! {minrule196, "(min (min ?z (+ ?y ?x)) (+ ?x ?w))", "(min (+ (min ?y ?w) ?x) ?z)"}
rule! {minrule197, "(min (min (+ ?y ?x) ?z) (+ ?w ?x))", "(min (+ (min ?y ?w) ?x) ?z)"}
rule! {minrule198, "(min (min ?z (+ ?y ?x)) (+ ?w ?x))", "(min (+ (min ?y ?w) ?x) ?z)"}
rule! {minrule200, "(min (+ (+ ?x ?w) ?y) (+ ?x ?z))", "(+ ?x (min (+ ?w ?y) ?z))"}
rule! {minrule201, "(min (+ (+ ?w ?x) ?y) (+ ?x ?z))", "(+ (min (+ ?w ?y) ?z) ?x)"}
rule! {minrule202, "(min (+ (+ ?x ?w) ?y) (+ ?z ?x))", "(+ ?x (min (+ ?w ?y) ?z))"}
rule! {minrule203, "(min (+ (+ ?w ?x) ?y) (+ ?z ?x))", "(+ (min (+ ?w ?y) ?z) ?x)"}
rule! {minrule204, "(min (+ (+ ?x ?w) ?y) ?x)", "(+ ?x (min (+ ?w ?y) 0))"}
rule! {minrule205, "(min (+ (+ ?w ?x) ?y) ?x)", "(+ ?x (min (+ ?w ?y) 0))"}
rule! {minrule206, "(min (+ ?x ?y) (+ (+ ?w ?x) ?z))", "(+ ?x (min (+ ?w ?z) ?y))"}
rule! {minrule207, "(min (+ ?x ?y) (+ (+ ?x ?w) ?z))", "(+ ?x (min (+ ?w ?z) ?y))"}
rule! {minrule208, "(min (+ ?y ?x) (+ (+ ?w ?x) ?z))", "(+ (min (+ ?w ?z) ?y) ?x)"}
rule! {minrule209, "(min (+ ?y ?x) (+ (+ ?x ?w) ?z))", "(+ (min (+ ?w ?z) ?y) ?x)"}
rule! {minrule210, "(min ?x (+ (+ ?w ?x) ?z))", "(+ ?x (min (+ ?w ?z) 0))"}
rule! {minrule211, "(min ?x (+ (+ ?x ?w) ?z))", "(+ ?x (min (+ ?w ?z) 0))"}
rule! {minrule213, "(min (- ?y ?x) (- ?z ?x))", "(- (min ?y ?z) ?x)"}
rule! {minrule214, "(min (- ?x ?y) (- ?x ?z))", "(- ?x (max ?y ?z))"}
rule! {minrule216, "(min ?x (- ?x ?y))", "(- ?x (max 0 ?y))"}
rule! {minrule217, "(min (- ?x ?y) ?x)", "(- ?x (max 0 ?y))"}
rule! {minrule218, "(min ?x (+ (- ?x ?y) ?z))", "(+ ?x (min 0 (- ?z ?y)))"}
rule! {minrule219, "(min ?x (+ ?z (- ?x ?y)))", "(+ ?x (min 0 (- ?z ?y)))"}
rule! {minrule220, "(min ?x (- (- ?x ?y) ?z))", "(- ?x (max 0 (+ ?y ?z)))"}
rule! {minrule221, "(min (+ (- ?x ?y) ?z) ?x)", "(+ (min 0 (- ?z ?y)) ?x)"}
rule! {minrule222, "(min (+ ?z (- ?x ?y)) ?x)", "(+ (min 0 (- ?z ?y)) ?x)"}
rule! {minrule223, "(min (- (- ?x ?y) ?z) ?x)", "(- ?x (max 0 (+ ?y ?z)))"}
conditional_rule! {minrule225, "(min (* ?x ?c0) ?c1)", "(* (min ?x (/ ?c1 ?c0)) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {minrule226, "(min (* ?x ?c0) ?c1)", "(* (max ?x (/ ?c1 ?c0)) ?c0)", "(= (< ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {minrule228, "(min (* ?x ?c0) (* ?y ?c1))", "(* (min ?x (* ?y (/ ?c1 ?c0))) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {minrule229, "(min (* ?x ?c0) (* ?y ?c1))", "(* (max ?x (* ?y (/ ?c1 ?c0))) ?c0)", "(= (< ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {minrule230, "(min (* ?x ?c0) (* ?y ?c1))", "(* (min (* ?x (/ ?c0 ?c1)) ?y) ?c1)", "(= (> ?c1 (&& 0 (% ?c0 ?c1))) 0)", "T"}
conditional_rule! {minrule231, "(min (* ?x ?c0) (* ?y ?c1))", "(* (max (* ?x (/ ?c0 ?c1)) ?y) ?c1)", "(= (< ?c1 (&& 0 (% ?c0 ?c1))) 0)", "T"}
conditional_rule! {minrule232, "(min (* ?x ?c0) (+ (* ?y ?c0) ?c1))", "(* (min ?x (+ ?y (/ ?c1 ?c0))) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {minrule233, "(min (* ?x ?c0) (+ (* ?y ?c0) ?c1))", "(* (max ?x (+ ?y (/ ?c1 ?c0))) ?c0)", "(= (< ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {minrule235, "(min (/ ?x ?c0) (/ ?y ?c0))", "(/ (min ?x ?y) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {minrule236, "(min (/ ?x ?c0) (/ ?y ?c0))", "(/ (max ?x ?y) ?c0)", "(< ?c0 0)", "T"}
rule! {minrule246, "(min (select ?x ?y ?z) (select ?x ?w ?u))", "(select ?x (min ?y ?w) (min ?z ?u))"}
rule! {modrule53, "(% 0 ?x)", "0"}
rule! {modrule54, "(% ?x ?x)", "0"}
rule! {modrule58, "(% ?x 1)", "0"}
conditional_rule! {modrule65, "(% (* ?x ?c0) ?c1)", "(% (* ?x (% ?c0 ?c1)) ?c1)", "(> ?c1 (&& 0 (< (>= ?c0 (|| ?c1 ?c0)) 0)))", "T"}
conditional_rule! {modrule66, "(% (+ ?x ?c0) ?c1)", "(% (+ ?x (% ?c0 ?c1)) ?c1)", "(> ?c1 (&& 0 (< (>= ?c0 (|| ?c1 ?c0)) 0)))", "T"}
conditional_rule! {modrule67, "(% (* ?x ?c0) ?c1)", "(* (% ?x (/ ?c1 ?c0)) ?c0)", "(= (> ?c0 (&& 0 (% ?c1 ?c0))) 0)", "T"}
conditional_rule! {modrule68, "(% (+ (* ?x ?c0) ?y) ?c1)", "(% ?y ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
conditional_rule! {modrule69, "(% (+ ?y (* ?x ?c0)) ?c1)", "(% ?y ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
conditional_rule! {modrule70, "(% (- (* ?x ?c0) ?y) ?c1)", "(% (- ?y) ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
conditional_rule! {modrule71, "(% (- ?y (* ?x ?c0)) ?c1)", "(% ?y ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
rule! {modrule72, "(% (- ?x ?y) 2)", "(% (+ ?x ?y) 2)"}
rule! {mulrule56, "(* 0 ?x)", "0"}
rule! {mulrule57, "(* 1 ?x)", "?x"}
rule! {mulrule58, "(* ?x 0)", "0"}
conditional_rule! {mulrule64, "(* (- ?x ?y) ?c0)", "(* (- ?y ?x) (- ?c0))", "(> (< ?c0 (&& 0 (- ?c0))) 0)", "T"}
rule! {mulrule67, "(* ?x (* ?y ?c0))", "(* (* ?x ?y) ?c0)"}
rule! {mulrule68, "(* (max ?x ?y) (min ?x ?y))", "(* ?x ?y)"}
rule! {mulrule69, "(* (max ?x ?y) (min ?y ?x))", "(* ?y ?x)"}
rule! {notrule11, "(not (< ?x ?y))", "(<= ?y ?x)"}
rule! {notrule12, "(not (<= ?x ?y))", "(< ?y ?x)"}
rule! {notrule13, "(not (> ?x ?y))", "(>= ?y ?x)"}
rule! {notrule14, "(not (>= ?x ?y))", "(> ?y ?x)"}
rule! {notrule15, "(not (= ?x ?y))", "(!= ?x ?y)"}
rule! {notrule16, "(not (!= ?x ?y))", "(= ?x ?y)"}
rule! {notrule17, "(not (not ?x))", "?x"}
rule! {orrule21, "(|| ?x T)", "T"}
rule! {orrule22, "(|| ?x F)", "?x"}
rule! {orrule23, "(|| ?x ?x)", "?x"}
rule! {orrule25, "(|| (|| ?x ?y) ?x)", "(|| ?x ?y)"}
rule! {orrule26, "(|| ?x (|| ?x ?y))", "(|| ?x ?y)"}
rule! {orrule27, "(|| (|| ?x ?y) ?y)", "(|| ?x ?y)"}
rule! {orrule28, "(|| ?y (|| ?x ?y))", "(|| ?x ?y)"}
rule! {orrule30, "(|| (|| (|| ?x ?y) ?z) ?x)", "(|| (|| ?x ?y) ?z)"}
rule! {orrule31, "(|| ?x (|| (|| ?x ?y) ?z))", "(|| (|| ?x ?y) ?z)"}
rule! {orrule32, "(|| (|| ?z (|| ?x ?y)) ?x)", "(|| ?z (|| ?x ?y))"}
rule! {orrule33, "(|| ?x (|| ?z (|| ?x ?y)))", "(|| ?z (|| ?x ?y))"}
rule! {orrule34, "(|| (|| (|| ?x ?y) ?z) ?y)", "(|| (|| ?x ?y) ?z)"}
rule! {orrule35, "(|| ?y (|| (|| ?x ?y) ?z))", "(|| (|| ?x ?y) ?z)"}
rule! {orrule36, "(|| (|| ?z (|| ?x ?y)) ?y)", "(|| ?z (|| ?x ?y))"}
rule! {orrule37, "(|| ?y (|| ?z (|| ?x ?y)))", "(|| ?z (|| ?x ?y))"}
rule! {orrule39, "(|| (&& ?x ?y) ?x)", "?x"}
rule! {orrule40, "(|| ?x (&& ?x ?y))", "?x"}
rule! {orrule41, "(|| (&& ?x ?y) ?y)", "?y"}
rule! {orrule42, "(|| ?y (&& ?x ?y))", "?y"}
rule! {orrule44, "(|| (|| (|| ?x ?y) ?z) ?x)", "(|| (|| ?x ?y) ?z)"}
rule! {orrule45, "(|| ?x (|| (|| ?x ?y) ?z))", "(|| (|| ?x ?y) ?z)"}
rule! {orrule46, "(|| (|| ?z (|| ?x ?y)) ?x)", "(|| ?z (|| ?x ?y))"}
rule! {orrule47, "(|| ?x (|| ?z (|| ?x ?y)))", "(|| ?z (|| ?x ?y))"}
rule! {orrule48, "(|| (|| (|| ?x ?y) ?z) ?y)", "(|| (|| ?x ?y) ?z)"}
rule! {orrule49, "(|| ?y (|| (|| ?x ?y) ?z))", "(|| (|| ?x ?y) ?z)"}
rule! {orrule50, "(|| (|| ?z (|| ?x ?y)) ?y)", "(|| ?z (|| ?x ?y))"}
rule! {orrule51, "(|| ?y (|| ?z (|| ?x ?y)))", "(|| ?z (|| ?x ?y))"}
rule! {orrule53, "(!= ?x (= (|| ?y ?x) ?y))", "T"}
rule! {orrule54, "(!= ?x (= (|| ?y ?y) ?x))", "T"}
rule! {orrule55, "(= (|| (!= (|| ?z ?x) ?y) ?x) ?y)", "T"}
rule! {orrule56, "(= (|| (!= (|| ?z ?x) ?y) ?y) ?x)", "T"}
rule! {orrule57, "(= (|| (!= ?x (|| ?y ?z)) ?x) ?y)", "T"}
rule! {orrule58, "(= (|| (!= ?x (|| ?y ?z)) ?y) ?x)", "T"}
rule! {orrule59, "(!= (|| (= (|| ?z ?x) ?y) ?x) ?y)", "T"}
rule! {orrule60, "(!= (|| (= (|| ?z ?x) ?y) ?y) ?x)", "T"}
rule! {orrule61, "(!= (|| (= ?x (|| ?y ?z)) ?x) ?y)", "T"}
rule! {orrule62, "(!= (|| (= ?x (|| ?y ?z)) ?y) ?x)", "T"}
rule! {orrule63, "(|| ?x (not ?x))", "T"}
rule! {orrule64, "(|| (not ?x) ?x)", "T"}
rule! {orrule65, "(< (<= ?y (|| ?x ?x)) ?y)", "T"}
conditional_rule! {orrule69, "(< (<= ?x (|| ?c0 ?c1)) ?x)", "T", "(<= ?c1 ?c0)", "T"}
conditional_rule! {orrule70, "(< (<= ?c1 (|| ?x ?x)) ?c0)", "T", "(<= ?c1 ?c0)", "T"}
conditional_rule! {orrule71, "(< (< ?x (|| ?c0 ?c1)) ?x)", "T", "(< ?c1 ?c0)", "T"}
conditional_rule! {orrule72, "(< (< ?c1 (|| ?x ?x)) ?c0)", "T", "(< ?c1 ?c0)", "T"}
rule! {orrule73, "(< (< ?c0 (|| ?x ?c1)) ?x)", "(< (min ?c0 ?c1) ?x)"}
rule! {orrule74, "(<= (<= ?c0 (|| ?x ?c1)) ?x)", "(<= (min ?c0 ?c1) ?x)"}
rule! {orrule75, "(< (< ?x (|| ?c0 ?x)) ?c1)", "(< ?x (max ?c0 ?c1))"}
rule! {orrule76, "(<= (<= ?x (|| ?c0 ?x)) ?c1)", "(<= ?x (max ?c0 ?c1))"}
rule! {orrule82, "(|| (&& ?x (|| ?y ?z)) ?y)", "(|| (&& ?x ?z) ?y)"}
rule! {orrule83, "(|| (&& ?x (|| ?z ?y)) ?y)", "(|| (&& ?x ?z) ?y)"}
rule! {orrule84, "(|| ?y (&& ?x (|| ?y ?z)))", "(|| ?y (&& ?x ?z))"}
rule! {orrule85, "(|| ?y (&& ?x (|| ?z ?y)))", "(|| ?y (&& ?x ?z))"}
rule! {orrule87, "(|| (&& (|| ?y ?z) ?x) ?y)", "(|| (&& ?z ?x) ?y)"}
rule! {orrule88, "(|| (&& (|| ?z ?y) ?x) ?y)", "(|| (&& ?z ?x) ?y)"}
rule! {orrule89, "(|| ?y (&& (|| ?y ?z) ?x))", "(|| ?y (&& ?z ?x))"}
rule! {orrule90, "(|| ?y (&& (|| ?z ?y) ?x))", "(|| ?y (&& ?z ?x))"}
rule! {orrule92, "(|| (|| ?x (&& ?y ?z)) ?y)", "(|| ?x ?y)"}
rule! {orrule93, "(|| (|| ?x (&& ?z ?y)) ?y)", "(|| ?x ?y)"}
rule! {orrule94, "(|| ?y (|| ?x (&& ?y ?z)))", "(|| ?y ?x)"}
rule! {orrule95, "(|| ?y (|| ?x (&& ?z ?y)))", "(|| ?y ?x)"}
rule! {orrule97, "(|| (|| (&& ?y ?z) ?x) ?y)", "(|| ?x ?y)"}
rule! {orrule98, "(|| (|| (&& ?z ?y) ?x) ?y)", "(|| ?x ?y)"}
rule! {orrule99, "(|| ?y (|| (&& ?y ?z) ?x))", "(|| ?y ?x)"}
rule! {orrule100, "(|| ?y (|| (&& ?z ?y) ?x))", "(|| ?y ?x)"}
rule! {orrule102, "(|| (&& ?x ?y) (&& ?x ?z))", "(&& ?x (|| ?y ?z))"}
rule! {orrule103, "(|| (&& ?x ?y) (&& ?z ?x))", "(&& ?x (|| ?y ?z))"}
rule! {orrule104, "(|| (&& ?y ?x) (&& ?x ?z))", "(&& ?x (|| ?y ?z))"}
rule! {orrule105, "(|| (&& ?y ?x) (&& ?z ?x))", "(&& ?x (|| ?y ?z))"}
rule! {orrule107, "(< (< ?x (|| ?y ?x)) ?z)", "(< ?x (max ?y ?z))"}
rule! {orrule108, "(< (< ?y (|| ?x ?z)) ?x)", "(< (min ?y ?z) ?x)"}
rule! {orrule109, "(<= (<= ?x (|| ?y ?x)) ?z)", "(<= ?x (max ?y ?z))"}
rule! {orrule110, "(<= (<= ?y (|| ?x ?z)) ?x)", "(<= (min ?y ?z) ?x)"}
rule! {selectrule29, "(select 1 ?x ?y)", "?x"}
rule! {selectrule30, "(select 0 ?x ?y)", "?y"}
rule! {selectrule31, "(select ?x ?y ?y)", "?y"}
rule! {selectrule58, "(select (!= ?x ?y) ?z ?w)", "(select (= ?x ?y) ?w ?z)"}
rule! {selectrule59, "(select (<= ?x ?y) ?z ?w)", "(select (< ?y ?x) ?w ?z)"}
rule! {selectrule60, "(select ?x (select ?y ?z ?w) ?z)", "(select (&& ?x (not ?y)) ?w ?z)"}
rule! {selectrule61, "(select ?x (select ?y ?z ?w) ?w)", "(select (&& ?x ?y) ?z ?w)"}
rule! {selectrule62, "(select ?x ?y (select ?z ?y ?w))", "(select (|| ?x ?z) ?y ?w)"}
rule! {selectrule63, "(select ?x ?y (select ?z ?w ?y))", "(select (|| ?x (not ?z)) ?y ?w)"}
rule! {selectrule64, "(select ?x (select ?x ?y ?z) ?w)", "(select ?x ?y ?w)"}
rule! {selectrule65, "(select ?x ?y (select ?x ?z ?w))", "(select ?x ?y ?w)"}
rule! {selectrule66, "(select ?x (+ ?y ?z) (+ ?y ?w))", "(+ ?y (select ?x ?z ?w))"}
rule! {selectrule67, "(select ?x (+ ?y ?z) (+ ?w ?y))", "(+ ?y (select ?x ?z ?w))"}
rule! {selectrule68, "(select ?x (+ ?z ?y) (+ ?y ?w))", "(+ ?y (select ?x ?z ?w))"}
rule! {selectrule69, "(select ?x (+ ?z ?y) (+ ?w ?y))", "(+ (select ?x ?z ?w) ?y)"}
rule! {selectrule70, "(select ?x (- ?y ?z) (- ?y ?w))", "(- ?y (select ?x ?z ?w))"}
rule! {selectrule71, "(select ?x (- ?y ?z) (+ ?y ?w))", "(+ ?y (select ?x (- ?z) ?w))"}
rule! {selectrule72, "(select ?x (+ ?y ?z) (- ?y ?w))", "(+ ?y (select ?x ?z (- ?w)))"}
rule! {selectrule73, "(select ?x (- ?y ?z) (+ ?w ?y))", "(+ ?y (select ?x (- ?z) ?w))"}
rule! {selectrule74, "(select ?x (+ ?z ?y) (- ?y ?w))", "(+ ?y (select ?x ?z (- ?w)))"}
rule! {selectrule75, "(select ?x (- ?z ?y) (- ?w ?y))", "(- (select ?x ?z ?w) ?y)"}
rule! {selectrule76, "(select ?x (* ?y ?z) (* ?y ?w))", "(* ?y (select ?x ?z ?w))"}
rule! {selectrule77, "(select ?x (* ?y ?z) (* ?w ?y))", "(* ?y (select ?x ?z ?w))"}
rule! {selectrule78, "(select ?x (* ?z ?y) (* ?y ?w))", "(* ?y (select ?x ?z ?w))"}
rule! {selectrule79, "(select ?x (* ?z ?y) (* ?w ?y))", "(* (select ?x ?z ?w) ?y)"}
rule! {selectrule80, "(select ?x (/ ?z ?y) (/ ?w ?y))", "(/ (select ?x ?z ?w) ?y)"}
rule! {selectrule81, "(select ?x (% ?z ?y) (% ?w ?y))", "(% (select ?x ?z ?w) ?y)"}
rule! {selectrule83, "(select (< ?x ?y) ?x ?y)", "(min ?x ?y)"}
rule! {selectrule84, "(select (< ?x ?y) ?y ?x)", "(max ?x ?y)"}
conditional_rule! {selectrule87, "(select ?x (* ?y ?c0) ?c1)", "(* (select ?x ?y (/ ?c1 ?c0)) ?c0)", "(= (% ?c1 ?c0) 0)", "T"}
conditional_rule! {selectrule88, "(select ?x ?c0 (* ?y ?c1))", "(* (select ?x (/ ?c0 ?c1) ?y) ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
conditional_rule! {selectrule91, "(select (< ?c0 ?x) (+ ?x ?c1) ?c2)", "(max (+ ?x ?c1) ?c2)", "(= ?c2 (= (|| (+ ?c0 ?c1) ?c2) (+ (+ ?c0 ?c1) 1)))", "T"}
conditional_rule! {selectrule92, "(select (< ?x ?c0) ?c1 (+ ?x ?c2))", "(max (+ ?x ?c2) ?c1)", "(= ?c1 (= (|| (+ ?c0 ?c2) (+ ?c1 1)) (+ ?c0 ?c2)))", "T"}
conditional_rule! {selectrule93, "(select (< ?c0 ?x) ?c1 (+ ?x ?c2))", "(min (+ ?x ?c2) ?c1)", "(= ?c1 (= (|| (+ ?c0 ?c2) ?c1) (+ (+ ?c0 ?c2) 1)))", "T"}
conditional_rule! {selectrule94, "(select (< ?x ?c0) (+ ?x ?c1) ?c2)", "(min (+ ?x ?c1) ?c2)", "(= ?c2 (= (|| (+ ?c0 ?c1) (+ ?c2 1)) (+ ?c0 ?c1)))", "T"}
conditional_rule! {selectrule96, "(select (< ?c0 ?x) ?x ?c1)", "(max ?x ?c1)", "(= ?c1 (+ ?c0 1))", "T"}
conditional_rule! {selectrule97, "(select (< ?x ?c0) ?c1 ?x)", "(max ?x ?c1)", "(= (+ ?c1 1) ?c0)", "T"}
conditional_rule! {selectrule98, "(select (< ?c0 ?x) ?c1 ?x)", "(min ?x ?c1)", "(= ?c1 (+ ?c0 1))", "T"}
rule! {selectrule104, "(select ?x ?y F)", "(&& ?x ?y)"}
rule! {selectrule105, "(select ?x ?y T)", "(|| (not ?x) ?y)"}
rule! {selectrule106, "(select ?x F ?y)", "(&& (not ?x) ?y)"}
rule! {selectrule107, "(select ?x T ?y)", "(|| ?x ?y)"}
rule! {subrule36, "(- ?x 0)", "?x"}
rule! {subrule47, "(- (select ?x ?y ?z) (select ?x ?w ?u))", "(select ?x (- ?y ?w) (- ?z ?u))"}
rule! {subrule48, "(- (select ?x ?y ?z) ?y)", "(select ?x 0 (- ?z ?y))"}
rule! {subrule49, "(- (select ?x ?y ?z) ?z)", "(select ?x (- ?y ?z) 0)"}
rule! {subrule50, "(- ?y (select ?x ?y ?z))", "(select ?x 0 (- ?y ?z))"}
rule! {subrule51, "(- ?z (select ?x ?y ?z))", "(select ?x (- ?z ?y) 0)"}
rule! {subrule52, "(- (+ ?x ?y) ?x)", "?y"}
rule! {subrule53, "(- (+ ?x ?y) ?y)", "?x"}
rule! {subrule54, "(- ?x (+ ?x ?y))", "(- ?y)"}
rule! {subrule55, "(- ?y (+ ?x ?y))", "(- ?x)"}
rule! {subrule56, "(- (- ?x ?y) ?x)", "(- ?y)"}
rule! {subrule57, "(- (+ (select ?x ?y ?z) ?w) (select ?x ?u ?v))", "(+ (select ?x (- ?y ?u) (- ?z ?v)) ?w)"}
rule! {subrule58, "(- (+ ?w (select ?x ?y ?z)) (select ?x ?u ?v))", "(+ (select ?x (- ?y ?u) (- ?z ?v)) ?w)"}
rule! {subrule59, "(- (select ?x ?y ?z) (+ (select ?x ?u ?v) ?w))", "(- (select ?x (- ?y ?u) (- ?z ?v)) ?w)"}
rule! {subrule60, "(- (select ?x ?y ?z) (+ ?w (select ?x ?u ?v)))", "(- (select ?x (- ?y ?u) (- ?z ?v)) ?w)"}
rule! {subrule61, "(- (- (select ?x ?y ?z) ?w) (select ?x ?u ?v))", "(- (select ?x (- ?y ?u) (- ?z ?v)) ?w)"}
rule! {subrule62, "(- ?c0 (select ?x ?c1 ?c2))", "(select ?x (- ?c0 ?c1) (- ?c0 ?c2))"}
rule! {subrule63, "(- (+ ?x ?c0) ?c1)", "(+ ?x (- ?c0 ?c1))"}
rule! {subrule64, "(- (+ ?x ?c0) (- ?c1 ?y))", "(+ (+ ?x ?y) (- ?c0 ?c1))"}
rule! {subrule65, "(- (+ ?x ?c0) (+ ?y ?c1))", "(+ (- ?x ?y) (- ?c0 ?c1))"}
rule! {subrule66, "(- (+ ?x ?c0) ?y)", "(+ (- ?x ?y) ?c0)"}
rule! {subrule67, "(- (- ?c0 ?x) (- ?c1 ?y))", "(+ (- ?y ?x) (- ?c0 ?c1))"}
rule! {subrule68, "(- (- ?c0 ?x) (+ ?y ?c1))", "(- (- ?c0 ?c1) (+ ?x ?y))"}
rule! {subrule69, "(- ?x (- ?y ?z))", "(+ ?x (- ?z ?y))"}
conditional_rule! {subrule70, "(- ?x (* ?y ?c0))", "(+ ?x (* ?y (- ?c0)))", "(> (< ?c0 (&& 0 (- ?c0))) 0)", "T"}
rule! {subrule71, "(- ?x (+ ?y ?c0))", "(- (- ?x ?y) ?c0)"}
rule! {subrule72, "(- (- ?c0 ?x) ?c1)", "(- (- ?c0 ?c1) ?x)"}
rule! {subrule73, "(- (* ?x ?y) (* ?z ?y))", "(* (- ?x ?z) ?y)"}
rule! {subrule74, "(- (* ?x ?y) (* ?y ?z))", "(* (- ?x ?z) ?y)"}
rule! {subrule75, "(- (* ?y ?x) (* ?z ?y))", "(* ?y (- ?x ?z))"}
rule! {subrule76, "(- (* ?y ?x) (* ?y ?z))", "(* ?y (- ?x ?z))"}
rule! {subrule77, "(- (+ ?x ?y) (+ ?x ?z))", "(- ?y ?z)"}
rule! {subrule78, "(- (+ ?x ?y) (+ ?z ?x))", "(- ?y ?z)"}
rule! {subrule79, "(- (+ ?y ?x) (+ ?x ?z))", "(- ?y ?z)"}
rule! {subrule80, "(- (+ ?y ?x) (+ ?z ?x))", "(- ?y ?z)"}
rule! {subrule81, "(- (+ (+ ?x ?y) ?z) ?x)", "(+ ?y ?z)"}
rule! {subrule82, "(- (+ (+ ?y ?x) ?z) ?x)", "(+ ?y ?z)"}
rule! {subrule83, "(- (+ ?z (+ ?x ?y)) ?x)", "(+ ?z ?y)"}
rule! {subrule84, "(- (+ ?z (+ ?y ?x)) ?x)", "(+ ?z ?y)"}
rule! {subrule86, "(- (max ?x ?y) ?x)", "(max 0 (- ?y ?x))"}
rule! {subrule87, "(- (min ?x ?y) ?x)", "(min 0 (- ?y ?x))"}
rule! {subrule88, "(- (max ?x ?y) ?y)", "(max (- ?x ?y) 0)"}
rule! {subrule89, "(- (min ?x ?y) ?y)", "(min (- ?x ?y) 0)"}
rule! {subrule94, "(- (* ?x ?y) ?x)", "(* ?x (- ?y 1))"}
rule! {subrule95, "(- (* ?x ?y) ?y)", "(* (- ?x 1) ?y)"}
rule! {subrule96, "(- ?x (* ?x ?y))", "(* ?x (- 1 ?y))"}
rule! {subrule97, "(- ?x (* ?y ?x))", "(* (- 1 ?y) ?x)"}
rule! {subrule98, "(- ?x (min (+ ?x ?y) ?z))", "(max (- ?y) (- ?x ?z))"}
rule! {subrule99, "(- ?x (min (+ ?y ?x) ?z))", "(max (- ?y) (- ?x ?z))"}
rule! {subrule100, "(- ?x (min ?z (+ ?x ?y)))", "(max (- ?x ?z) (- ?y))"}
rule! {subrule101, "(- ?x (min ?z (+ ?y ?x)))", "(max (- ?x ?z) (- ?y))"}
rule! {subrule102, "(- (min (+ ?x ?y) ?z) ?x)", "(min ?y (- ?z ?x))"}
rule! {subrule103, "(- (min (+ ?y ?x) ?z) ?x)", "(min ?y (- ?z ?x))"}
rule! {subrule104, "(- (min ?z (+ ?x ?y)) ?x)", "(min (- ?z ?x) ?y)"}
rule! {subrule105, "(- (min ?z (+ ?y ?x)) ?x)", "(min (- ?z ?x) ?y)"}
rule! {subrule106, "(- (min ?x ?y) (min ?y ?x))", "0"}
rule! {subrule110, "(- ?x (max (+ ?x ?y) ?z))", "(min (- ?y) (- ?x ?z))"}
rule! {subrule111, "(- ?x (max (+ ?y ?x) ?z))", "(min (- ?y) (- ?x ?z))"}
rule! {subrule112, "(- ?x (max ?z (+ ?x ?y)))", "(min (- ?x ?z) (- ?y))"}
rule! {subrule113, "(- ?x (max ?z (+ ?y ?x)))", "(min (- ?x ?z) (- ?y))"}
rule! {subrule114, "(- (max (+ ?x ?y) ?z) ?x)", "(max ?y (- ?z ?x))"}
rule! {subrule115, "(- (max (+ ?y ?x) ?z) ?x)", "(max ?y (- ?z ?x))"}
rule! {subrule116, "(- (max ?z (+ ?x ?y)) ?x)", "(max (- ?z ?x) ?y)"}
rule! {subrule117, "(- (max ?z (+ ?y ?x)) ?x)", "(max (- ?z ?x) ?y)"}
rule! {subrule118, "(- (max ?x ?y) (max ?y ?x))", "0"}
conditional_rule! {subrule225, "(- ?c0 (/ (- ?c1 ?x) ?c2))", "(/ (+ (- (+ (- (* ?c0 ?c2) ?c1) ?c2) 1) ?x) ?c2)", "(> ?c2 0)", "T"}
conditional_rule! {subrule226, "(- ?c0 (/ (+ ?x ?c1) ?c2))", "(/ (- (- (+ (- (* ?c0 ?c2) ?c1) ?c2) 1) ?x) ?c2)", "(> ?c2 0)", "T"}
conditional_rule! {subrule227, "(- ?x (/ (+ ?x ?y) ?c0))", "(/ (+ (- (* ?x (- ?c0 1)) ?y) (- ?c0 1)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule228, "(- ?x (/ (- ?x ?y) ?c0))", "(/ (+ (+ (* ?x (- ?c0 1)) ?y) (- ?c0 1)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule229, "(- ?x (/ (+ ?y ?x) ?c0))", "(/ (+ (- (* ?x (- ?c0 1)) ?y) (- ?c0 1)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule230, "(- ?x (/ (- ?y ?x) ?c0))", "(/ (+ (- (* ?x (+ ?c0 1)) ?y) (- ?c0 1)) ?c0)", "(> ?c0 0)", "T"}
rule! {subrule231, "(- (/ (+ ?x ?y) ?c0) ?x)", "(/ (+ (* ?x (- 1 ?c0)) ?y) ?c0)"}
rule! {subrule232, "(- (/ (+ ?y ?x) ?c0) ?x)", "(/ (+ ?y (* ?x (- 1 ?c0))) ?c0)"}
rule! {subrule233, "(- (/ (- ?x ?y) ?c0) ?x)", "(/ (- (* ?x (- 1 ?c0)) ?y) ?c0)"}
rule! {subrule234, "(- (/ (- ?y ?x) ?c0) ?x)", "(/ (- ?y (* ?x (+ 1 ?c0))) ?c0)"}
conditional_rule! {subrule236, "(- (* (/ ?x ?c0) ?c0) ?x)", "(- (% ?x ?c0))", "(> ?c0 0)", "T"}
conditional_rule! {subrule237, "(- ?x (* (/ ?x ?c0) ?c0))", "(% ?x ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule238, "(- (* (/ (+ ?x ?c0) ?c1) ?c1) ?x)", "(% (- ?x) ?c1)", "(= (> ?c1 (&& 0 (+ ?c0 1))) ?c1)", "T"}
conditional_rule! {subrule239, "(- ?x (* (/ (+ ?x ?c0) ?c1) ?c1))", "(+ (% (+ ?x ?c0) ?c1) (- ?c0))", "(= (> ?c1 (&& 0 (+ ?c0 1))) ?c1)", "T"}
conditional_rule! {subrule240, "(- (* ?x ?c0) (* ?y ?c1))", "(* (- (* ?x (/ ?c0 ?c1)) ?y) ?c1)", "(= (% ?c0 ?c1) 0)", "T"}
conditional_rule! {subrule241, "(- (* ?x ?c0) (* ?y ?c1))", "(* (- ?x (* ?y (/ ?c1 ?c0))) ?c0)", "(= (% ?c1 ?c0) 0)", "T"}
conditional_rule! {subrule246, "(- (/ (+ (+ ?x ?y) ?z) ?c0) (/ (+ (+ ?y ?x) ?w) ?c0))", "(- (/ (+ (+ ?x ?y) ?z) ?c0) (/ (+ (+ ?x ?y) ?w) ?c0))", "(> ?c0 0)", "T"}
conditional_rule! {subrule247, "(- (/ (+ ?x ?y) ?c0) (/ (+ ?y ?x) ?c0))", "0", "(!= ?c0 0)", "T"}
conditional_rule! {subrule248, "(- (/ (+ ?x ?y) ?c0) (/ (+ ?x ?c1) ?c0))", "(/ (+ (% (+ ?x (% ?c1 ?c0)) ?c0) (- ?y ?c1)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule249, "(- (/ (+ ?x ?c1) ?c0) (/ (+ ?x ?y) ?c0))", "(/ (- (- (- (+ ?c0 ?c1) 1) ?y) (% (+ ?x (% ?c1 ?c0)) ?c0)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule250, "(- (/ (- ?x ?y) ?c0) (/ (+ ?x ?c1) ?c0))", "(/ (- (- (% (+ ?x (% ?c1 ?c0)) ?c0) ?y) ?c1) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule251, "(- (/ (+ ?x ?c1) ?c0) (/ (- ?x ?y) ?c0))", "(/ (- (+ ?y (- (+ ?c0 ?c1) 1)) (% (+ ?x (% ?c1 ?c0)) ?c0)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule252, "(- (/ ?x ?c0) (/ (+ ?x ?y) ?c0))", "(/ (- (- (- ?c0 1) ?y) (% ?x ?c0)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule253, "(- (/ (+ ?x ?y) ?c0) (/ ?x ?c0))", "(/ (+ (% ?x ?c0) ?y) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule254, "(- (/ ?x ?c0) (/ (- ?x ?y) ?c0))", "(/ (- (+ ?y (- ?c0 1)) (% ?x ?c0)) ?c0)", "(> ?c0 0)", "T"}
conditional_rule! {subrule255, "(- (/ (- ?x ?y) ?c0) (/ ?x ?c0))", "(/ (- (% ?x ?c0) ?y) ?c0)", "(> ?c0 0)", "T"}


#[test]
fn htrs() {

let rules = vec![
  subrule36(),
  subrule47(),
  subrule48(),
  subrule49(),
  subrule50(),
  subrule51(),
  subrule52(),
  subrule53(),
  subrule54(),
  subrule55(),
  subrule56(),
  subrule57(),
  subrule58(),
  subrule59(),
  subrule60(),
  subrule61(),
  subrule62(),
  subrule63(),
  subrule64(),
  subrule65(),
  subrule66(),
  subrule67(),
  subrule68(),
  subrule69(),
  subrule71(),
  subrule72(),
  subrule73(),
  subrule74(),
  subrule75(),
  subrule76(),
  subrule77(),
  subrule78(),
  subrule79(),
  subrule80(),
  subrule81(),
  subrule82(),
  subrule83(),
  subrule84(),
  subrule86(),
  subrule87(),
  subrule88(),
  subrule89(),
  subrule94(),
  subrule95(),
  subrule96(),
  subrule97(),
  subrule98(),
  subrule99(),
  subrule100(),
  subrule101(),
  subrule102(),
  subrule103(),
  subrule104(),
  subrule105(),
  subrule106(),
  subrule110(),
  subrule111(),
  subrule112(),
  subrule113(),
  subrule114(),
  subrule115(),
  subrule116(),
  subrule117(),
  subrule118(),
  subrule231(),
  subrule232(),
  subrule233(),
  subrule234(),
  selectrule29(),
  selectrule30(),
  selectrule31(),
  selectrule58(),
  selectrule59(),
  selectrule60(),
  selectrule61(),
  selectrule62(),
  selectrule63(),
  selectrule64(),
  selectrule65(),
  selectrule66(),
  selectrule67(),
  selectrule68(),
  selectrule69(),
  selectrule70(),
  selectrule71(),
  selectrule72(),
  selectrule73(),
  selectrule74(),
  selectrule75(),
  selectrule76(),
  selectrule77(),
  selectrule78(),
  selectrule79(),
  selectrule80(),
  selectrule81(),
  selectrule83(),
  selectrule84(),
  selectrule104(),
  selectrule105(),
  selectrule106(),
  selectrule107(),
  orrule21(),
  orrule22(),
  orrule23(),
  orrule25(),
  orrule26(),
  orrule27(),
  orrule28(),
  orrule30(),
  orrule31(),
  orrule32(),
  orrule33(),
  orrule34(),
  orrule35(),
  orrule36(),
  orrule37(),
  orrule39(),
  orrule40(),
  orrule41(),
  orrule42(),
  orrule44(),
  orrule45(),
  orrule46(),
  orrule47(),
  orrule48(),
  orrule49(),
  orrule50(),
  orrule51(),
  orrule53(),
  orrule54(),
  orrule55(),
  orrule56(),
  orrule57(),
  orrule58(),
  orrule59(),
  orrule60(),
  orrule61(),
  orrule62(),
  orrule63(),
  orrule64(),
  orrule65(),
  orrule73(),
  orrule74(),
  orrule75(),
  orrule76(),
  orrule82(),
  orrule83(),
  orrule84(),
  orrule85(),
  orrule87(),
  orrule88(),
  orrule89(),
  orrule90(),
  orrule92(),
  orrule93(),
  orrule94(),
  orrule95(),
  orrule97(),
  orrule98(),
  orrule99(),
  orrule100(),
  orrule102(),
  orrule103(),
  orrule104(),
  orrule105(),
  orrule107(),
  orrule108(),
  orrule109(),
  orrule110(),
  notrule11(),
  notrule12(),
  notrule13(),
  notrule14(),
  notrule15(),
  notrule16(),
  notrule17(),
  mulrule56(),
  mulrule57(),
  mulrule58(),
  mulrule67(),
  mulrule68(),
  mulrule69(),
  modrule53(),
  modrule54(),
  modrule58(),
  modrule72(),
  minrule45(),
  minrule46(),
  minrule64(),
  minrule66(),
  minrule68(),
  minrule70(),
  minrule72(),
  minrule74(),
  minrule76(),
  minrule78(),
  minrule80(),
  minrule82(),
  minrule84(),
  minrule86(),
  minrule88(),
  minrule90(),
  minrule138(),
  minrule139(),
  minrule140(),
  minrule141(),
  minrule142(),
  minrule143(),
  minrule144(),
  minrule149(),
  minrule150(),
  minrule151(),
  minrule152(),
  minrule153(),
  minrule154(),
  minrule155(),
  minrule171(),
  minrule176(),
  minrule177(),
  minrule178(),
  minrule179(),
  minrule180(),
  minrule181(),
  minrule182(),
  minrule183(),
  minrule190(),
  minrule191(),
  minrule192(),
  minrule193(),
  minrule195(),
  minrule196(),
  minrule197(),
  minrule198(),
  minrule200(),
  minrule201(),
  minrule202(),
  minrule203(),
  minrule204(),
  minrule205(),
  minrule206(),
  minrule207(),
  minrule208(),
  minrule209(),
  minrule210(),
  minrule211(),
  minrule213(),
  minrule214(),
  minrule216(),
  minrule217(),
  minrule218(),
  minrule219(),
  minrule220(),
  minrule221(),
  minrule222(),
  minrule223(),
  minrule246(),
  maxrule45(),
  maxrule46(),
  maxrule64(),
  maxrule66(),
  maxrule68(),
  maxrule70(),
  maxrule72(),
  maxrule74(),
  maxrule76(),
  maxrule78(),
  maxrule80(),
  maxrule82(),
  maxrule84(),
  maxrule86(),
  maxrule88(),
  maxrule90(),
  maxrule138(),
  maxrule139(),
  maxrule140(),
  maxrule141(),
  maxrule142(),
  maxrule143(),
  maxrule144(),
  maxrule149(),
  maxrule150(),
  maxrule151(),
  maxrule152(),
  maxrule153(),
  maxrule154(),
  maxrule155(),
  maxrule168(),
  maxrule173(),
  maxrule174(),
  maxrule175(),
  maxrule176(),
  maxrule177(),
  maxrule178(),
  maxrule179(),
  maxrule180(),
  maxrule187(),
  maxrule188(),
  maxrule189(),
  maxrule190(),
  maxrule192(),
  maxrule193(),
  maxrule194(),
  maxrule195(),
  maxrule197(),
  maxrule198(),
  maxrule199(),
  maxrule200(),
  maxrule201(),
  maxrule202(),
  maxrule203(),
  maxrule204(),
  maxrule205(),
  maxrule206(),
  maxrule207(),
  maxrule208(),
  maxrule210(),
  maxrule211(),
  maxrule213(),
  maxrule214(),
  maxrule215(),
  maxrule216(),
  maxrule217(),
  maxrule218(),
  maxrule219(),
  maxrule220(),
  maxrule243(),
  ltrule33(),
  ltrule34(),
  ltrule38(),
  ltrule39(),
  ltrule40(),
  ltrule41(),
  ltrule58(),
  ltrule61(),
  ltrule64(),
  ltrule65(),
  ltrule67(),
  ltrule68(),
  ltrule69(),
  ltrule70(),
  ltrule72(),
  ltrule73(),
  ltrule74(),
  ltrule75(),
  ltrule77(),
  ltrule78(),
  ltrule79(),
  ltrule80(),
  ltrule84(),
  ltrule85(),
  ltrule88(),
  ltrule89(),
  ltrule92(),
  ltrule93(),
  ltrule94(),
  ltrule95(),
  ltrule98(),
  ltrule99(),
  ltrule100(),
  ltrule101(),
  ltrule102(),
  ltrule103(),
  ltrule104(),
  ltrule105(),
  ltrule108(),
  ltrule109(),
  ltrule110(),
  ltrule111(),
  ltrule112(),
  ltrule113(),
  ltrule114(),
  ltrule115(),
  ltrule118(),
  ltrule119(),
  ltrule120(),
  ltrule121(),
  ltrule122(),
  ltrule123(),
  ltrule124(),
  ltrule125(),
  ltrule126(),
  ltrule127(),
  ltrule128(),
  ltrule129(),
  ltrule130(),
  ltrule131(),
  ltrule132(),
  ltrule133(),
  ltrule151(),
  ltrule152(),
  ltrule153(),
  ltrule154(),
  ltrule156(),
  ltrule157(),
  ltrule158(),
  ltrule159(),
  ltrule162(),
  ltrule163(),
  ltrule164(),
  ltrule165(),
  ltrule167(),
  ltrule168(),
  ltrule169(),
  ltrule170(),
  ltrule173(),
  ltrule174(),
  ltrule175(),
  ltrule176(),
  ltrule178(),
  ltrule179(),
  ltrule180(),
  ltrule181(),
  ltrule184(),
  ltrule185(),
  ltrule186(),
  ltrule187(),
  ltrule190(),
  ltrule191(),
  ltrule192(),
  ltrule193(),
  eqrule67(),
  eqrule69(),
  eqrule71(),
  eqrule72(),
  eqrule73(),
  eqrule74(),
  eqrule75(),
  eqrule76(),
  eqrule77(),
  eqrule78(),
  eqrule83(),
  eqrule84(),
  eqrule90(),
  eqrule91(),
  divrule96(),
  divrule100(),
  divrule107(),
  divrule152(),
  divrule153(),
  divrule154(),
  divrule155(),
  divrule156(),
  divrule157(),
  divrule158(),
  divrule159(),
  divrule162(),
  divrule163(),
  divrule164(),
  divrule165(),
  divrule166(),
  divrule167(),
  divrule168(),
  divrule169(),
  divrule177(),
  addrule39(),
  addrule45(),
  addrule49(),
  addrule50(),
  addrule51(),
  addrule52(),
  addrule54(),
  addrule55(),
  addrule56(),
  addrule57(),
  addrule58(),
  addrule59(),
  addrule60(),
  addrule61(),
  addrule63(),
  addrule64(),
  addrule65(),
  addrule66(),
  addrule67(),
  addrule68(),
  addrule69(),
  addrule70(),
  addrule71(),
  addrule72(),
  addrule75(),
  addrule76(),
  addrule77(),
  addrule78(),
  addrule82(),
  addrule83(),
  addrule84(),
  addrule86(),
  addrule87(),
  addrule88(),
  addrule90(),
  addrule91(),
  addrule92(),
  addrule93(),
  addrule94(),
  addrule95(),
  addrule96(),
  addrule97(),
  addrule98(),
  addrule99(),
  addrule102(),
  addrule103(),
  addrule104(),
  addrule105(),
  addrule106(),
  addrule107(),
  addrule110(),
  addrule122(),
  andrule22(),
  andrule23(),
  andrule24(),
  andrule26(),
  andrule27(),
  andrule28(),
  andrule29(),
  andrule31(),
  andrule32(),
  andrule33(),
  andrule34(),
  andrule35(),
  andrule36(),
  andrule37(),
  andrule38(),
  andrule40(),
  andrule41(),
  andrule42(),
  andrule43(),
  andrule45(),
  andrule46(),
  andrule47(),
  andrule48(),
  andrule49(),
  andrule50(),
  andrule51(),
  andrule52(),
  andrule53(),
  andrule54(),
  andrule55(),
  andrule56(),
  andrule57(),
  andrule70(),
  andrule71(),
  andrule72(),
  andrule73(),
  andrule79(),
  andrule80(),
  andrule81(),
  andrule82(),
  andrule84(),
  andrule85(),
  andrule86(),
  andrule87(),
  andrule89(),
  andrule90(),
  andrule91(),
  andrule92(),
  andrule94(),
  andrule95(),
  andrule96(),
  andrule97(),
  andrule99(),
  andrule100(),
  andrule101(),
  andrule102(),
  andrule104(),
  andrule105(),
  andrule106(),
  andrule107(),
  ];

let condrules = vec![
  addrule73(),
  addrule79(),
  addrule80(),
  addrule100(),
  addrule101(),
  addrule108(),
  addrule109(),
  addrule113(),
  addrule114(),
  addrule115(),
  addrule116(),
  addrule117(),
  addrule118(),
  addrule119(),
  addrule120(),
  addrule121(),
  addrule124(),
  addrule125(),
  addrule126(),
  andrule66(),
  andrule67(),
  andrule68(),
  andrule69(),
  divrule112(),
  divrule114(),
  divrule116(),
  divrule117(),
  divrule118(),
  divrule119(),
  divrule121(),
  divrule122(),
  divrule123(),
  divrule124(),
  divrule126(),
  divrule127(),
  divrule128(),
  divrule129(),
  divrule131(),
  divrule132(),
  divrule133(),
  divrule134(),
  divrule136(),
  divrule137(),
  divrule138(),
  divrule139(),
  divrule142(),
  divrule143(),
  divrule144(),
  divrule145(),
  divrule146(),
  divrule147(),
  divrule148(),
  divrule149(),
  divrule151(),
  divrule178(),
  divrule179(),
  divrule180(),
  divrule182(),
  eqrule68(),
  eqrule70(),
  eqrule79(),
  eqrule80(),
  eqrule81(),
  eqrule82(),
  ltrule136(),
  ltrule137(),
  ltrule141(),
  ltrule144(),
  ltrule198(),
  ltrule199(),
  ltrule200(),
  ltrule201(),
  ltrule203(),
  ltrule204(),
  ltrule205(),
  ltrule206(),
  ltrule208(),
  ltrule209(),
  ltrule210(),
  ltrule211(),
  ltrule213(),
  ltrule214(),
  ltrule215(),
  ltrule216(),
  ltrule222(),
  ltrule223(),
  ltrule225(),
  ltrule226(),
  ltrule236(),
  ltrule237(),
  ltrule238(),
  ltrule239(),
  ltrule240(),
  ltrule241(),
  ltrule242(),
  ltrule243(),
  ltrule246(),
  ltrule247(),
  ltrule248(),
  ltrule249(),
  ltrule252(),
  ltrule253(),
  ltrule254(),
  ltrule255(),
  ltrule258(),
  ltrule259(),
  ltrule260(),
  ltrule261(),
  ltrule262(),
  ltrule263(),
  ltrule264(),
  ltrule265(),
  ltrule268(),
  ltrule269(),
  ltrule272(),
  ltrule273(),
  ltrule274(),
  ltrule275(),
  ltrule278(),
  ltrule279(),
  ltrule280(),
  ltrule281(),
  ltrule284(),
  ltrule285(),
  ltrule288(),
  ltrule289(),
  ltrule291(),
  ltrule292(),
  ltrule294(),
  ltrule295(),
  ltrule298(),
  ltrule299(),
  ltrule302(),
  ltrule303(),
  ltrule304(),
  ltrule305(),
  ltrule306(),
  ltrule307(),
  ltrule309(),
  ltrule310(),
  ltrule311(),
  ltrule312(),
  ltrule313(),
  ltrule314(),
  ltrule316(),
  ltrule317(),
  ltrule318(),
  ltrule319(),
  ltrule320(),
  ltrule321(),
  ltrule322(),
  ltrule323(),
  ltrule326(),
  ltrule327(),
  ltrule328(),
  ltrule329(),
  ltrule332(),
  ltrule333(),
  ltrule334(),
  ltrule335(),
  ltrule336(),
  ltrule337(),
  ltrule338(),
  ltrule339(),
  ltrule342(),
  ltrule343(),
  ltrule344(),
  ltrule345(),
  maxrule60(),
  maxrule62(),
  maxrule92(),
  maxrule110(),
  maxrule112(),
  maxrule114(),
  maxrule116(),
  maxrule119(),
  maxrule121(),
  maxrule123(),
  maxrule125(),
  maxrule127(),
  maxrule129(),
  maxrule131(),
  maxrule158(),
  maxrule159(),
  maxrule160(),
  maxrule161(),
  maxrule163(),
  maxrule164(),
  maxrule165(),
  maxrule166(),
  maxrule170(),
  maxrule171(),
  maxrule182(),
  maxrule183(),
  maxrule184(),
  maxrule185(),
  maxrule222(),
  maxrule223(),
  maxrule225(),
  maxrule226(),
  maxrule227(),
  maxrule228(),
  maxrule229(),
  maxrule230(),
  maxrule232(),
  maxrule233(),
  minrule60(),
  minrule62(),
  minrule92(),
  minrule110(),
  minrule112(),
  minrule114(),
  minrule116(),
  minrule119(),
  minrule121(),
  minrule123(),
  minrule125(),
  minrule127(),
  minrule129(),
  minrule131(),
  minrule158(),
  minrule161(),
  minrule162(),
  minrule163(),
  minrule164(),
  minrule166(),
  minrule167(),
  minrule168(),
  minrule169(),
  minrule173(),
  minrule174(),
  minrule185(),
  minrule186(),
  minrule187(),
  minrule188(),
  minrule225(),
  minrule226(),
  minrule228(),
  minrule229(),
  minrule230(),
  minrule231(),
  minrule232(),
  minrule233(),
  minrule235(),
  minrule236(),
  modrule65(),
  modrule66(),
  modrule67(),
  modrule68(),
  modrule69(),
  modrule70(),
  modrule71(),
  mulrule64(),
  orrule69(),
  orrule70(),
  orrule71(),
  orrule72(),
  selectrule87(),
  selectrule88(),
  selectrule91(),
  selectrule92(),
  selectrule93(),
  selectrule94(),
  selectrule96(),
  selectrule97(),
  selectrule98(),
  subrule70(),
  subrule225(),
  subrule226(),
  subrule227(),
  subrule228(),
  subrule229(),
  subrule230(),
  subrule236(),
  subrule237(),
  subrule238(),
  subrule239(),
  subrule240(),
  subrule241(),
  subrule246(),
  subrule247(),
  subrule248(),
  subrule249(),
  subrule250(),
  subrule251(),
  subrule252(),
  subrule253(),
  subrule254(),
  subrule255(),
];

//(<= (+ (min (* (- v4 v3) c0) c1) (* v3 c0)) (min (* v4 c0) (+ (* v3 c0) c1)))

  let start = "(<= (+ (min (* (- v4 v3) c0) c1) (* v3 c0)) (min (* v4 c0) (+ (* v3 c0) c1)))";
  let goal = "T";
  let start_expr = HalideExpr.parse_expr(start).unwrap();
  let goal_expr = HalideExpr.parse_expr(goal).unwrap();
  let (mut egraph, _old_root) = EGraph::<HalideExpr, ()>::from_expr(&start_expr);

  let mut egraph_size = egraph.total_size();

  println!("Original egraph size: {}", egraph_size);

  for r in rules {
    r.run(&mut egraph);
    if egraph.total_size() > egraph_size {
      println!("Rule: LHS: {}, RHS: {}, new egraph size: {}", r.lhs.to_sexp(), r.rhs.to_sexp(), egraph.total_size());
      egraph_size = egraph.total_size();
    }
  }
  for cr in condrules {
    cr.run(&mut egraph);
    if egraph.total_size() > egraph_size {
      let condition = &cr.conditions[0];
      println!("Cond Rule: LHS: {}, RHS: {}, Condition: {}, new egraph size: {}", cr.lhs.to_sexp(), cr.rhs.to_sexp(), condition.lhs.to_sexp(), egraph.total_size());
      egraph_size = egraph.total_size();   
    }
  }
  egraph.rebuild();

  let equivs = egraph.equivs(&start_expr, &goal_expr);
  if equivs.is_empty() {
    panic!("Couldn't prove equivalence {}: {}", start, goal);
  }
}

