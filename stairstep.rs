// goal is to prove a small inequality with min, add, multiply, subtract
// min((v7 - v6)*c0, -1) + (v6*c0) + 1 <= min(v6*c0, v7*c0 + 1)

// cargo test whatever --release

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
  #[strum(serialize = "min")]
  Min,
  #[strum(serialize = "-")]
  Sub,
  #[strum(serialize = "+")]
  Add,
  #[strum(serialize = "*")]
  Mul,
  #[strum(serialize = "<=")]
  Le,
  #[strum(serialize = "div")]
  Div,
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

rule! {sub_mul_distributve,           "(* (- ?a ?b) ?c)",     "(- (* ?a ?c) (* ?b ?c))"}
rule! {add_min_distributive,          "(+ (min ?a ?b) ?c)",   "(min (+ ?a ?c) (+ ?b ?c))"}
rule! {add_comm,                      "(+ ?a ?b)",            "(+ ?b ?a)"}
rule! {mul_comm,                      "(* ?a ?b)",            "(* ?b ?a)"}
rule! {min_comm,                      "(min ?a ?b)",          "(min ?b ?a)"}
rule! {le_refl,                       "(<= ?a ?a)",           "T"}
rule! {add_negative_cancellation,     "(+ (+ ?a (- ?b)) ?b)", "?a"}
rule! {sub_add_cancellation,          "(+ (- ?a ?b) ?b)",     "?a"}
rule! {add_assoc,                     "(+ ?a (+ ?b ?c))",     "(+ (+ ?a ?b) ?c)"}

fn prove_equivalence(start: &str, rewrites: &[Rewrite<HalideExpr>], goal: &str) {
  // parse start and goal expressions
  let start_expr = HalideExpr.parse_expr(start).unwrap();
  let goal_expr = HalideExpr.parse_expr(goal).unwrap();
  // initialize the egraph with the start expression
  let (mut egraph, _old_root) = EGraph::<HalideExpr, ()>::from_expr(&start_expr);
  // run the rules over the egraph for some bounded loop
  for _ in 0..6 {
    for rw in rewrites {
      rw.run(&mut egraph);
    }
    egraph.rebuild();
  }
  // check if egraph.equivs is empty
  let equivs = egraph.equivs(&start_expr, &goal_expr);
  if equivs.is_empty() {
    panic!("Couldn't prove goal {}: {}", start, goal);
  }
}

//((min(((((v0 + 13)/4)*4) + v1), (v2 + 10)) + 4)
//  min(((((min((((v0 + 17)/4)*4), (min((v2 - v1), v0) + 14)) + 3)/4)*4) + v1), (v2 + 14)))

// we cheat so we don't need constant integer values
// manipulate to eliminate 10
// then express all constants in terms of 4, 13 and the constant symbol 1
// a := 13, b := 4

  rule! { add_3term_comm, "(+ (+ ?a ?b) ?c)", "(+ (+ ?a ?c) ?b)"}
  rule! { min_sub_outside, "(min (+ ?a ?b) ?c)", "(+ (min ?a (- ?c ?b)) ?b)"}
  rule! { le_add_cancel, "(<= (+ ?a ?b) (+ ?c ?b))", "(<= ?a ?c)"}
  rule! { add_inside_mul_div, "(+ (* (div ?a ?b) ?b) ?b)", "(* (div (+ ?a ?b) ?b) ?b)" }
  rule! { move_one_add, "(+ ?a ?b)", "(+ (+ ?a 1) (- ?b 1))" }
  rule! { div_min_distributive, "(* (div (min ?a ?b) ?c) ?c)", "(min (* (div ?a ?c) ?c) (* (div ?b ?c) ?c))" }
  rule! { add_sub_shuffle, "(- (+ ?x ?z) ?y)", "(+ (- ?x ?y) ?z)" }
  rule! { min_div_bound, "(min (* (div (+ ?a (- ?c 1)) ?c) ?c) (min ?b ?a))", "(min ?b ?a)" }
  rule! { another_min_div_bound, "(min (min (* (div (+ ?a ?b) ?b) ?b) ?c) (+ ?a 1))", "(min ?c (+ ?a 1))" }

#[test]
fn stairstepbyhand() {
  println!("hello!");
  let start = "(<= (min (+ (+ (* (div (+ x a) b) b) y) b)
                        (+ z (+ a 1)))
                   (min (+ (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b) y)
                        (+ z (+ a 1))))";
  let start_expr = HalideExpr.parse_expr(start).unwrap();
  let goal = "T";
  let goal_expr = HalideExpr.parse_expr(goal).unwrap();
  let (mut egraph, _old_root) = EGraph::<HalideExpr, ()>::from_expr(&start_expr);

  let add_3term_comm_rule = add_3term_comm();
  add_3term_comm_rule.run(&mut egraph);
  egraph.rebuild();
  let step0 = "(<= (min (+ (+ (* (div (+ x a) b) b) b) y)
                        (+ z (+ a 1)))
                   (min (+ (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b) y)
                        (+ z (+ a 1))))";
  let _step0_expr = HalideExpr.parse_expr(step0).unwrap();

  let min_sub_outside_rule = min_sub_outside();
  min_sub_outside_rule.run(&mut egraph);
  egraph.rebuild();
  let step1 = "(<= (+ (min (+ (* (div (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y)) y)
                   (min (+ (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b) y)
                        (+ z (+ a 1))))";
  let _step1_expr = HalideExpr.parse_expr(step1).unwrap();

  min_sub_outside_rule.run(&mut egraph);
  egraph.rebuild();
  let step2 = "(<= (+ (min (+ (* (div (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y)) y)
                   (+ (min (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)) y))";
  let _step2_expr = HalideExpr.parse_expr(step2).unwrap();

  let le_add_cancel_rule = le_add_cancel();
  le_add_cancel_rule.run(&mut egraph);
  egraph.rebuild();
  let step3 = "(<= (min (+ (* (div (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step3_expr = HalideExpr.parse_expr(step3).unwrap();

  let add_inside_mul_div_rule = add_inside_mul_div();
  add_inside_mul_div_rule.run(&mut egraph);
  egraph.rebuild();
  let step4 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step4_expr = HalideExpr.parse_expr(step4).unwrap();

  let add_min_distributive_rule = add_min_distributive();
  add_min_distributive_rule.run(&mut egraph);
  egraph.rebuild();
  let step5 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (min (+ (- z y) (+ a 1)) (+ x (+ a 1)))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step5_expr = HalideExpr.parse_expr(step5).unwrap();

  let move_one_add_rule = move_one_add();
  move_one_add_rule.run(&mut egraph);
  egraph.rebuild();
  let step6_pre = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ (min (* (div (+ x (+ (+ a 1) (- b 1))) b) b)
                                           (min (+ (- z y) (+ a 1)) (+ x (+ a 1)))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step6_pre_expr = HalideExpr.parse_expr(step6_pre).unwrap();

  let add_assoc_rule = add_assoc();
  add_assoc_rule.run(&mut egraph);
  egraph.rebuild();
  let step6_pre2 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ (min (* (div (+ (+ x (+ a 1)) (- b 1)) b) b)
                                           (min (+ (- z y) (+ a 1)) (+ x (+ a 1)))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step6_pre2_expr = HalideExpr.parse_expr(step6_pre2).unwrap();

  let min_div_bound_rule = min_div_bound();
  min_div_bound_rule.run(&mut egraph);
  egraph.rebuild();
  let step6 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ (min (+ (- z y) (+ a 1)) (+ x (+ a 1))) (- b 1)) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step6_expr = HalideExpr.parse_expr(step6).unwrap();

  add_min_distributive_rule.run(&mut egraph);
  egraph.rebuild();
  let step7 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (min (+ (+ (- z y) (+ a 1)) (- b 1)) (+ (+ x (+ a 1)) (- b 1))) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step7_expr = HalideExpr.parse_expr(step7).unwrap();

  add_assoc_rule.flip().run(&mut egraph);
  egraph.rebuild();

  let step8 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (min (+ (+ (- z y) (+ a 1)) (- b 1)) (+ x (+ (+ a 1) (- b 1)))) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step8_expr = HalideExpr.parse_expr(step8).unwrap();

  move_one_add_rule.flip().run(&mut egraph);
  egraph.rebuild();
  let step9 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (min (+ (+ (- z y) (+ a 1)) (- b 1)) (+ x (+ a b))) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step9_expr = HalideExpr.parse_expr(step9).unwrap();

  add_assoc_rule.flip().run(&mut egraph);
  egraph.rebuild();
  let step10 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (min (+ (- z y) (+ (+ a 1) (- b 1))) (+ x (+ a b))) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step10_expr = HalideExpr.parse_expr(step10).unwrap();

  move_one_add_rule.flip().run(&mut egraph);
  egraph.rebuild();
  let step11 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (min (+ (- z y) (+ a b)) (+ x (+ a b))) b) b)
                        (- (+ z (+ a 1)) y)))";
  let _step11_expr = HalideExpr.parse_expr(step11).unwrap();

  let div_min_distributive_rule = div_min_distributive();
  div_min_distributive_rule.run(&mut egraph);
  egraph.rebuild();
  let step12 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (min (* (div (+ (- z y) (+ a b)) b) b) (* (div (+ x (+ a b)) b) b))
                        (- (+ z (+ a 1)) y)))";
  let _step12_expr = HalideExpr.parse_expr(step12).unwrap();

  let add_sub_shuffle_rule = add_sub_shuffle();
  add_sub_shuffle_rule.run(&mut egraph);
  egraph.rebuild();
  let step13 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (min (* (div (+ (- z y) (+ a b)) b) b) (* (div (+ x (+ a b)) b) b))
                        (+ (- z y) (+ a 1))))";
  let _step13_expr = HalideExpr.parse_expr(step13).unwrap();

  let another_min_div_bound_rule = another_min_div_bound();
  another_min_div_bound_rule.run(&mut egraph);
  egraph.rebuild();
  let step14 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (- (+ z (+ a 1)) y))
                   (min (* (div (+ x (+ a b)) b) b)
                        (+ (- z y) (+ a 1))))";
  let _step14_expr = HalideExpr.parse_expr(step14).unwrap();

  add_sub_shuffle_rule.run(&mut egraph);
  egraph.rebuild();
  let step15 = "(<= (min (* (div (+ (+ x a) b) b) b)
                           (+ (- z y) (+ a 1)))
                   (min (* (div (+ x (+ a b)) b) b)
                        (+ (- z y) (+ a 1))))";
  let _step15_expr = HalideExpr.parse_expr(step15).unwrap();

  le_refl().run(&mut egraph);
  egraph.rebuild();

  let equivs = egraph.equivs(&start_expr, &goal_expr);
  if equivs.is_empty() {
    panic!("Couldn't prove goal {}: {}", start, goal);
  }

}
#[test]
fn autostairstep() {
//   ((min(((((v0 + 13)/4)*4) + v1), (v2 + 10)) + 4) <= 
// min(((((min((((v0 + 17)/4)*4), (min((v2 - v1), v0) + 14)) + 3)/4)*4) + v1), (v2 + 14)))

let fullruleset = &[
  add_3term_comm(),
  min_sub_outside(),
  le_add_cancel(),
  add_inside_mul_div(),
  add_min_distributive(),
  move_one_add(),
  add_assoc(),
  min_div_bound(),
  add_min_distributive(),
//  add_assoc().flip(),
  move_one_add().flip(),
  div_min_distributive(),
  add_sub_shuffle(),
  another_min_div_bound(),
  le_refl(),
];

  let phase1 = &[
    add_3term_comm(),
    min_sub_outside(),
    le_add_cancel(),
    add_inside_mul_div(),
    add_min_distributive(),
  ];

  let phase2 = &[
    min_div_bound(),
    add_min_distributive(),
  ];
  let phase3 = &[
    div_min_distributive(),
    add_sub_shuffle(),
    another_min_div_bound(),
    le_refl(),
  ];

  let start = "(<= (min (+ (+ (* (div (+ x a) b) b) y) b)
                        (+ z (+ a 1)))
                   (min (+ (* (div (+ (min (* (div (+ x (+ a b)) b) b)
                                           (+ (min (- z y) x) (+ a 1))) (- b 1)) b) b) y)
                        (+ z (+ a 1))))";
  let goal = "T";
  let start_expr = HalideExpr.parse_expr(start).unwrap();
  let goal_expr = HalideExpr.parse_expr(goal).unwrap();
  // initialize the egraph with the start expression
  let (mut egraph, _old_root) = EGraph::<HalideExpr, ()>::from_expr(&start_expr);
  // run the rules over the egraph for some bounded loop

  println!("Original egraph size: {}", egraph.total_size());
  for i in 0..5 {
    for rw in fullruleset {
      rw.run(&mut egraph);
      println!("Rule: {}, new egraph size: {}", rw.name, egraph.total_size());
    }
    egraph.rebuild();
    println!("Ruleset round {}", i);
    println!("New egraph size: {}", egraph.total_size());
    let check_equivs = egraph.equivs(&start_expr, &goal_expr);
    if ! check_equivs.is_empty() {
      println!("Done early!~");
      break;
    }
  }

  // check if egraph.equivs is empty
  let equivs = egraph.equivs(&start_expr, &goal_expr);
  if equivs.is_empty() {
    panic!("Couldn't prove goal {}: {}", start, goal);
  }
}
