use crate::Rule;
use egg::*;
use extract::BestJoin;
use join_plan::{JoinPlan, SetPattern, Variables};

mod extract;
mod join_plan;

#[allow(unused)]
fn analyse_rule(rule: Rule) -> RecExpr<JoinPlan> {
    let mut runner = <Runner<_, _, ()>>::new(Variables);

    let rule_expr = rule.into();
    let join = runner.egraph.add_expr(&rule_expr);

    let egg_rules = &[
        rewrite!("Constraining vars idempotent"; "(constr_vars ?a ?b)" => "?a"),
        rewrite!("Join empty set"; "(do_join (set))" => "(frog_var (vars) (vars))"),
        rewrite!("Join unit"; "(frog_join (vars) (frog_var (vars) (vars)) ?a)" => "?a"),
        // rewrite!("vars"; "" => ""),
        // rewrite!("do_join"; "" => ""),
        // rewrite!("var"; "" => ""),
        // rewrite!("constr_var"; "" => ""),
        // rewrite!("constr_vars"; "" => ""),
        // rewrite!("frog_map"; "" => ""),
        // rewrite!("frog_var"; "" => ""),
        // rewrite!("frog_join"; "" => ""),
        // rewrite!("frog_leapjoin"; "" => ""),
        Rewrite::new("Set commutativity", SetPattern, SetPattern).unwrap(),
    ];

    let runner = runner.run(egg_rules);
    let egraph = runner.egraph;
    let extractor = Extractor::new(&egraph, BestJoin { egraph: &egraph });
    let (_, best) = extractor.find_best(join);
    best

    // let source_variables: Vec<_> = sources
    //     .iter()
    //     .map(|Source { name, variables }| set_of_vars(variables))
    //     .enumerate()
    //     .collect();

    // let mut overlaps: BinaryHeap<(usize, Box<[_]>, [usize; 2])> =
    //     BinaryHeap::with_capacity(sources.len().pow(2));

    // for ((i_x, x), (i_y, y)) in source_variables.iter().tuple_combinations() {
    //     let overlap = x.intersection(y).collect::<Box<_>>();
    //     if !overlap.is_empty() {
    //         let pair: [usize; 2] = [*i_x, *i_y].tap_mut(|pair| pair.sort());
    //         overlaps.push((overlap.len(), overlap, pair));
    //     }
    // }

    // let overlap_groups = overlaps.iter().rev().chunk_by(|(_, overlap, _)| overlap);

    // for group in overlap_groups.into_iter() {}

    // fn set_of_vars(variables: &[Ident]) -> HashSet<Ident> {
    //     variables.iter().cloned().collect()
    // }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse::RuleSyntax, Rule};
    use proc_macro2::{Span, TokenStream};
    use quote::quote;
    use syn::{parse2, Ident};

    fn rules_from_tokenstream(tokens: TokenStream) -> Rule {
        let rules: RuleSyntax = parse2(tokens).unwrap();
        rules.try_into().unwrap()
    }

    #[test]
    fn analyse_empty() {
        let ident = Ident::new("TARGET", Span::call_site());
        let result = analyse_rule(Rule {
            target_name: ident,
            target_expressions: vec![],
            sources: vec![],
        });
        assert_eq!(
            result,
            RecExpr::from(vec![
                JoinPlan::VarTuple(Box::new([])),
                JoinPlan::FrogVariable([0.into(), 0.into()])
            ])
        );
    }

    #[test]
    fn analyse_single() {
        let rule = rules_from_tokenstream(
            quote! {
                TARGET() <- SOURCE(x, y)
            }
            .into(),
        );
        let result = analyse_rule(rule);
        assert_eq!(
            result,
            "(frog_var (vars) (vars))".parse().unwrap()
        );
    }

    #[test]
    fn analyse_simple_join() {
        let ident = Ident::new("TARGET", Span::call_site());
        let result = analyse_rule(Rule {
            target_name: ident,
            target_expressions: vec![],
            sources: vec![],
        });
        assert_eq!(
            result,
            RecExpr::from(vec![
                JoinPlan::VarTuple(Box::new([])),
                JoinPlan::FrogVariable([0.into(), 0.into()])
            ])
        );
    }

    #[test]
    fn simplify() {
        todo!();
        let ident = Ident::new("TARGET", Span::call_site());
        let result = analyse_rule(Rule {
            target_name: ident,
            target_expressions: vec![],
            sources: vec![],
        });
        eprintln!("{result:?}");
    }
}
