use egg::*;
use std::borrow::{BorrowMut, Cow};
use std::cmp::Ordering;
use std::mem::{self, Discriminant};
use std::slice::{from_mut, from_ref};
use tap::Tap;

use crate::Rule;
use crate::StringSource;

pub(crate) struct Variables;
impl Analysis<JoinPlan> for Variables {
    type Data = Box<[String]>;

    fn make(egraph: &mut EGraph<JoinPlan, Self>, enode: &JoinPlan) -> Self::Data {
        let data = |id: &Id| &egraph[*id].data;
        let merge_all = |ids: &[Id]| {
            let mut new_data = vec![];
            for id in ids {
                new_data.extend_from_slice(&data(id)[..]);
            }
            new_data.sort();
            new_data.dedup();
            new_data.into_boxed_slice()
        };
        match enode {
            JoinPlan::Source(source) => source.variables.iter().map(|s| s.clone()).collect(),
            JoinPlan::Set(elements) => merge_all(elements),
            JoinPlan::JoinSources(sources) => data(sources).clone(),
            JoinPlan::Var(ident) => Box::new([ident.clone()]),
            JoinPlan::ConstrainedVar([var, _constraints]) => data(var).clone(),
            JoinPlan::ConstrainingVars([_expr, variables]) => data(variables).clone(),
            JoinPlan::VarTuple(vars) => merge_all(vars),
            JoinPlan::FrogMap([expr, ..]) => data(expr).clone(),
            JoinPlan::FrogVariable(vars) => merge_all(vars),
            JoinPlan::FrogJoin(exprs) => merge_all(exprs),
            JoinPlan::FrogLeapjoin(exprs) => merge_all(exprs),
        }
    }

    fn merge(&mut self, a: &mut Self::Data, b: Self::Data) -> DidMerge {
        match a.len().cmp(&b.len()) {
            Ordering::Less => {
                *a = b;
                DidMerge(true, false)
            }
            Ordering::Equal => DidMerge(false, false),
            Ordering::Greater => DidMerge(false, true),
        }
    }

    fn modify(egraph: &mut EGraph<JoinPlan, Self>, id: Id) {
        let data = egraph[id].data.clone();
        let mut variables = Vec::with_capacity(data.len());
        for ident in data {
            let var = egraph.add(JoinPlan::Var(ident));
            variables.push(var);
        }
        let set = egraph.add(JoinPlan::Set(variables.into()));
        let constrained = JoinPlan::ConstrainingVars([id, set]);
        let constrained_id =
            match egraph[id].for_each_matching_node(&constrained, |plan| match plan {
                JoinPlan::ConstrainingVars([original_id, _]) => Err(*original_id),
                _ => Ok(()),
            }) {
                Ok(()) => egraph.add(constrained),
                Err(original_id) => original_id,
            };
        egraph.union(id, constrained_id);
    }
}

#[derive(Hash, PartialEq, PartialOrd, Eq, Ord, Clone, Debug)]
pub(crate) enum JoinPlan {
    Set(Box<[Id]>), // commutative

    Source(StringSource), // clause
    JoinSources(Id),      // set of sources, represents the result of joining each clause

    // so we take out a single clause, and say it's joining (to the other clauses)
    // with all its variables
    // then we remove unneeded variables
    Var(String),
    ConstrainedVar([Id; 2]),   // var, plus set of sources constraining it
    ConstrainingVars([Id; 2]), // any term, plus set of vars it constrains
    VarTuple(Box<[Id]>),       // list of constrained vars

    // TODO FIXME
    FrogMap([Id; 3]), // input frog variable, plus pair of input output exprs

    FrogVariable([Id; 2]),   // key var tuple, plus other var tuple
    FrogJoin([Id; 3]),       // key var tuple, and the two frog variables it's joining
    FrogLeapjoin(Box<[Id]>), // key var tuple, and all frog variables it's joining
}

impl From<Rule> for RecExpr<JoinPlan> {
    fn from(Rule { sources, .. }: Rule) -> Self {
        let mut expr = RecExpr::default();
        let source_ids = sources
            .into_iter()
            .map(|source| expr.add(JoinPlan::Source(source.into())))
            .collect();
        let set_id = expr.add(JoinPlan::Set(source_ids));
        let _join_id = expr.add(JoinPlan::JoinSources(set_id));
        expr
    }
}

impl FromOp for JoinPlan {
    type Error = FromOpError;

    fn from_op(op: &str, children: Vec<Id>) -> Result<Self, Self::Error> {
        match op {
            "source" => unimplemented!(),
            "set" => Ok(JoinPlan::Set(children.into())),
            "vars" => Ok(JoinPlan::VarTuple(children.into())),
            "do_join" => {
                let [sources_set] = children[..] else {
                    panic!()
                };
                Ok(JoinPlan::JoinSources(sources_set))
            }
            "var" => unimplemented!(),
            "constr_var" => {
                let [var, sources] = children[..] else {
                    panic!()
                };
                Ok(JoinPlan::ConstrainedVar([var, sources]))
            }
            "constr_vars" => {
                let [expr, vars] = children[..] else { panic!() };
                Ok(JoinPlan::ConstrainingVars([expr, vars]))
            }

            "frog_map" => {
                let [var, input, output] = children[..] else {
                    panic!()
                };
                Ok(JoinPlan::FrogMap([var, input, output]))
            }

            "frog_var" => {
                let [key, data] = children[..] else { panic!() };
                Ok(JoinPlan::FrogVariable([key, data]))
            }
            "frog_join" => {
                let [key, left, right] = children[..] else {
                    panic!()
                };
                Ok(JoinPlan::FrogJoin([key, left, right]))
            }
            "frog_leapjoin" => {
                let [_key, _terms @ ..] = &children[..] else {
                    panic!()
                };
                Ok(JoinPlan::FrogLeapjoin(children.into()))
            }
            _ => Err(FromOpError::new(op, children)),
        }
    }
}

impl Language for JoinPlan {
    fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (JoinPlan::Source(source_a), JoinPlan::Source(source_b)) => source_a == source_b,
            (JoinPlan::Var(var_a), JoinPlan::Var(var_b)) => var_a == var_b,
            (JoinPlan::Set(_), JoinPlan::Set(_))
            | (JoinPlan::JoinSources(_), JoinPlan::JoinSources(_))
            | (JoinPlan::ConstrainedVar(_), JoinPlan::ConstrainedVar(_))
            | (JoinPlan::ConstrainingVars(_), JoinPlan::ConstrainingVars(_))
            | (JoinPlan::VarTuple(_), JoinPlan::VarTuple(_))
            | (JoinPlan::FrogMap(_), JoinPlan::FrogMap(_))
            | (JoinPlan::FrogVariable(_), JoinPlan::FrogVariable(_))
            | (JoinPlan::FrogJoin(_), JoinPlan::FrogJoin(_))
            | (JoinPlan::FrogLeapjoin(_), JoinPlan::FrogLeapjoin(_)) => true,
            _ => false,
        }
    }

    fn children(&self) -> &[Id] {
        match self {
            JoinPlan::Source(_) => &[],
            JoinPlan::Set(ids) => ids,
            JoinPlan::JoinSources(id) => from_ref(id),
            JoinPlan::Var(_) => &[],
            JoinPlan::ConstrainedVar(ids) => ids,
            JoinPlan::ConstrainingVars(ids) => ids,
            JoinPlan::VarTuple(ids) => ids,
            JoinPlan::FrogMap(ids) => ids,
            JoinPlan::FrogVariable(ids) => ids,
            JoinPlan::FrogJoin(ids) => ids,
            JoinPlan::FrogLeapjoin(ids) => ids,
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            JoinPlan::Source(_) => &mut [],
            JoinPlan::Set(ids) => ids,
            JoinPlan::JoinSources(id) => from_mut(id),
            JoinPlan::Var(_) => &mut [],
            JoinPlan::ConstrainedVar(ids) => ids,
            JoinPlan::ConstrainingVars(ids) => ids,
            JoinPlan::VarTuple(ids) => ids,
            JoinPlan::FrogMap(ids) => ids,
            JoinPlan::FrogVariable(ids) => ids,
            JoinPlan::FrogJoin(ids) => ids,
            JoinPlan::FrogLeapjoin(ids) => ids,
        }
    }

    type Discriminant = Discriminant<JoinPlan>;

    fn discriminant(&self) -> Self::Discriminant {
        mem::discriminant(self)
    }
}

pub(crate) struct SetPattern;
impl<A: Analysis<JoinPlan>> Searcher<JoinPlan, A> for SetPattern {
    fn search_eclass_with_limit(
        &self,
        egraph: &EGraph<JoinPlan, A>,
        eclass: Id,
        limit: usize,
    ) -> Option<SearchMatches<JoinPlan>> {
        if limit > 0 {
            let ast = Some(Cow::Owned(vec![ENodeOrVar::Var(Var::from_u32(0))].into()));
            for join_plan in egraph[eclass].iter() {
                if let JoinPlan::Set(_) = join_plan {
                    return Some(SearchMatches {
                        eclass: eclass,
                        substs: vec![Subst::with_capacity(1).tap_mut(|s| {
                            s.insert(Var::from_u32(0), eclass);
                        })],
                        ast,
                    });
                }
            }
        }
        return None;
    }

    fn vars(&self) -> Vec<Var> {
        vec![Var::from_u32(0)]
    }
}
impl<A: Analysis<JoinPlan>> Applier<JoinPlan, A> for SetPattern {
    fn apply_one(
        &self,
        egraph: &mut EGraph<JoinPlan, A>,
        eclass: Id,
        _: &Subst,
        _: Option<&PatternAst<JoinPlan>>,
        rule_name: Symbol,
    ) -> Vec<Id> {
        let class = &egraph[eclass];
        let mut new_sets = Vec::with_capacity(class.len() * 2);
        for join_plan in class.iter() {
            if let JoinPlan::Set(elements) = join_plan {
                if let Some(new_value) = rotate_box(elements) {
                    new_sets.push(JoinPlan::Set(new_value))
                }
                if let Some(new_value) = swap_box(elements) {
                    new_sets.push(JoinPlan::Set(new_value))
                }
            }
        }
        new_sets
            .into_iter()
            .filter_map(|set| {
                let id = egraph.add(set);
                egraph.union_trusted(eclass, id, rule_name).then(|| id)
            })
            .collect()
    }
}

fn rotate_box<V: Clone>(elements: &[V]) -> Option<Box<[V]>> {
    if elements.len() > 2 {
        let mut new = elements.iter().cloned().collect::<Box<_>>();
        new.rotate_right(1);
        Some(new)
    } else {
        None
    }
}
fn swap_box<V: Clone>(elements: &[V]) -> Option<Box<[V]>> {
    if elements.len() > 1 {
        let mut new = elements.iter().cloned().collect::<Box<_>>();
        let [x, y, ..]: &mut [V] = new.borrow_mut() else {
            panic!()
        };
        mem::swap(x, y);
        Some(new)
    } else {
        None
    }
}
