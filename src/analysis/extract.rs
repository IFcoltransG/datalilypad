use super::{JoinPlan, Variables};
use egg::*;

pub(crate) struct BestJoin<'egraph> {
    pub egraph: &'egraph EGraph<JoinPlan, Variables>,
}

impl<'egraph> CostFunction<JoinPlan> for BestJoin<'egraph> {
    type Cost = f32;

    fn cost<C>(&mut self, enode: &JoinPlan, mut costs: C) -> Self::Cost
    where
        C: FnMut(egg::Id) -> Self::Cost,
    {
        // fewer maps
        // joins that have more variables, and more clauses
        // TODO: FIXME!
        match enode {
            JoinPlan::Source(_) => f32::INFINITY,
            JoinPlan::Set(ids) => ids.iter().map(|id| costs(*id)).sum(),
            JoinPlan::JoinSources(_) => f32::INFINITY,
            JoinPlan::Var(_) => 1.0,
            JoinPlan::ConstrainedVar(ids) => ids.iter().map(|id| costs(*id)).sum(),
            JoinPlan::ConstrainingVars([id, _]) => costs(*id) + 1.0,
            JoinPlan::VarTuple(ids) => ids.iter().map(|id| costs(*id)).sum(),
            JoinPlan::FrogMap(ids) => ids.iter().map(|id| costs(*id)).sum(),
            JoinPlan::FrogVariable(ids) => ids.iter().map(|id| costs(*id)).sum(),
            JoinPlan::FrogJoin(ids) => ids.iter().map(|id| costs(*id)).sum(),
            JoinPlan::FrogLeapjoin(ids) => ids.iter().map(|id| costs(*id)).sum(),
        }
        // start(x, y), end(y, z), pred(y)
        // FrogLeapjoin(VarTuple(Var("y")), ///, \\\)
    }
}
