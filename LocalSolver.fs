namespace RCPSP

open localsolver
open Utils

module LocalSolver =    
    let solve (ps:ProjectStructure) =
        use ls = new LocalSolver ()

        let model = ls.GetModel ()

        let lastJob = Set.maxElement ps.Jobs

        let timeWindow j = [|(ps.EarliestFinishingTimes j) .. (ps.LatestFinishingTimes j)|]

        // decision variables
        let xjt = Array2D.init ps.Jobs.Count ps.TimeHorizon.Length (fun i j -> model.Bool ())
        let zrt = Array2D.init (Seq.length ps.Resources) ps.TimeHorizon.Length (fun r t -> model.Int (0L, int64(ps.ZMax (r+1))))

        // objective function
        let objfunc =
            let revenueTerm = model.Sum([| for t in timeWindow lastJob -> LSExpression.op_Multiply(ps.U t, xjt.[lastJob, t])|])
            let costParts = model.Sum([| for r in ps.Resources do for t in ps.TimeHorizon -> LSExpression.op_Multiply(ps.Kappa r, zrt.[r,t]) |])
            model.Sum(revenueTerm, LSExpression.op_Multiply(-1.0, costParts))

        // constraints

        // each activity once
        for j in ps.Jobs do
            model.AddConstraint (LSExpression.op_Equality(model.Sum([| for t in timeWindow j -> xjt.[j, t]|]), 1.0))

        // precedence restrictions
        for j in ps.Jobs do
            for i in ps.Preds j do
                let predFinishSum = model.Sum([| for t in timeWindow i -> LSExpression.op_Multiply(int64(t), xjt.[i,t]) |])
                let jobStartSum = model.Sum([| for t in timeWindow j -> LSExpression.op_Multiply(int64(t), xjt.[j,t]) |])
                jobStartSum.AddOperand(-float(ps.Durations j))
                model.AddConstraint(LSExpression.op_LessThanOrEqual(predFinishSum, jobStartSum))

        // capacity restrictions
        for r in ps.Resources do
            for t in ps.TimeHorizon do
                let cumulatedDemand = model.Sum([| for j in ps.Jobs do for tau in t..t+(ps.Durations j)-1 -> LSExpression.op_Multiply(int64(ps.Demands j r), xjt.[j,tau]) |])
                let totalCapacity = model.Sum(int64(ps.Capacities r), zrt.[r,t])
                model.AddConstraint(LSExpression.op_LessThanOrEqual(cumulatedDemand, totalCapacity))

        // NOTE: upper bound for overtime not needed, since domain already enforces this!        
        model.Maximize (objfunc)
        model.Close ()

        let phase = ls.CreatePhase ()
        phase.SetTimeLimit(3600)
        ls.Solve ()

        ls.GetSolution ()
    
