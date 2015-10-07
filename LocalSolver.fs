namespace RCPSP

open localsolver
open Utils

module LocalSolver =    
    let solve (ps:ProjectStructure) =
        use ls = new LocalSolver ()

        let model = ls.GetModel ()

        let lastJob = Set.maxElement ps.Jobs

        let timeWindow j = [(ps.EarliestFinishingTimes j) .. (ps.LatestFinishingTimes j)]

        // decision variables
        let xjt = Array2D.init ps.Jobs.Count ps.TimeHorizon.Length (fun i j -> model.Bool ())
        let zrt = Array2D.init (Seq.length ps.Resources) ps.TimeHorizon.Length (fun r t -> model.Int (0L, int64(ps.ZMax (r+1))))

        // objective function
        let objfunc =
            let revenueTerm = model.Sum()
            timeWindow lastJob
            |> List.iter (fun t -> revenueTerm.AddOperand(LSExpression.op_Multiply(ps.U t, xjt.[lastJob, t])))

            let costParts = model.Sum()
            let costsTerm = LSExpression.op_Multiply(-1.0, costParts)
            ps.Resources >< ps.TimeHorizon
            |> Seq.iter (fun (r,t) -> costParts.AddOperand(LSExpression.op_Multiply(ps.Kappa r, zrt.[r,t])))

            model.Sum(revenueTerm, costsTerm)

        // constraints

        // each activity once
        for j in ps.Jobs do
            let onceSum = model.Sum()
            timeWindow j
            |> Seq.iter (fun t -> onceSum.AddOperand(xjt.[j, t]))
            model.AddConstraint (LSExpression.op_Equality(onceSum, 1.0))

        // precedence restrictions
        for j in ps.Jobs do
            for i in ps.Preds j do
                let predFinishSum = model.Sum()
                timeWindow i |> Seq.iter (fun t -> predFinishSum.AddOperand(LSExpression.op_Multiply(int64(t), xjt.[i,t])))

                let jobStartSum = model.Sum()
                timeWindow j |> Seq.iter (fun t -> jobStartSum.AddOperand(LSExpression.op_Multiply(int64(t), xjt.[j,t])))

                jobStartSum.AddOperand(-float(ps.Durations j))

                model.AddConstraint(LSExpression.op_LessThanOrEqual(predFinishSum, jobStartSum))

        // capacity restrictions
        for r in ps.Resources do
            for t in ps.TimeHorizon do
                let cumulatedDemand = model.Sum()
                for j in ps.Jobs do
                    [t..t+(ps.Durations j)-1]
                    |> Seq.iter (fun tau -> cumulatedDemand.AddOperand(LSExpression.op_Multiply(int64(ps.Demands j r), xjt.[j,tau])))

                let totalCapacity = model.Sum(int64(ps.Capacities r), zrt.[r,t])

                model.AddConstraint(LSExpression.op_LessThanOrEqual(cumulatedDemand, totalCapacity))

        // NOTE: upper bound for overtime not needed, since domain already enforces this!        
        model.Maximize (objfunc)
        model.Close ()

        let phase = ls.CreatePhase ()
        phase.SetTimeLimit(3600)
        ls.Solve ()

        ls.GetSolution ()
    
