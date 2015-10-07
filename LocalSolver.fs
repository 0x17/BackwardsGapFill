namespace RCPSP

open localsolver

module LocalSolver =    
    let solve (ps:ProjectStructure) =
        let inline (<<=>) (a:LSExpression) (b:LSExpression) = LSExpression.op_LessThanOrEqual(a,b)
        let inline (<=>) (a:LSExpression) (b:float) = LSExpression.op_Equality(a,b)
        let inline (<*>) (a:int) (b:LSExpression) = LSExpression.op_Multiply(int64(a),b)
        let inline (<**>) (a:float) (b:LSExpression) = LSExpression.op_Multiply(a,b)

        use ls = new LocalSolver ()
        let model = ls.GetModel ()

        let timeWindow j = [|(ps.EarliestFinishingTimes j) .. (ps.LatestFinishingTimes j) - 1|]

        let horizon = 0 :: ps.TimeHorizon

        // decision variables
        let xjt = Array2D.init ps.Jobs.Count horizon.Length (fun i j -> model.Bool ())
        let zrt = Array2D.init (Seq.length ps.Resources) horizon.Length (fun r t -> model.Int (0L, int64(ps.ZMax (r+1))))

        // objective function
        let objfunc =
            let lastJob = Set.maxElement ps.Jobs
            let revenueTerm = model.Sum([| for t in timeWindow lastJob -> ps.U t <**> xjt.[lastJob-1, t]|])
            let costParts = model.Sum([| for r in ps.Resources do for t in horizon -> ps.Kappa r <**> zrt.[r-1,t] |])
            model.Sum(revenueTerm, -1 <*> costParts)

        // constraints

        // each activity once
        for j in ps.Jobs do
            model.AddConstraint (model.Sum([| for t in timeWindow j -> xjt.[j-1, t]|]) <=> 1.0)

        // precedence restrictions
        for j in ps.Jobs do
            for i in ps.Preds j do
                let predFinishSum = model.Sum([| for t in timeWindow i -> t <*> xjt.[i-1,t] |])
                let jobStartSum = model.Sum([| for t in timeWindow j -> t <*> xjt.[j-1,t] |])
                jobStartSum.AddOperand(int64(-ps.Durations j))
                model.AddConstraint(predFinishSum <<=> jobStartSum)

        // capacity restrictions
        for r in ps.Resources do
            for t in horizon do
                let cumulatedDemand = model.Sum([| for j in ps.Jobs do for tau in t..min (horizon.Length-1) (t+(ps.Durations j)-1) -> ps.Demands j r <*> xjt.[j-1,tau] |])
                let totalCapacity = model.Sum(int64(ps.Capacities r), zrt.[r-1,t])
                model.AddConstraint(cumulatedDemand <<=> totalCapacity)

        // NOTE: upper bound for overtime not needed, since domain already enforces this!        
        model.Maximize (objfunc)
        model.Close ()

        let phase = ls.CreatePhase ()
        phase.SetTimeLimit(3600)
        ls.Solve ()

        ls.GetSolution ()
    
