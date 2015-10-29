namespace RCPSP

open localsolver

module LocalSolver =
    let solveWithTimeLimit (ps:ProjectStructure) tlimit =
        let inline (<<=>) (a:LSExpression) (b:LSExpression) = LSExpression.op_LessThanOrEqual(a,b)
        let inline (<=>) (a:LSExpression) (b:float) = LSExpression.op_Equality(a,b)
        let inline (<*>) (a:int) (b:LSExpression) = LSExpression.op_Multiply(int64(a),b)
        let inline (<**>) (a:float) (b:LSExpression) = LSExpression.op_Multiply(a,b)
        let inline (<->) (a:LSExpression) (b:int64) = LSExpression.op_Subtraction(a,b)

        use ls = new LocalSolver ()
        let model = ls.GetModel ()

        let timeWindow j = [| ps.EarliestFinishingTimes j .. ps.LatestFinishingTimes j |]

        // sets
        let horizon = 0 :: ps.TimeHorizon

        // decision variables
        let (x,z) =
            let xarr = Array2D.init ps.Jobs.Count (horizon.Length+1) (fun i j -> model.Bool ())
            let zarr = Array2D.init (Seq.length ps.Resources) (horizon.Length+1) (fun r t -> model.Int (0L, int64(ps.ZMax (r+1))))
            ((fun j t -> xarr.[j-1, t]), (fun r t -> zarr.[r-1, t]))

        // objective function
        let objfunc =
            let lastJob = Set.maxElement ps.Jobs
            let revenueTerm = [| for t in timeWindow lastJob -> ps.U t <**> x lastJob t |]
            let costTerm = [| for r in ps.Resources do for t in horizon -> -ps.Kappa r <**> z r t |]
            model.Sum(Array.append revenueTerm costTerm)

        // constraints
        let eachActivityOnce () =
            for j in ps.Jobs do
                model.Constraint(model.Sum([| for t in timeWindow j -> x j t |]) <=> 1.0)

        let precedenceRestrictions () =
            for j in ps.Jobs do
                for i in ps.Preds j do
                    let predFt = model.Sum([| for t in timeWindow i -> t <*> x i t |])
                    let jobSt = model.Sum( [| for t in timeWindow j -> t <*> x j t |]  ) <-> int64(ps.Durations j)
                    model.Constraint(predFt <<=> jobSt)

        let capacityRestrictions () =
            for r in ps.Resources do
                for t in horizon do
                    let cumulatedDemand = model.Sum([| for j in ps.Jobs -> ps.Demands j r <*> model.Sum([| for tau in t .. min horizon.Length (t+(ps.Durations j)-1) -> x j tau |]) |])
                    let totalCapacity = model.Sum(int64(ps.Capacities r), z r t)
                    model.Constraint(cumulatedDemand <<=> totalCapacity)

        let overtimeLimit () =
            for r in ps.Resources do
                for t in horizon do
                    model.Constraint(z r t <<=> (model.Sum(int64(ps.ZMax r))))

        // add constraints
        eachActivityOnce ()
        precedenceRestrictions ()
        capacityRestrictions ()

        // NOTE: upper bound for overtime not needed, since domain already enforces this!
        overtimeLimit ()
                
        model.Maximize (objfunc)
        model.Close ()

        let phase = ls.CreatePhase ()
        phase.SetTimeLimit(tlimit)

        let param = ls.GetParam()
        param.SetNbThreads(8)

        ls.Solve ()

        let sol = ls.GetSolution ()

        let stof j = Seq.find (fun t -> sol.GetValue (x j t) = 1L) horizon
        let sts = Map.ofList <| List.map (fun j -> (j, stof j)) (Set.toList ps.Jobs)

        let status = sol.GetStatus ()
        let solvetime = ls.GetStatistics().GetRunningTime () // in secs

        if status <> LSSolutionStatus.Feasible then
            raise (System.Exception("No feasible solution found!"))

        (sts, solvetime, status)

    let solveWithoutTimeLimit (ps:ProjectStructure) =
        solveWithTimeLimit ps System.Int32.MaxValue

    let solve (ps:ProjectStructure) = solveWithTimeLimit ps 2
    
