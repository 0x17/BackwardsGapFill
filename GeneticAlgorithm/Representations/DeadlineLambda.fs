namespace RCPSP

open Utils
open GeneticOperators

module DeadlineLambda =  
    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let (minDeadline, maxDeadline) =
            (ps.UpperBoundForMakespanWithOC (fun r t -> ps.ZMax r),
             ps.UpperBoundForMakespanWithOC (fun r t -> 0))
                
        let init ix =
            (rand minDeadline maxDeadline, shuffle [1..ps.Jobs.Count])

        let crossover ((mdeadline, morder),(fdeadline,forder)) =
            ((mdeadline - fdeadline) / 2 + fdeadline, onePointCrossover morder forder)

        let mutate (deadline,order) =
            let ndeadline = if rand 0 1 = 0 then max minDeadline (deadline-1) else min (deadline+1) maxDeadline
            (ndeadline, neighborhoodSwapPermuts order)
                
        let fitness (deadline,order) =
            let stsOpt = ps.DeadlineCostMinHeur deadline (Seq.ofList order)
            if stsOpt.IsSome then ps.Profit stsOpt.Value
            else 0.0

        GeneticAlgorithm.solve init crossover mutate fitness numGens popSize pmutate