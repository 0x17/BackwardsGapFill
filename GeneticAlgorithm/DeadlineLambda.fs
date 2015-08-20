namespace RCPSP

open Utils
open GeneticOperators

module DeadlineLambda =
    type Individual = { deadline: int; order: int list }
    
    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let makespanWithOc oc = seq { 1..ps.Jobs.Count } |> ps.SerialSGS oc |> ps.Makespan
        let (minDeadline, maxDeadline) = (makespanWithOc (fun r t -> ps.ZMax r), makespanWithOc (fun r t -> 0))
        
        let init popSize (ps:ProjectStructure) =
            Array.init popSize (fun i -> {deadline = rand minDeadline maxDeadline; order = shuffle [1..ps.Jobs.Count]})

        let crossover pair =
            let (mother,father) = pair
            let ddeadline = (mother.deadline - father.deadline) / 2 + father.deadline
            let dorder = onePointCrossoverPermuts mother.order father.order
            {deadline=ddeadline; order=dorder}

        let mutate individual =
            if rand 1 100 <= pmutate then
                let ndeadline = if rand 0 1 = 0 then max minDeadline (individual.deadline-1) else min (individual.deadline+1) maxDeadline
                let norder = neighborhoodSwapPermuts individual.order
                {deadline=ndeadline; order=norder}
            else individual
                
        let fitness (individual:Individual) =
            ps.DeadlineCostMinHeur individual.deadline (Seq.ofList individual.order)
            |> ps.Profit

        GeneticAlgorithm.solve (init popSize) crossover mutate fitness numGens ps