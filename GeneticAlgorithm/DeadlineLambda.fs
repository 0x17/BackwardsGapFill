namespace RCPSP

open Utils
open GeneticOperators

module DeadlineLambda =
    type Individual = { deadline: int; order: int list }
    
    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let (minDeadline, maxDeadline) = (ps.UpperBoundForMakespanWithOC (fun r t -> ps.ZMax r), ps.UpperBoundForMakespanWithOC (fun r t -> 0))
                
        let init ix = {deadline = rand minDeadline maxDeadline; order = shuffle [1..ps.Jobs.Count]}

        let crossover pair =
            let (mother,father) = pair
            let ddeadline = (mother.deadline - father.deadline) / 2 + father.deadline
            let dorder = onePointCrossover mother.order father.order
            {deadline=ddeadline; order=dorder}

        let mutate individual =
            let ndeadline = if rand 0 1 = 0 then max minDeadline (individual.deadline-1) else min (individual.deadline+1) maxDeadline
            let norder = neighborhoodSwapPermuts individual.order
            {deadline=ndeadline; order=norder}
                
        let fitness (individual:Individual) =
            let stsOpt = ps.DeadlineCostMinHeur individual.deadline (Seq.ofList individual.order)
            if stsOpt.IsSome then ps.Profit stsOpt.Value
            else 0.0

        GeneticAlgorithm.solve init crossover mutate fitness numGens popSize pmutate