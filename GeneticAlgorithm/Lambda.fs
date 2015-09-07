namespace RCPSP

open Utils
open GeneticOperators

module Lambda =
    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let init popSize (ps:ProjectStructure) = Array.init popSize (fun i -> TopologicalSorting.randomTopSort ps.Jobs ps.Preds)

        let crossover (mother,father) = onePointCrossover mother father

        let mutate = withProbabilityOrElse pmutate (neighborhoodSwap ps.Preds) identity

        let fitness = ps.Profit << ps.SerialSGSOC

        GeneticAlgorithm.solve (init popSize) crossover mutate fitness numGens ps