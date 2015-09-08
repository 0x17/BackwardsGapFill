namespace RCPSP

open GeneticOperators

module Lambda =
    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let pickfunc = Sampling.generateDefaultNaiveSamplingPickFunc ps
        let init ix = pickfunc ix
        let crossover (mother,father) = onePointCrossover mother father
        let mutate = neighborhoodSwap ps.Preds
        let fitness = ps.Profit << ps.SerialSGSOC
        GeneticAlgorithm.solve init crossover mutate fitness numGens popSize pmutate