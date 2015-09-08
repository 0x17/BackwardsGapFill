namespace RCPSP

open GeneticOperators
open Utils

module LambdaTau =
    let solveWithGAShared valgen invertfunc sgsfunc (ps:ProjectStructure) popSize numGens pmutate =
        let pickfunc = Sampling.generateDefaultNaiveSamplingPickFunc ps

        let init ix = (pickfunc ix, List.init ps.Jobs.Count valgen)

        let crossover ((morder,mtau),(forder,ftau)) =
            (onePointCrossover morder forder,
             onePointCrossover mtau ftau)

        let mutate (order, tau) =
            let rix = rand 0 (dec ps.Jobs.Count)
            (neighborhoodSwap ps.Preds order,
             List.mapi (fun ix b -> if ix = rix then invertfunc(b) else b) tau)

        let fitness (order, tau) =
            sgsfunc (Seq.ofList order) (Seq.ofList tau)
            |> ps.Profit

        GeneticAlgorithm.solve init crossover mutate fitness popSize numGens pmutate

    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        solveWithGAShared (fun ix -> randFloat ()) (fun b -> 1.0 - b) ps.SerialSGSTau ps popSize numGens pmutate