namespace RCPSP

open GeneticOperators
open Utils

module LambdaZrt =    
    let solveWithGAShared valgen crossoverZfunc mutateZfunc convZtoFunc (ps:ProjectStructure) popSize numGens pmutate =
        let pickfunc = Sampling.generateDefaultNaiveSamplingPickFunc ps
        let init ix = (pickfunc ix, valgen ix)
        let crossover ((morder,mz),(forder,fz)) = (onePointCrossover morder forder, crossoverZfunc mz fz)
        let mutate (order,z) = (neighborhoodSwap ps.Preds order, mutateZfunc ps z)
        let fitness (order,z) =
            order
            |> List.toSeq
            |> ps.SerialSGS (convZtoFunc z)
            |> ps.Profit
        GeneticAlgorithm.solve init crossover mutate fitness numGens popSize pmutate

    let private mutateZrt (ps:ProjectStructure) zrt =
        Map.map (fun (r,t) zrtval -> if rand 0 1 = 1 then min (ps.ZMax r) (inc zrtval) else max 0 (dec zrtval)) zrt

    let private randomCrossoverZrt (mother:Map<'A*'B, 'C>) (father:Map<'A*'B, 'C>) =
        Map.map (fun (r,t) zrtvalM -> if rand 0 1 = 1 then zrtvalM else Map.find (r,t) father) mother

    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let valgen ix = Seq.map2 (fun r t -> ((r,t), rand 0 (ps.ZMax r))) ps.Resources ps.TimeHorizon |> Map.ofSeq
        solveWithGAShared valgen randomCrossoverZrt mutateZrt (fun z r t -> Map.find (r,t) z) ps popSize numGens pmutate