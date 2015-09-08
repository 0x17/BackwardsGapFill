namespace RCPSP

open GeneticOperators
open Utils

module LambdaZrt =
    type Individual = { order: int list; zrt: Map<int*int,int> }

    let private randomCrossoverZrt (mother:Map<'A*'B, 'C>) (father:Map<'A*'B, 'C>) =
        Map.map (fun (r,t) zrtvalM -> if rand 0 1 = 1 then zrtvalM else Map.find (r,t) father) mother

    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let init ix =
            {order = TopologicalSorting.randomTopSort ps.Jobs ps.Preds;
             zrt = Seq.map2 (fun r t -> ((r,t), rand 0 (ps.ZMax r))) ps.Resources ps.TimeHorizon |> Map.ofSeq }

        let crossover (mother,father) =
            {order = onePointCrossover mother.order father.order;
             zrt = randomCrossoverZrt mother.zrt father.zrt}

        let mutateOc zrt =
            Map.map (fun (r,t) zrtval -> if rand 0 1 = 1 then min (ps.ZMax r) (inc zrtval) else max 0 (dec zrtval)) zrt
                        
        let mutate individual = {order = neighborhoodSwap ps.Preds individual.order; zrt = mutateOc individual.zrt}

        let fitness individual =
            individual.order
            |> List.toSeq
            |> ps.SerialSGS (fun r t -> Map.find (r,t) individual.zrt) 
            |> ps.Profit

        GeneticAlgorithm.solve init crossover mutate fitness numGens popSize pmutate