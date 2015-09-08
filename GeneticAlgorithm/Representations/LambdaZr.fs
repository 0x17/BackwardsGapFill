namespace RCPSP

open GeneticOperators
open Utils

module LambdaZr =
    type Individual = { order: int list; zr: Map<int,int> }

    let private randomCrossoverZr (mother:Map<'A,'B>) (father:Map<'A,'B>) =
        Map.map (fun r zrvalM -> if rand 0 1 = 1 then zrvalM else Map.find r father) mother

    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let init ix =
            {order = TopologicalSorting.randomTopSort ps.Jobs ps.Preds;
             zr = Seq.map (fun r -> (r, rand 0 (ps.ZMax r))) ps.Resources |> Map.ofSeq }

        let crossover (mother,father) =
            {order = onePointCrossover mother.order father.order;
             zr = randomCrossoverZr mother.zr father.zr}

        let mutateOc zr =
            Map.map (fun r zrval -> if rand 0 1 = 1 then min (ps.ZMax r) (inc zrval) else max 0 (dec zrval)) zr
                        
        let mutate individual = {order = neighborhoodSwap ps.Preds individual.order; zr = mutateOc individual.zr}

        let fitness individual =
            individual.order
            |> List.toSeq
            |> ps.SerialSGS (fun r t -> Map.find r individual.zr) 
            |> ps.Profit

        GeneticAlgorithm.solve init crossover mutate fitness numGens popSize pmutate


