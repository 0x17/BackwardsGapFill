namespace RCPSP

open Utils

module LambdaZr =
    let private mutateZr (ps:ProjectStructure) zr =
        Map.map (fun r zrval -> if rand 0 1 = 1 then min (ps.ZMax r) (inc zrval) else max 0 (dec zrval)) zr

    let private randomCrossoverZr (mother:Map<'A,'B>) (father:Map<'A,'B>) =
        Map.map (fun r zrvalM -> if rand 0 1 = 1 then zrvalM else Map.find r father) mother

    let solveWithGA (ps:ProjectStructure) popSize numGens pmutate =
        let valgen ix = Seq.map (fun r -> (r, rand 0 (ps.ZMax r))) ps.Resources |> Map.ofSeq 
        LambdaZrt.solveWithGAShared valgen randomCrossoverZr mutateZr (fun z r t -> Map.find r z) ps popSize numGens pmutate


