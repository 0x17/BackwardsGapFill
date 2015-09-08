namespace RCPSP

open Utils

module Sampling =
    let private regret (pvals:Map<int,float>) j = Map.find j pvals - Seq.min (vals pvals)
    let private c pvals decisionset = 1.0 / (float(Seq.length decisionset) + (Seq.sumBy (regret pvals) decisionset))
    let private pselection c regretj = c * (regretj + 1.0)

    let regretBasedBiasedRandomSampling (ps:ProjectStructure) (pvals:Map<int,float>) =
        let selector decisionset =
            let cval = c pvals decisionset
            decisionset
            |> Seq.map (fun j -> (j, pselection cval (regret pvals j)))
            |> Map.ofSeq
            |> pickWithDiscreteDistribution
            
        TopologicalSorting.topOrderSelector ps.Jobs ps.Preds selector

    // TODO: Implement me!
    let betaBiasedRandomSampling: ProjectStructure -> Map<int,float> -> int list =
        (fun (ps:ProjectStructure) pvals -> List.ofSeq ps.CanonicalOrder)

    let generateNaiveSamplingPickFunc (ps:ProjectStructure) (prioRules: (ProjectStructure -> int list) list) =
        (fun ix -> if ix <= List.length prioRules then (List.item ix prioRules) ps else TopologicalSorting.randomTopSort ps.Jobs ps.Preds)

    let generateDefaultNaiveSamplingPickFunc (ps:ProjectStructure) = generateNaiveSamplingPickFunc ps PriorityRules.allRules
        

