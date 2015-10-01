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

    let generateNaiveSamplingPickFunc (ps:ProjectStructure) (prioRules: (ProjectStructure -> int list) list) =
        let pickFunc ix =
            if ix < List.length prioRules then (List.item ix prioRules) ps
            else TopologicalSorting.randomTopSort ps.Jobs ps.Preds
        pickFunc

    let generateDefaultNaiveSamplingPickFunc (ps:ProjectStructure) =
        generateNaiveSamplingPickFunc ps PriorityRules.allRules
