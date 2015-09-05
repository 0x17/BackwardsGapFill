namespace RCPSP

open Utils
open TopologicalSorting

module PriorityRules =
    let orderJobsBy (ps:ProjectStructure) f  =
        topSortPreserveOrder (ps.Jobs |> Set.toList |> List.sortBy f) ps.Preds

    let orderJobsByRev ps f = orderJobsBy ps (neg << f)

    let spt ps = orderJobsBy ps ps.Durations    
    let lis ps = orderJobsBy ps (Set.count << ps.Succs)    
    let mts ps = orderJobsBy ps (Set.count << ps.TransSuccs)

    let lpt ps = orderJobsByRev ps ps.Durations
    let mis ps = orderJobsByRev ps (Set.count << ps.Succs)
    let lts ps = orderJobsByRev ps (Set.count << ps.TransSuccs)

    let grpw (ps:ProjectStructure) =
        let rankPositionalWeight j =
            let allSuccs = ps.TransSuccs j
            if Set.isEmpty allSuccs then 0
            else
                allSuccs
                |> Set.toList
                |> List.maxBy ps.Durations
        orderJobsBy ps rankPositionalWeight

    let est ps = orderJobsBy ps ps.EarliestStartingTimes
    let ect ps = orderJobsBy ps ps.EarliestFinishingTimes
    let lst ps = orderJobsBy ps ps.LatestStartingTimes
    let lct ps = orderJobsBy ps ps.LatestFinishingTimes

    let mslk (ps:ProjectStructure) =
        let slackTime j =
            ps.Deadline - ps.EarliestFinishingTimes j - ps.Durations j
        orderJobsBy ps slackTime

    let grr ps = orderJobsByRev ps (fun j -> Seq.sumBy (fun r -> ps.Demands j r) ps.Resources)

    let allRules = [spt; lis; mts; lpt; mis; lts; grpw; est; ect; lst; lct; mslk; grr]

