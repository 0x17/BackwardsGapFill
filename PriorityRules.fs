namespace RCPSP

module PriorityRules =
    let orderJobsBy (ps:ProjectStructure) f  =
        ps.Jobs
        |> Set.toList
        |> List.sortBy f

    let spt ps = orderJobsBy ps ps.Durations    
    let lis ps = orderJobsBy ps (Set.count << ps.Succs)    
    let mts ps = orderJobsBy ps (Set.count << ps.TransSuccs)

    let lpt: ProjectStructure -> int list = List.rev << spt
    let mis: ProjectStructure -> int list = List.rev << lis
    let lts: ProjectStructure -> int list = List.rev << mts

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

    let grr ps = orderJobsBy ps (((*) -1) << (fun j -> ps.Resources |> Seq.sumBy (fun r -> ps.Demands j r)))

    let allRules = [spt; lis; mts; lpt; mis; lts; grpw; est; ect; lst; lct; mslk; grr]

