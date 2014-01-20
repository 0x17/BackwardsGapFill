namespace RCPSP

module PriorityRules =
    let spt (ps:ProjectStructure) =
        ps.Jobs
        |> Set.toList
        |> List.sortBy ps.Durations

    let lpt: ProjectStructure -> int list = List.rev << spt

    let lis (ps:ProjectStructure) =
        ps.Jobs
        |> Set.toList
        |> List.sortBy (Set.count << ps.Succs)

    let mis: ProjectStructure -> int list = List.rev << lis
