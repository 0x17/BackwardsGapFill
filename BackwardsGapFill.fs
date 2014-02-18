namespace RCPSP

open Utils
open TopologicalSorting

module BackwardsGapFill =
    let onePeriodLeftShift sts js =
        Map.map (fun j stj -> stj - (contains j js |> boolToInt)) sts

    let afterLatestGaps (ps:ProjectStructure) sts =
        let lastJob = ps.LastJob
        let rec processRest alg rest =
            if List.isEmpty rest then alg
            else
                let j = List.head rest
                let latestFt = ps.LastPredFinishingTime sts j
                let latestPreds =
                    if latestFt = sts.[j] then (ps.Preds j |> Seq.filter (fun i -> sts.[i] + ps.Durations i = latestFt))
                    else Seq.empty
                let nrest = Seq.fold (fun acc i -> i :: acc) rest.Tail latestPreds
                processRest (j :: alg) nrest
        if sts.[lastJob] = ps.EarliestStartingTimes lastJob then List.empty
        else processRest List.empty [lastJob]

    let tryDecrementMakespan (ps:ProjectStructure) sts =
        let alg = afterLatestGaps ps sts
        if alg.IsEmpty then None
        else
            let nsts = onePeriodLeftShift sts alg
            if ps.ScheduleFeasibleWithMaxOC nsts then Some(nsts)
            else None

    let backwardsGapFillHeuristic (ps:ProjectStructure) λ =
        let rec buildProfitToSchedulesMapping acc sts =
            let nstsOption = tryDecrementMakespan ps sts
            if nstsOption.IsSome then
                let nsts = nstsOption.Value
                let p = ps.Profit nsts
                let nval = if Map.containsKey p acc then sts else nsts
                buildProfitToSchedulesMapping (Map.add p nval acc) nsts
            else acc

        let schedule = ps.SerialScheduleGenerationSchemeWithOC (fun r t -> 0) λ
        let profitsToSchedules = buildProfitToSchedulesMapping (Map.ofList [ps.Profit schedule, schedule]) schedule

        profitsToSchedules.[Seq.max (keys profitsToSchedules)]

    let backwardsGapFillHeuristicDefault ps = backwardsGapFillHeuristic ps (topSort ps.Jobs ps.Preds)