namespace RCPSP

open Utils
open TopologicalSorting

module SSGSOC =
    let cleverSsgsHeuristic (ps:ProjectStructure) λ =
        let computeCandidateSchedule sts j t =
            let candidate = Map.add j t sts
            let jix = indexOf λ j
            let subλ = Seq.skip (inc jix) λ
            ps.SerialScheduleGenerationSchemeCore (fun r t -> 0) candidate subλ

        let chooseBestPeriod sts j tlower tupper =
            [tlower..tupper]
            |> Seq.filter (fun t -> ps.EnoughCapacityForJob (fun r t -> ps.ZMax r) sts j t)
            |> Seq.maxBy (fun t -> computeCandidateSchedule sts j t |> ps.Profit)
            
        let scheduleJob acc job =
            let tlower = ps.LastPredFinishingTime acc job
            let tupper = numsGeq tlower |> Seq.find (ps.EnoughCapacityForJob (fun r t -> 0) acc job)
            Map.add job (chooseBestPeriod acc job tlower tupper) acc

        Seq.fold scheduleJob (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)
