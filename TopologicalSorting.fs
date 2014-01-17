﻿namespace RCPSP

open Utils
open Microsoft.FSharp.Collections

module TopologicalSorting =
    let rec topSort jobs preds =
        if Set.isEmpty jobs then []
        else
            let x = Seq.find (Set.isEmpty << Set.intersect jobs << preds) jobs
            x :: topSort (Set.remove x jobs) preds

    let allTopSorts jobs preds =
        let candidatesForNext ordering =
            let unused = (Set.difference jobs (Set.ofSeq ordering))
            Set.filter (Set.isEmpty << Set.intersect unused << preds) unused
        let extendOrdering ordering =
            candidatesForNext ordering |> Set.map (fun candidate -> ordering @ [candidate])
        let extendOrderings orderings =
            orderings
            |> Set.map extendOrdering
            |> Set.unionMany
        foldItselfTimes extendOrderings (set [[1]]) (dec jobs.Count)

    let rec countTopSorts jobs preds =
        if Set.isEmpty jobs then bigint 1
        else            
            Seq.filter (Set.isEmpty << Set.intersect jobs << preds) jobs
            |> Seq.sumBy (fun eligible -> countTopSorts (Set.remove eligible jobs) preds)

    let feasibleTopSort jobs preds ordering =
        let rec helper e rest =
            if not(Set.isEmpty (Set.intersect (preds e) (Set.ofSeq rest))) then false
            else helper (List.head rest) (List.tail rest)
        helper (List.head ordering) (List.tail ordering)