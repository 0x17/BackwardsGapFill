namespace RCPSP

open Utils

module TopologicalSorting =
    let rec topSort jobs preds =
        if Set.isEmpty jobs then []
        else
            let x = Seq.find (Set.isEmpty << Set.intersect jobs << preds) jobs
            x :: topSort (Set.remove x jobs) preds

    let rec topSortPreserveOrder jobs preds =
        if List.isEmpty jobs then []
        else
            let x = Seq.find (Set.isEmpty << Set.intersect (Set.ofList jobs) << preds) jobs
            x :: topSortPreserveOrder (without x jobs) preds

    let rec randomTopSort jobs preds =
        if Set.isEmpty jobs then []
        else
            let eligibles = Seq.filter (Set.isEmpty << Set.intersect jobs << preds) jobs
            let x = Seq.item (rand 0 ((Seq.length eligibles)-1)) eligibles
            x :: topSort (Set.remove x jobs) preds
