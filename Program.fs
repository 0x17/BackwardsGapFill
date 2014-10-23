namespace RCPSP

open Runners

module Program =
    [<EntryPoint>]
    let main argv =
        convertBatchSmToGdx false @"Projekte/j30"
        0
