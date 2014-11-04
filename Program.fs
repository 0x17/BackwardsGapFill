namespace RCPSP

open Runners

module Program =
    [<EntryPoint>]
    let main argv =
        //convertBatchSmToGdx false @"Projekte/j30"
        //convertResultsGdxToCsv @"ClusterResults" @"optimalResultsCluster.csv"
        //extractSolveStatsFromGdx @"ClusterResults" @"solveStatValues.csv"
        batchComputePriorityRules @"Projekte/j30"
        0
