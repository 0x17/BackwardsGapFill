namespace RCPSP

open Runners

module Program =
    [<EntryPoint>]
    let main argv =
        //convertBatchSmToGdx false @"Projekte/j30"
        //convertResultsGdxToCsv @"../ClusterResultsFiltered" @"optimalResultsClusterFiltered.csv"
        extractSolveStatsFromGdx @"../ClusterResults" @"solveStatValues.csv"
        //copyRelevantInstances @"Projekte/j30" @"ClusterResults" @"ClusterResults2"
        //batchComputePriorityRules @"../Projekte/j120"
        0
