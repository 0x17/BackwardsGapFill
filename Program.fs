namespace RCPSP

open Runners

module Program =
    [<EntryPoint>]
    let main argv =
        //convertBatchSmToGdx false @"Projekte/j30"
        //convertResultsGdxToCsv @"../ClusterResultsFiltered" @"optimalResultsClusterFiltered.csv"
        //extractSolveStatsFromGdx @"../ClusterResults" @"solveStatValues.csv"
        //copyRelevantInstances @"Projekte/j30" @"ClusterResults" @"ClusterResults2"
        //batchComputePriorityRules @"../Projekte/j120"
        (*Evaluation.evaluateResultsToTex @"../heursOptsAndTime30.txt" 3 (Some @"../OptProfits.csv")
        Evaluation.evaluateResultsToTex @"../heursOptsAndTime60.txt" 3 None
        Evaluation.evaluateResultsToTex @"../heursOptsAndTime90.txt" 3 None
        Evaluation.evaluateResultsToTex @"../heursOptsAndTime120.txt" 3 None*)
        Evaluation.evaluateMultipleResultsToTexFile [(@"../heursOptsAndTime30.txt", "j30", (Some @"../OptProfits.csv"));
                                                     (@"../heursOptsAndTime60.txt", "j60", None);
                                                     (@"../heursOptsAndTime90.txt", "j90", None);
                                                     (@"../heursOptsAndTime120.txt", "j120", None)] 3
        0
