namespace RCPSP

module Program =
    [<EntryPoint>]
    let main argv =
        (*let ps = Runners.testProjectStructure ()
        let sts = ps.SerialSGS (fun r t -> 0) (Set.toSeq ps.Jobs)
        printf "%O" sts*)
        
        
        //convertBatchSmToGdx false @"Projekte/j30"
        //convertResultsGdxToCsv @"../ClusterResultsFiltered" @"optimalResultsClusterFiltered.csv"
        //extractSolveStatsFromGdx @"../ClusterResults" @"solveStatValues.csv"
        //copyRelevantInstances @"Projekte/j30" @"ClusterResults" @"ClusterResults2"
        //batchComputePriorityRules @"../Projekte/j120"
        
        (*evaluateMultipleResultsToTexFile [(@"../HeursRawj30.csv", "j30", (Some @"../OptProfitsj30.csv"));
                                          (@"../HeursRawj60.csv", "j60", None);
                                          (@"../HeursRawj90.csv", "j90", None);
                                          (@"../HeursRawj120.csv", "j120", None)]*)

        (*multipleProfitsToRankings [@"../HeursRawj30.csv";
                                   @"../HeursRawj60.csv";
                                   @"../HeursRawj90.csv";
                                   @"../HeursRawj120.csv"]*)

        //let ps = Runners.testProjectStructure ()
        
        //let bestIndiv = DeadlineLambda.solveWithGA ps 80 100 5
        //let bestIndiv = Lambda.solveWithGA ps 2 2 5
        //let bestIndiv = LambdaZr.solveWithGA ps 12 4 5

        //System.Console.WriteLine("Best individual fitness = " + bestIndiv.ToString())

        System.Console.ReadKey () |> ignore        
        0
