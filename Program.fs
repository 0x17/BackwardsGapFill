namespace RCPSP

open Utils

module Program =
    let obsoleteCode =
        //convertBatchSmToGdx false @"Projekte/j30"
        //convertResultsGdxToCsv @"../ClusterResultsFiltered" @"optimalResultsClusterFiltered.csv"
        //extractSolveStatsFromGdx @"../ClusterResults" @"solveStatValues.csv"
        //copyRelevantInstances @"Projekte/j30" @"ClusterResults" @"ClusterResults2"
        //batchComputePriorityRules @"../Projekte/j120

        (*evaluateMultipleResultsToTexFile [(@"../HeursRawj30.csv", "j30", (Some @"../OptProfitsj30.csv"));
                                          (@"../HeursRawj60.csv", "j60", None);
                                          (@"../HeursRawj90.csv", "j90", None);
                                          (@"../HeursRawj120.csv", "j120", None)]*)

        (*multipleProfitsToRankings [@"../HeursRawj30.csv";
                                   @"../HeursRawj60.csv";
                                   @"../HeursRawj90.csv";
                                   @"../HeursRawj120.csv"]*)

        //let bestIndiv = DeadlineLambda.solveWithGA ps 80 100 5
        //let bestIndiv = Lambda.solveWithGA ps 2 2 5
        //let bestIndiv = LambdaZr.solveWithGA ps 80 100 5
        //System.Console.WriteLine("Best individual fitness = " + bestIndiv.ToString())

        //let sol = LocalSolver.solve ps

        //let ps = Runners.testProjectStructure ()
        //let sts = ps.SerialSGS (fun r t -> 0) (Set.toSeq ps.Jobs)
        //printf "%O" sts

        ()

    [<EntryPoint>]
    [<System.STAThreadAttribute>]
    let main argv =
        //ScheduleVisualisation.fileSelectionPrompt ()
        let ps = PSPLibParser.parse "../../Projekte/QBWLBeispiel.DAT"
        GraphVisualisation.visualizePrecedenceGraph ps "QBWLExample.pdf"
        let gmsSol = fst3 <| GamsSolver.solve ps
        //let lsSol = fst3 <| LocalSolver.solve ps
        ScheduleVisualisation.showSchedules [("GAMS optimal schedule", ps, gmsSol)]
                                             //("LocalSolver optimal schedule", ps, lsSol)]
        //System.pause
        0