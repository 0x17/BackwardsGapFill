﻿namespace RCPSP

open Runners

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
        //scheduleVizCommand argv

        //smToGdxCommand argv
        //smToGdxCommand [|"../../Projekte/j90"|]

        smToGdxCommand (fun _ -> true) argv

        //smToGdxCommandRelevant argv
        
        //let ps = JsonParser.parse "test.json"

        //let ps = PSPLibParser.parse @"../../Projekte/j60/j6011_2.sm"
        //let ps = PSPLibParser.parse @"C:\Users\a.schnabel\Dropbox\Arbeit\Scheduling\Code\CPP-RCPSP-OC\MiniBeispiel.DAT"
        //GraphVisualisation.visualizePrecedenceGraph ps "MiniBeispielGraph"

        //ScheduleVisualisation.fileSelectionPrompt ()
        //ScheduleVisualisation.exactSolvePrompt ()

        //Runners.batchSolveInPathToCsv @"../../Projekte/testrun/" "resultsfortestrunMIP.csv"
        //Runners.batchStsToProfitCsv @"C:\Users\a.schnabel\Dropbox\Arbeit\Scheduling\Code\RCPSP-OC\Model\localsolver\testrun\results" "resutlsfortestrunLocalSolver.csv"
        
        (*let ps = PSPLibParser.parse @"../../Projekte/j30/j301_1.sm"
        GraphVisualisation.visualizePrecedenceGraph ps "j301_1.pdf"
        let gmsSol = fst3 <| GamsSolver.solve ps
        //let lsSol = fst3 <| LocalSolver.solve ps
        //ScheduleVisualisation.showSchedule "LocalSolver schedule" ps lsSol
        ScheduleVisualisation.showSchedules [("GAMS optimal schedule", ps, gmsSol)]
                                             //("LocalSolver schedule", ps, lsSol)]
        //System.pause*)

        0