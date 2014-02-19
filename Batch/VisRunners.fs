namespace RCPSP

open Runners
open Serialization
open Utils

module VisRunners =
    let visualizeGraph () =
        GraphVisualisation.visualizePrecedenceGraph (testProjectStructure ()) @"Modellendogen001"

    let solveAndVisualize () =
        let ps = testProjectStructure ()

        //visualizeGraph ()

        let optSchedFn = testFilename + ".OPTSCHED"

        //let (sts1,solveTime) = GamsSolver.solve ps        
        //spitMap optSchedFn sts1

        let (sts1, solveTime1) = (slurpMap optSchedFn, 0)

        //let sts2 = ps.BackwardsGapFillHeuristicDefault ()
        //let sts3 = ps.SerialScheduleGenerationScheme ()
        //let sts4 = ps.ParallelScheduleGenerationScheme ()

        //let sts5 = ps.CleverSSGSHeuristic (GamsSolver.optTopSort ps.Jobs sts1 |> Seq.ofList)

        //stopwatchStart ()
        //let sts5 = ModifiedSSGS.cleverSSGSHeuristicAllOrderings ps
        //let solveTime5 = stopwatchStop ()   

        //let (sts5, solveTime5)  = ActivityListOptimizer.optimizeHeuristic ps None               

        let calcAndShowGaps () =
            let (sts5, solveTime5) = ActivityListOptimizer.optimizeHeuristic ps (Some(GamsSolver.optTopSort ps.Jobs sts1))
            let (sts6, solveTime6) = ActivityListOCOptimizer.optimizeHeuristic ps
            printf "Gap SSGS2/GA-AL = %.2f SolveTime=%.2f seconds\n" (ps.CalculateGap sts1 sts5) solveTime5.TotalSeconds
            printf "Gap SSGS/GA-AL-OC = %.2f SolveTime=%.2f seconds\n" (ps.CalculateGap sts1 sts6) solveTime6.TotalSeconds
            ()

        for i in 1..10 do
            calcAndShowGaps ()
        //calcAndShowGaps ()

        (*ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1);
                                             ("SSGS2/GA", ps, sts5);
                                             ("SSGS/GA", ps, sts6)]*)
        ()