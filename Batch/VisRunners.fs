namespace RCPSP

open Runners
open Serialization

module VisRunners =
    let visualizeGraph () =
        GraphVisualisation.visualizePrecedenceGraph (testProjectStructure ()) @"Modellendogen001"

    let solveAndVisualize () =
        let ps = testProjectStructure ()

        //visualizeGraph ()

        let optSchedFn = testFilename + ".OPTSCHED"

        //let (sts1,solveTime) = GamsSolver.solve ps        
        //spitMap optSchedFn sts1

        let (sts1,solveTime) = (slurpMap optSchedFn, 0)

        let sts2 = ps.BackwardsGapFillHeuristicDefault ()
        let sts3 = ps.SerialScheduleGenerationScheme ()
        let sts4 = ps.ParallelScheduleGenerationScheme ()

        //let sts5 = ps.CleverSSGSHeuristic (GamsSolver.optTopSort ps.Jobs sts1 |> Seq.ofList)
        //let sts5 = ps.CleverSSGSHeuristicAllOrderings ()        
        //let sts5 = ActivityListOptimizer.optimizeHeuristic ps (Some(GamsSolver.optTopSort ps.Jobs sts1))
        //let sts5 = ActivityListOptimizer.optimizeHeuristic ps None
        
        let sts6 = ActivityListOCOptimizer.optimizeHeuristic ps

        printf "Gap = %.2f" <| ps.CalculateGap sts1 sts6

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1);
                                             //("SSGS2/GA", ps, sts5);
                                             ("SSGS/GA", ps, sts6)]
        ()