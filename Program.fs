namespace RCPSP

open Runners
open VisRunners
open TempRunners
open StatRunners

module Program =
    let investigateAnomaly () =
        let ps = PSPLibParser.parse @"Projekte/j30/j3020_10.sm"
        (*let makespans = GamsSolver.solveVariants ps
        printf "%O" makespans*)
        //let (sts, solveTime) = GamsSolver.solve ps
        let ssgsCoreEs sts λ =
            let scheduleJob acc j =
                Map.add j (ps.LastPredFinishingTime acc j) acc
            Seq.fold scheduleJob sts λ
        let ssgsEs λ =
            ssgsCoreEs (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)
        let sts = (ssgsEs (TopologicalSorting.topSort ps.Jobs ps.Preds))
        //GraphVisualisation.visualizePrecedenceGraph ps @"Anomaly"
        ScheduleVisualisation.showSchedules [("Anomaly", ps, sts)]

    [<EntryPoint>]
    let main argv =
        //let projectFolder = @"Projekte/16Jobs"
        //BatchRunner.stripAdditionalData projectFolder
        //BatchRunner.addAdditionalDataToProjs projectFolder
        //solveAndVisualize ()
        //writePriorityRulesToFile ()
        
        //fastSSGSBench ()
        //writeOptsAndTime ()
        //finishOptsAndTime ()

        //writeCostsAndRevenues ()

        //testFastSSGS ()

        //GAtoExhaustiveEnumGap ()

        //buildTableForVaryingKappas ()
        //buildTableForOrderingStats ()

        //trySSGS2 ()
        //showRevenuePlot ()                
        //genTopSorts  ()
        //countingTopOrderings ()
        
        //convertProjectStructureToCCode ()

        //writeGaps @"gaps99999.txt"

        //precomputeOptimalSchedules @"Projekte/32Jobs"

        //printTransitiveHulls ()

        //countingTopOrderings ()

        //System.Console.ReadKey () |> ignore

        //writeVariantMakespans ()

        //scheduleVisTool argv

        //investigateAnomaly ()

        //testRCPSP ()

        //writeCostsForDeadlinesExample ()

        //testSolveWithDeadline ()

        //findProjectWithBigRange ()

        writeProjectRanges ()

        0
