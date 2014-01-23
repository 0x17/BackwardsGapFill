namespace RCPSP

open Runners
open VisRunners
open TempRunners
open StatRunners

module Program =
    [<EntryPoint>]
    let main argv =
        //let projectFolder = @"Projekte/16Jobs"
        //BatchRunner.stripAdditionalData projectFolder
        //BatchRunner.addAdditionalDataToProjs projectFolder

        //solveAndVisualize ()

        //GAtoExhaustiveEnumGap ()

        //buildTableForVaryingKappas ()
        //buildTableForOrderingStats ()

        //trySSGS2 ()
        //showRevenuePlot ()                
        //genTopSorts  ()
        //countingTopOrderings ()
        //convertPrecedenceRelationToCArray ()

        //writeGaps @"gaps.txt"

        precomputeOptimalSchedules @"Projekte/32Jobs"

        //printTransitiveHulls ()

        //countingTopOrderings ()

        System.Console.ReadKey () |> ignore
        0
