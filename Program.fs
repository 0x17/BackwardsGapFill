namespace RCPSP

open Runners

module Program =
    [<EntryPoint>]
    let main argv =
        //let projectFolder = @"Projekte/16Jobs"
        //BatchRunner.stripAdditionalData projectFolder
        //BatchRunner.addAdditionalDataToProjs projectFolder
        solveAndVisualize ()
        //buildTableForVaryingKappas ()
        //buildTableForOrderingStats ()
        //trySSGS2 ()
        //showRevenuePlot ()                
        //genTopSorts  ()
        //countingTopOrderings ()
        //convertPrecedenceRelationToCArray ()
        //writeGaps @"gaps.txt"
        0
