namespace RCPSP

open ScheduleVisualisation
open Utils
open PSPLibParser

module Program =
    let solveAndVisualize () =
        let testFilename = "../../Projekte/Modellendogen0001.DAT"
        let ps = PSPLibParser.parse testFilename

        let (z1,sts1) = GamsSolver.solve ps
        let (z2,sts2) = ps.ComputeOptimalSchedule()

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1, z1); ("Heuristik", ps, sts2, z2)]

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData "../../Projekte"
        //BatchRunner.addCostsAndLevelsToProjs "../../Projekte"
        solveAndVisualize ()
        0
    
