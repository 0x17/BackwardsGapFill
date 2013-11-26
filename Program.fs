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
        let zeroOc r t = 0
        let (z3,sts3) = (zeroOc, ps.SerialScheduleGenerationScheme zeroOc)

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1, z1);                                             
                                             ("BGF Heuristik", ps, sts2, z2);
                                             ("SSGS Heuristik", ps, sts3, z3)]

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData "../../Projekte"
        //BatchRunner.addCostsAndLevelsToProjs "../../Projekte"
        solveAndVisualize ()
        0
    
