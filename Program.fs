namespace RCPSP

open ScheduleVisualisation
open Utils
open PSPLibParser

module Program =
    let solveAndVisualize () =
        let testFilename = @"Projekte/Modellendogen0001.DAT"
        let ps = PSPLibParser.parse testFilename

        GraphVisualisation.visualizePrecedenceGraph ps @"Modellendogen0001"

        let (z1,sts1) = GamsSolver.solve ps
        let (z2,sts2) = ps.ComputeOptimalSchedule()
        let zeroOc r t = 0
        let (z3,sts3) = (zeroOc, ps.SerialScheduleGenerationScheme zeroOc)
        let (z4,sts4) = (zeroOc, ps.ParallelScheduleGenerationScheme zeroOc)

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1, z1);
                                             ("BGF Heuristik", ps, sts2, z2);
                                             ("SSGS Heuristik", ps, sts3, z3);
                                             ("PSGS Heuristik", ps, sts4, z4);]

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData @"Projekte"
        //BatchRunner.addCostsAndLevelsToProjs @"Projekte"
        solveAndVisualize () |> ignore
        0
    
