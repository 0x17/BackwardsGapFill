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
        let (z2,sts2) = ps.BackwardsGapFillHeuristic ()
        let zeroOc r t = 0
        let (z3,sts3) = (zeroOc, ps.SerialScheduleGenerationScheme zeroOc)
        let (z4,sts4) = (zeroOc, ps.ParallelScheduleGenerationScheme zeroOc)
        let (z5,sts5) = ps.ModifiedPsgsHeuristicDefault ()

        let optTopSort = GamsSolver.optTopSort ps.Jobs sts1
        let (z6,sts6) = ps.ModifiedPsgsHeuristic (Seq.ofList optTopSort) ()

        ScheduleVisualisation.showSchedules [("SSGS Heuristik w/out OC", ps, sts3, z3);
                                             ("PSGS Heuristik w/out OC", ps, sts4, z4);
                                             ("MIP Modell", ps, sts1, z1);                                             
                                             ("BGF Heuristik", ps, sts2, z2);
                                             ("Modified PSGS Heuristik", ps, sts5, z5);
                                             ("Modified PSGS Heuristik - Optimale Einplanungsreihenfolge", ps, sts6, z6);]

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData @"Projekte"
        //BatchRunner.addCostsAndLevelsToProjs @"Projekte"
        solveAndVisualize () |> ignore
        0
    
