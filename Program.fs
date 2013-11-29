namespace RCPSP

open System.Text

open ScheduleVisualisation
open Utils
open PSPLibParser

module Program =
    let solveAndVisualize () =
        let testFilename = @"Projekte/Modellendogen0001.DAT"
        let ps = PSPLibParser.parse testFilename

        //GraphVisualisation.visualizePrecedenceGraph ps @"Modellendogen0001"

        let (z1,sts1,solveTime) = GamsSolver.solve ps
        let (z2,sts2) = ps.BackwardsGapFillHeuristicDefault ()
        let zeroOc r t = 0
        let (z3,sts3) = (zeroOc, ps.SerialScheduleGenerationScheme zeroOc)
        let (z4,sts4) = (zeroOc, ps.ParallelScheduleGenerationScheme zeroOc)
        let (z5,sts5) = ps.ModifiedPsgsHeuristicDefault ()

        let optTopSort = GamsSolver.optTopSort ps.Jobs sts1
        let (z6,sts6) = ps.ModifiedPsgsHeuristic (Seq.ofList optTopSort) ()

        let (z7,sts7) = ps.ModifiedSsgsHeuristicDefault ()

        ScheduleVisualisation.showSchedules [//("SSGS Heuristik w/out OC", ps, sts3, z3);
                                             //("PSGS Heuristik w/out OC", ps, sts4, z4);
                                             ("MIP Modell", ps, sts1, z1);                                             
                                             ("BGF Heuristik", ps, sts2, z2);
                                             ("Modified PSGS Heuristik", ps, sts5, z5);
                                             //("Modified PSGS Heuristik - Optimal λ", ps, sts6, z6);
                                             ("Modified SSGS Heuristik", ps, sts7, z7);]
    let buildTableForVaryingKappas () =
        let sb = new StringBuilder ("kappa;profit;makespan;total-oc;solve-time\n")
        let testFilename = @"Projekte/Modellendogen0001.DAT"
        let ps = PSPLibParser.parse testFilename
        let infty = 999999999999999.0
        for kappa in infty :: [0.0 .. 0.1 .. 2.0] do
            let kappaFunc = (fun r -> kappa)
            let nps = new ProjectStructure (ps.Jobs, ps.Durations, ps.Demands, ps.Costs, ps.Preds,
                                            ps.Resources, ps.Capacities, Utils.topSort ps.Jobs ps.Preds,
                                            ps.ReachedLevels, kappaFunc, ps.ZMax)
            let (z,sts,solveTime) = GamsSolver.solve nps
            let profit = nps.Profit sts
            let makespan = float(nps.Makespan sts)
            let totalOc = float(nps.TotalOvercapacityCosts sts)
            let parts = Array.map (fun n -> n.ToString().Replace('.',',')) [| kappa; profit; makespan; totalOc; solveTime |]
            sb.Append (System.String.Join(";", parts) + "\n") |> ignore
        spit "kappaVariations.csv" (sb.ToString())

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData @"Projekte"
        //BatchRunner.addCostsAndLevelsToProjs @"Projekte"
        //solveAndVisualize () |> ignore
        buildTableForVaryingKappas () |> ignore
        0
    
