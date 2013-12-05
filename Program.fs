namespace RCPSP

open System.Text

open ScheduleVisualisation
open Utils
open PSPLibParser

module Program =
    let saveOptimalSchedule ps filename = spitMap filename (fst <| GamsSolver.solve ps)
    let loadOptimalSchedule = slurpMap

    let testProjectStructure =
        let testFilename = @"Projekte/Modellendogen0001.DAT"
        PSPLibParser.parse testFilename

    let visualizeGraph () =
        GraphVisualisation.visualizePrecedenceGraph testProjectStructure @"Modellendogen0001"

    let solveAndVisualize () =
        let ps = testProjectStructure       

        let (sts1,solveTime) = GamsSolver.solve ps
        saveOptimalSchedule ps "optsched.txt" sts1
        let sts2 = ps.BackwardsGapFillHeuristicDefault ()
        let zeroOc r t = 0
        let sts3 = ps.SerialScheduleGenerationScheme zeroOc
        let sts4 = ps.ParallelScheduleGenerationScheme zeroOc
        let sts5 = ps.ModifiedPsgsHeuristicDefault ()

        let optTopSort = GamsSolver.optTopSort ps.Jobs sts1
        let sts6 = ps.ModifiedPsgsHeuristic (Seq.ofList optTopSort) ()

        let sts7 = ps.ModifiedSsgsHeuristicDefault ()

        printf "Gap = %.2f" <| ps.CalculateGap sts1 sts2

        ScheduleVisualisation.showSchedules [("SSGS Heuristik w/out OC", ps, sts3);
                                             ("PSGS Heuristik w/out OC", ps, sts4);
                                             ("MIP Modell", ps, sts1);
                                             ("BGF Heuristik", ps, sts2);
                                             ("Modified PSGS Heuristik", ps, sts5);
                                             ("Modified PSGS Heuristik - Optimal λ", ps, sts6);
                                             ("Modified SSGS Heuristik", ps, sts7);]
        ()

    let buildTableForVaryingKappas () =
        let sb = new StringBuilder ("kappa;profit;makespan;total-oc;solve-time\n")
        let ps = testProjectStructure
        let infty = 999999999999999.0
        for kappa in infty :: [1.0 .. 0.1 .. 2.0] do
            let kappaFunc = (fun r -> kappa)
            let nps = new ProjectStructure (ps.Jobs, ps.Durations, ps.Demands, ps.Costs, ps.Preds,
                                            ps.Resources, ps.Capacities, Utils.topSort ps.Jobs ps.Preds,
                                            ps.ReachedLevels, kappaFunc, ps.ZMax)
            let (sts,solveTime) = GamsSolver.solve nps
            let profit = nps.Profit sts
            let makespan = float(nps.Makespan sts)
            let totalOc = float(nps.TotalOvercapacityCosts sts)
            let parts = Array.map (fun n -> n.ToString().Replace('.',',')) [| kappa; profit; makespan; totalOc; solveTime |]
            sb.Append (System.String.Join(";", parts) + "\n") |> ignore
            spitAppend "kappaVariations.csv" (sb.ToString())
        ()

    let trySSGS2 () =
        let ps = testProjectStructure
        let sts = ps.CleverSSGSHeuristic (fun r t -> 0)
        ScheduleVisualisation.showSchedules [("SSGS2", ps, sts)]
        ()

    let showUStarPlot () =
        let ps = testProjectStructure
        PlotVisualisation.generatePlot ps.UStar ps.TimeHorizon "ustar.dat"

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData @"Projekte"
        //BatchRunner.addCostsAndLevelsToProjs @"Projekte"
        //solveAndVisualize ()
        //buildTableForVaryingKappas ()
        //trySSGS2 ()
        showUStarPlot ()
        0
    
