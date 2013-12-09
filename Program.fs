﻿namespace RCPSP

open System.Text

open Utils
open Serialization
open TopologicalSorting
open PSPLibParser
open ScheduleVisualisation

module Program =
    let saveOptimalSchedule ps filename = spitMap filename (fst <| GamsSolver.solve ps)
    let loadOptimalSchedule = slurpMap

    let testProjectStructure =
        //let testFilename = @"C:\Users\a.schnabel\Dropbox\Arbeit\Scheduling\CarolinGA\Release\Win32\veraltet\10+2\Modellendogen001.DAT"
        let testFilename = @"Projekte\Modellendogen0001.DAT"
        PSPLibParser.parse testFilename

    let visualizeGraph () =
        GraphVisualisation.visualizePrecedenceGraph testProjectStructure @"Modellendogen0001"

    let solveAndVisualize () =
        let ps = testProjectStructure       

        //let (sts1,solveTime) = GamsSolver.solve ps
        //saveOptimalSchedule ps "optsched.txt" sts1
        let (sts1,solveTime) = (loadOptimalSchedule "optsched.txt", 0)
        let sts2 = ps.BackwardsGapFillHeuristicDefault ()
        let zeroOc r t = 0
        let sts3 = ps.SerialScheduleGenerationScheme zeroOc
        let sts4 = ps.ParallelScheduleGenerationScheme zeroOc
        let sts5 = ps.ModifiedPsgsHeuristicDefault ()

        let optTopSort = GamsSolver.optTopSort ps.Jobs sts1
        let sts6 = ps.ModifiedPsgsHeuristic (Seq.ofList optTopSort) ()

        let sts7 = ps.ModifiedSsgsHeuristicDefault ()
        //let sts8 = ps.CleverSSGSHeuristicAllOrderings ()

        //printf "Gap = %.2f" <| ps.CalculateGap sts1 sts8        

        ScheduleVisualisation.showSchedules [("SSGS Heuristik w/out OC", ps, sts3);
                                             ("PSGS Heuristik w/out OC", ps, sts4);                                             
                                             ("BGF Heuristik", ps, sts2);
                                             ("Modified PSGS Heuristik", ps, sts5);
                                             ("Modified PSGS Heuristik - Optimal λ", ps, sts6);
                                             ("Modified SSGS Heuristik", ps, sts7);
                                             ("MIP Modell", ps, sts1);
                                             (*("SSGS2", ps, sts8);*)]
        ()

    let buildTableForVaryingKappas () =
        spit "kappaVariations.csv" "kappa;profit;makespan;total-oc;solve-time\n"
        let ps = testProjectStructure
        let infty = 999999999999999.0
        for kappa in infty :: [1.0 .. 0.1 .. 2.0] do
            let kappaFunc = (fun r -> kappa)
            let nps = ProjectStructure (ps.Jobs, ps.Durations, ps.Demands, ps.Costs, ps.Preds,
                                            ps.Resources, ps.Capacities, topSort ps.Jobs ps.Preds,
                                            ps.ReachedLevels, kappaFunc, ps.ZMax)
            let (sts,solveTime) = GamsSolver.solve nps
            let profit = nps.Profit sts
            let makespan = float(nps.Makespan sts)
            let totalOc = float(nps.TotalOvercapacityCosts sts)
            let parts = Array.map (fun n -> n.ToString().Replace('.',',')) [| kappa; profit; makespan; totalOc; solveTime |]
            spitAppend "kappaVariations.csv" (System.String.Join(";", parts) + "\n")
        ()

    let trySSGS2 () =
        let ps = testProjectStructure
        let sts = ps.CleverSSGSHeuristic ()
        ScheduleVisualisation.showSchedules [("SSGS2", ps, sts)]
        ()

    let showUStarPlot () =
        let ps = testProjectStructure
        PlotVisualisation.generatePlot ps.UStar ps.TimeHorizon "ustar"

    let genTopSorts () =
        let jobs = set [1..6]
        let preds j =
            match j with
            | 1 -> set []
            | 2 -> set [1]
            | 3 -> set [2]
            | 4 -> set [3]
            | 5 -> set [3]
            | 6 -> set [4; 5]
            | _ -> set []
        //allTopSorts jobs preds |> ignore
        //let topsorts = allTopSorts jobs preds
        //printf "Anzahl = %O" 
        //printf "%O" topsorts        
        printf "Anzahl = %O" (countTopSorts jobs preds)
        System.Console.ReadKey() |> ignore
        //()

    let countingTopOrderings () =
        let ps = testProjectStructure
        let nsorts = countTopSorts ps.Jobs ps.Preds
        printf "Num sorts = %O" nsorts
        System.Console.ReadKey () |> ignore

    let convertPrecedenceRelationToCArray () =
        let ps = testProjectStructure
        let sb = StringBuilder ("static int adjMx[NUM_JOBS*NUM_JOBS] = {\n")
        for i in ps.Jobs do
            let succs = ps.Succs i
            let rowEntries = Seq.map (fun j -> if succs.Contains j then "1" else "0") ps.Jobs
            sb.Append(System.String.Join(",", rowEntries)+",\n") |> ignore
        sb.Append("};") |> ignore
        spit "test.c" (sb.ToString())

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData @"Projekte"
        //BatchRunner.addCostsAndLevelsToProjs @"Projekte"
        //solveAndVisualize ()
        //buildTableForVaryingKappas ()
        //trySSGS2 ()
        showUStarPlot ()                
        //genTopSorts  ()
        //countingTopOrderings ()
        0
