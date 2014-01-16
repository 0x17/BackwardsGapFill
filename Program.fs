namespace RCPSP

open System.Text

open Utils
open Serialization
open TopologicalSorting
open PSPLibParser
open ScheduleVisualisation

module Program =
    let testProjectStructure () =
        //let testFilename = @"Projekte/12Jobs/Modellendogen002.DAT"
        let testFilename = @"Projekte/32Jobs/Modellendogen0001.DAT"
        PSPLibParser.parse testFilename

    let visualizeGraph () =
        GraphVisualisation.visualizePrecedenceGraph (testProjectStructure ()) @"Modellendogen001"

    let solveAndVisualize () =
        let ps = testProjectStructure ()

        //visualizeGraph ()

        //let (sts1,solveTime) = GamsSolver.solve ps
        //spitMap "optsched.txt" sts1
        let (sts1,solveTime) = (slurpMap "optsched.txt", 0)

        let sts2 = ps.BackwardsGapFillHeuristicDefault ()
        let sts3 = ps.SerialScheduleGenerationScheme ()
        let sts4 = ps.ParallelScheduleGenerationScheme ()

        let optTopSort = GamsSolver.optTopSort ps.Jobs sts1
        let sts5 = ps.CleverSSGSHeuristic (Seq.ofList optTopSort)

        //let sts5 = ps.CleverSSGSHeuristicAllOrderings ()

        printf "Gap = %.2f" <| ps.CalculateGap sts1 sts5

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1);
                                             ("SSGS2", ps, sts5);]
        ()

    let buildTableForOrderingStats () =        
        let outFilename = "orderingStats.csv"
        spit outFilename "ordering;gap\n"
        let ps = testProjectStructure ()
        let (sts1,solveTime) = (slurpMap "optsched.txt", 0)
        let stats = ps.CleverSSGSHeuristicOrderingStats sts1
        let lstStr (lst:seq<int>) = System.String.Join(",", lst)
        Map.iter (fun k v -> spitAppend outFilename (lstStr(k)+";"+(string(v) |> replace '.' ',')+"\n")) stats

    let buildTableForVaryingKappas () =
        let outFilename = "kappaVariations.csv"
        spit outFilename "kappa;profit;makespan;total-oc;solve-time\n"
        let ps = testProjectStructure ()
        let infty = 999999999999999.0
        for kappa in infty :: [0.0 .. 0.1 .. 2.0] do
            let kappaFunc = (fun r -> kappa)
            let nps = ProjectStructure (ps.Jobs, ps.Durations, ps.Demands, ps.Preds,
                                            ps.Resources, ps.Capacities, kappaFunc, ps.ZMax)
            let (sts,solveTime) = GamsSolver.solve nps
            let profit = nps.Profit sts
            let makespan = float (nps.Makespan sts)
            let totalOc = float (nps.TotalOvercapacityCosts sts)
            let parts = Array.map (fun n -> n.ToString() |> replace '.' ',') [| kappa; profit; makespan; totalOc; solveTime |]
            spitAppend outFilename (System.String.Join(";", parts) + "\n")
        ()

    let trySSGS2 () =
        let ps = testProjectStructure ()
        let sts = ps.CleverSSGSHeuristicDefault ()
        ScheduleVisualisation.showSchedules [("SSGS2", ps, sts)]
        ()

    let showRevenuePlot () =
        let ps = testProjectStructure ()
        PlotVisualisation.generatePlot ps.U ps.TimeHorizon "revenue"

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
        let ps = testProjectStructure ()
        let sw = System.Diagnostics.Stopwatch()
        sw.Start ()
        let nsorts = countTopSorts ps.Jobs ps.Preds
        sw.Stop ()
        printf "Num sorts = %O\n" nsorts
        printf "Time elapsed = %d\n" sw.ElapsedMilliseconds
        System.Console.ReadKey () |> ignore

    let convertPrecedenceRelationToCArray () =
        let ps = testProjectStructure ()
        let sb = StringBuilder ("static int adjMx[NUM_JOBS*NUM_JOBS] = {\n")
        for i in ps.Jobs do
            let succs = ps.Succs i
            let rowEntries = Seq.map (fun j -> if succs.Contains j then "1" else "0") ps.Jobs
            sb.Append(System.String.Join(",", rowEntries)+",\n") |> ignore
        sb.Append("};") |> ignore
        spit "test.c" (sb.ToString())

    [<EntryPoint>]
    let main argv =
        //let projectFolder = @"Projekte/32Jobs"
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
        0
