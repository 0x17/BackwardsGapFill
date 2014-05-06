namespace RCPSP

open System.Text

open Utils
open Serialization
open TopologicalSorting
open Runners

module TempRunners =
    let writePriorityRulesToFile () =
        let ps = testProjectStructure ()
        let orders = List.map (fun pr -> pr ps) PriorityRules.allRules
        spit "prules.txt" ""
        for order in orders do
            spitAppend "prules.txt" (System.String.Join(" ", order)+"\n")

    let fastSSGSBench () =
        let ps = testProjectStructure ()
        stopwatchStart ()
        for i in 0..1024*1024-1 do
            let (sts, rem) = FastSSGS.solve ps (fun r t -> 0) (TopologicalSorting.topSort ps.Jobs ps.Preds)
            ()

        //let sts = ps.SerialScheduleGenerationScheme ()
        let time = stopwatchStop ()
        printf "%O\n" time.TotalMilliseconds

    let testFastSSGS () =
        let ps = testProjectStructure ()
        //GraphVisualisation.visualizePrecedenceGraph ps @"Modellendogen001"
        let sts2 = ps.SerialScheduleGenerationScheme ()
        //printf "%s\n" (Serialization.mapToStr sts2)
        let (sts, remainingRes) = FastSSGS.solve ps (fun r t -> 0) (TopologicalSorting.topSort ps.Jobs ps.Preds)
        let partialSts = sts2 |> Map.filter (fun k v -> k < 10)
        let (sts3, remainingRes3) = FastSSGS.solvePartial ps (fun r t -> 0) partialSts (seq [10..ps.LastJob])
        System.Diagnostics.Debug.Assert((sts = sts2 && sts2 = sts3))
        //printf "%s\n" (Serialization.mapToStr sts)

        ScheduleVisualisation.showSchedules [("Slow", ps, sts2); ("Fast", ps, sts); ("Fast partial", ps, sts3)]

    let printTransitiveHulls () =
        let ps = testProjectStructure ()
        printf "%O\n%O\n" (ps.TransPreds ps.Jobs.MaximumElement) (ps.TransSuccs ps.Jobs.MinimumElement)

    let trySSGS2 () =
        let ps = testProjectStructure ()
        let sts = ModifiedSSGS.cleverSSGSHeuristicDefault ps
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

    let convertProjectStructureToCCode () =
        let ps = testProjectStructure ()

        let sb = StringBuilder ()

        sb.Append("const int NUM_JOBS = " + (string ps.Jobs.Count) + ";\n") |> ignore
        sb.Append("const int NUM_RES = " + (string (Seq.length ps.Resources)) + ";\n") |> ignore
        sb.Append("const int NUM_PERIODS = " + (string (ps.TimeHorizon.Length) + ";\n")) |> ignore

        sb.Append("static int capacities[NUM_RES] = {") |> ignore
        for r in ps.Resources do
            sb.Append((string (ps.Capacities r)) + ", ") |> ignore
        sb.Append("};\n") |> ignore

        sb.Append("static int adjMx[NUM_JOBS][NUM_JOBS] = {\n") |> ignore

        for i in ps.Jobs do
            let succs = ps.Succs i
            let rowEntries = Seq.map (fun j -> if succs.Contains j then "1" else "0") ps.Jobs
            sb.Append("{"+System.String.Join(",", rowEntries)+"},\n") |> ignore

        sb.Append("};\n") |> ignore

        sb.Append("static int durations[NUM_JOBS] = {") |> ignore
        for i in ps.Jobs do
            sb.Append(string (ps.Durations i) + ", ") |> ignore
        sb.Append("};\n") |> ignore

        sb.Append("static int demands[NUM_RES][NUM_JOBS] = {") |> ignore
        for r in ps.Resources do
            sb.Append("{") |> ignore
            for j in ps.Jobs do
                sb.Append((string (ps.Demands j r)) + ", ") |> ignore
            sb.Append("}\n") |> ignore
        sb.Append("};\n") |> ignore

        sb.Append("static int order[NUM_JOBS] = {") |> ignore
        let topOrder = topSort ps.Jobs ps.Preds
        for j in topOrder do
            sb.Append((string (j-1)) + ", ") |> ignore
        sb.Append("};\n") |> ignore
        
        spit "testAdjMx.c" (sb.ToString())

    let precomputeOptimalSchedules path =
        PSPLibParser.foreachProjInPath path (fun f ps ->
            let outfn = f+".OPTSCHED"
            if not (System.IO.File.Exists(outfn)) then
                spitMap outfn (fst (GamsSolver.solve ps)))
            
