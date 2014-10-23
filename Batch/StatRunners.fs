namespace RCPSP

open Runners
open Serialization
open Utils
open TopologicalSorting

module StatRunners =
    let (writeProjectRanges, findProjectWithBigRange) =
        let outFilename = "ranges.txt"
        let rangeOfProject filename =
            let ps = PSPLibParser.parse filename
            let (minMakespan, maxMakespan) = GamsSolver.minMaxMakespan ps
            maxMakespan-minMakespan
        let spitLineForProj pf = spitAppend outFilename (pf+";"+string(rangeOfProject pf)+"\n")      
        let projFiles = System.IO.Directory.GetFiles (@"Projekte/j30", "*.sm", System.IO.SearchOption.AllDirectories)
        ((fun () -> if not (System.IO.File.Exists (outFilename)) then
                        spit outFilename "proj;range\n"                    
                        projFiles |> Array.iter spitLineForProj
                    else
                        let alreadyComputed = slurpLines outFilename |> Array.map (fun line -> line.Split([|';'|]).[0]) |> Set.ofArray
                        let todo = Set.difference (Set.ofArray projFiles) alreadyComputed
                        todo |> Set.iter spitLineForProj),
         (fun () -> projFiles |> Array.maxBy rangeOfProject |> printf "%s\n"))

    let writeCostsForDeadlines ps =
        //GraphVisualisation.visualizePrecedenceGraph ps "mytesty.pdf" 

        let computeCostsForDeadlines (ps:ProjectStructure) =
            let (minMakespan, maxMakespan) = GamsSolver.minMaxMakespan ps        
            [minMakespan..maxMakespan]
            |> List.map (fun ms -> (ms, GamsSolver.solveMinimalCostsWithDeadline ps ms))
            |> Map.ofList

        let spitCostsForDeadlines msToCosts =
            spit "DeadlineCosts.txt" "Deadline;Costs\n"
            for ms in (keys msToCosts) do
                spitAppend "DeadlineCosts.txt" (sprintf "%d;%.2f\n" ms (msToCosts.Item(ms)))
            
        let msToCosts = computeCostsForDeadlines ps
        spitCostsForDeadlines msToCosts

    let writeCostsForDeadlinesExample () =
        let ps = PSPLibParser.parse @"Projekte/j30/j3021_9.sm"
        writeCostsForDeadlines ps

    let writeCostsAndRevenues () =
        let ps = PSPLibParser.parse @"Projekte/QBWLBeispiel.DAT"

        //GraphVisualisation.visualizePrecedenceGraph ps "mytest"

        let longestMs = ps.Makespan (fst (FastSSGS.solve ps (fun r t -> 0) (TopologicalSorting.topSort ps.Jobs ps.Preds)))
        let shortestMs = ps.Makespan (fst (FastSSGS.solve ps (fun r t -> ps.ZMax r) (TopologicalSorting.topSort ps.Jobs ps.Preds)))

        spit "ErloesKosten.txt" "Dauer;Erloes;Kosten;Deckungsbeitrag\n"

        for tau in shortestMs..longestMs do
            ps.TimeHorizon <- [1..tau]
            let (sts, solveTime, solveStat) = GamsSolver.solve ps
            ScheduleVisualisation.showSchedules [("Schedule", ps, sts)]
            spitAppend "ErloesKosten.txt" ((string tau)+";"+(string (ps.Revenue sts))+";"+(string (ps.TotalOvercapacityCosts sts))+";"+(string (ps.Revenue sts - ps.TotalOvercapacityCosts sts))+"\n")

    let writeOptsAndTime () =
        let writePriorityRules f ps =
            let orders = List.map (fun pr -> pr ps) PriorityRules.allRules
            spit (f+".PRULES") ""
            for order in orders do
                spitAppend (f+".PRULES") (System.String.Join(" ", order)+"\n")

        let minMaxMakespanSame (ps:ProjectStructure) =
            if ps.IsMinMaxMakespanBoundsSame then true
            else
                let (minMs, maxMs) = GamsSolver.minMaxMakespan ps
                minMs = maxMs

        let projFiles = System.IO.Directory.GetFiles(@"Projekte/j30", "*.sm", System.IO.SearchOption.AllDirectories)
        spit "optimalStats.txt" "File;Opt;Makespan;SlvTime;SlvStat\n"

        for f in projFiles do
            let ps = PSPLibParser.parse f

            if minMaxMakespanSame ps then
                spitAppend "optimalStats.txt" (f+";NA;NA;NA;NA\n")
            else
                let (sts, solveTime, solveStat) = GamsSolver.solve ps    

                //let solveTime = 0
                //let sts = slurpMap (f+".OPTSCHED")

                spitAppend "optimalStats.txt" (f+";"+string(ps.Profit sts)+";"+string(ps.Makespan sts)+";"+string(solveTime)+";"+string(solveStat)+"\n")
                spitMap (f+".OPTSCHED") sts
                writePriorityRules f ps

            printf "Wrote stats for %s\n" f

    let writeTminTmax fromFn toFn =
        let allProjFiles = System.IO.Directory.GetFiles(@"Projekte/j30", "*.sm", System.IO.SearchOption.AllDirectories)
        let fromIx = Array.findIndex (fun (fn:string) -> fn.EndsWith(fromFn)) allProjFiles
        let count = (Array.findIndex (fun (fn:string) -> fn.EndsWith(toFn)) allProjFiles) - fromIx + 1
        let projFiles = Array.sub allProjFiles fromIx count

        spit "tmintmax.txt" "File;Tmin;Tmax\n"
        for f in projFiles do
            let ps = PSPLibParser.parse f
            let makespans = GamsSolver.solveTminTmax ps
            spitAppend "variantMakespans.txt" (f+";"+string(makespans.Item(0))+";"+string(makespans.Item(1))+"\n")
            printf "Wrote makespan stats for %s\n" f

    let writeVariantMakespans () =
        let projFiles = System.IO.Directory.GetFiles(@"Projekte/j30", "*.sm", System.IO.SearchOption.AllDirectories)
        spit "variantMakespans.txt" "File;AndreUMS;CaroUMS;ParabelUMS;MinCostMS;MinMakespanMS\n"
        for f in projFiles do
            if not(f.Contains("3012")) then
                let ps = PSPLibParser.parse f
                let makespans = GamsSolver.solveVariants ps
                spitAppend "variantMakespans.txt" (f+";"+string(makespans.Item(0))+
                                                    ";"+string(makespans.Item(1))+
                                                    ";"+string(makespans.Item(2))+
                                                    ";"+string(makespans.Item(3))+
                                                    ";"+string(makespans.Item(4))+"\n")
                printf "Wrote makespan stats for %s\n" f       

    let finishOptsAndTime () =
        let alreadyMeasured filename =
            slurpLines "optimalStats.txt"
            |> Seq.exists (fun line -> line.StartsWith(filename))
        let projFiles = System.IO.Directory.GetFiles(@"Projekte/j30", "*.sm", System.IO.SearchOption.AllDirectories)
        for f in projFiles do
            if not(alreadyMeasured f) then
                let ps = PSPLibParser.parse f
                let (sts, solveTime, solveStat) = GamsSolver.solve ps    
                //let solveTime = 0
                //let sts = slurpMap (f+".OPTSCHED")
                spitAppend "optimalStats.txt" (f+";"+string(ps.Profit sts)+";"+string(solveTime)+"\n")
                spitMap (f+".OPTSCHED") sts
                let orders = List.map (fun pr -> pr ps) PriorityRules.allRules
                spit (f+".PRULES") ""
                for order in orders do
                    spitAppend (f+".PRULES") (System.String.Join(" ", order)+"\n")
                printf "Wrote stats for %s\n" f

    let GAtoExhaustiveEnumGap () =
        let ps = testProjectStructure ()
        let utility = ps.Profit << (ModifiedSSGS.cleverSsgsHeuristic ps)
        let enumOrdering = Seq.maxBy utility (allTopSorts ps.Jobs ps.Preds)
        let gaOrdering = Seq.maxBy utility (ActivityListOptimizer.optimizeActivityList ps None utility)
        printf "%.2f" (gap (utility enumOrdering) (utility gaOrdering))

    let buildTableForOrderingStats () =        
        let outFilename = "orderingStats.csv"
        spit outFilename "ordering;gap\n"
        let ps = testProjectStructure ()
        let (sts1,solveTime) = (slurpMap "optsched.txt", 0)
        let stats = ModifiedSSGS.cleverSSGSHeuristicOrderingStats ps sts1
        let lstStr (lst:seq<int>) = System.String.Join(",", lst)
        Map.iter (fun k v -> spitAppend outFilename (lstStr(k)+";"+(string(v) |> replace '.' ',')+"\n")) stats

    let buildTableForVaryingKappas () =
        let outFilename = "kappaVariations.csv"
        spit outFilename "kappa;profit;makespan;total-oc;solve-time\n"
        let ps = testProjectStructure ()
        let infty = 999999999999999.0
        for kappa in infty :: [0.0 .. 0.1 .. 2.0] do
            let kappaFunc = (fun r -> kappa)
            let nps = ProjectStructure (ps.Jobs, ps.Durations, ps.Demands, ps.Preds, ps.Resources,
                                        ps.Capacities, kappaFunc, ps.ZMax)
            let (sts, solveTime, solveStat) = GamsSolver.solve nps
            let profit = nps.Profit sts
            let makespan = float (nps.Makespan sts)
            let totalOc = float (nps.TotalOvercapacityCosts sts)
            let parts = Array.map (fun n -> n.ToString() |> replace '.' ',') [| kappa; profit; makespan; totalOc; solveTime |]
            spitAppend outFilename (System.String.Join(";", parts) + "\n")
        ()

    let writeGaps outFilename =
        let writeGapForProj f (ps:ProjectStructure) =
            //let (optSched, solveTime) = GamsSolver.solve ps
            let schedFn = f+".OPTSCHED"
            if System.IO.File.Exists (schedFn) then
                let optSched = slurpMap schedFn
                //let heurSched = ps.CleverSSGSHeuristicAllOrderings ()
                //let heurSched = ps.CleverSSGSHeuristic (GamsSolver.optTopSort ps.Jobs optSched |> Seq.ofList)
                //let heurSched = ActivityListOptimizer.optimizeHeuristic ps (Some(GamsSolver.optTopSort ps.Jobs optSched))                
                //let (heurSched2, solveTime2) = ActivityListOCOptimizer.optimizeHeuristic ps
                //printf "GA/SSGS done...\n"
                //let (heurSched3, solveTime3) = HelberIdee.optimizeHeuristic ps
                //printf "GA/SSGS-Helber done...\n"
                let (heurSched1, solveTime1) = ActivityListOptimizer.optimizeHeuristic ps (Some(GamsSolver.optTopSort ps.Jobs optSched))
                printf "GA/SSGS-OC done...\n"
                spitAppend outFilename (sprintf "%s;%.2f;%.2f\n" f (ps.CalculateGap optSched heurSched1) solveTime1.TotalSeconds)
                                                                                       //(ps.CalculateGap optSched heurSched2) solveTime2.TotalSeconds
                                                                                       //(ps.CalculateGap optSched heurSched3) solveTime3.TotalSeconds)

        spit outFilename "Filename;GapSSGS1;SlvTimeSSGS1\n" //";GapSSGS2;SlvTimeSSGS2;GapHelberIdee;SlvTimeHelberIdee\n"
        PSPLibParser.foreachProjInPath @"Projekte/32Jobs" writeGapForProj

        
