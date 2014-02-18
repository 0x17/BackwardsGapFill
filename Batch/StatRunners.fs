namespace RCPSP

open System.Text

open Runners
open Serialization
open Utils
open TopologicalSorting

module StatRunners =
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
            let (sts,solveTime) = GamsSolver.solve nps
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
                let (heurSched1, solveTime1) = ActivityListOptimizer.optimizeHeuristic ps (Some(GamsSolver.optTopSort ps.Jobs optSched))
                let (heurSched2, solveTime2) = ActivityListOCOptimizer.optimizeHeuristic ps
                spitAppend outFilename (sprintf "%s;%.2f;%.2f;%.2f;%.2f\n" f (ps.CalculateGap optSched heurSched1) solveTime1.TotalSeconds (ps.CalculateGap optSched heurSched2) solveTime2.TotalSeconds)

        spit outFilename "Filename;GapSSGS1;SlvTimeSSGS1;GapSSGS2;SlvTimeSSGS2\n"
        PSPLibParser.foreachProjInPath @"Projekte/32Jobs" writeGapForProj

        
