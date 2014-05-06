namespace RCPSP

open Microsoft.FSharp.Collections

open Utils
open TopologicalSorting
open Operators

module HelberIdee =
    type Individual = { al: int list; oc: bool list }    

    let helberSSGS (ps:ProjectStructure) (al:int list) (oc: bool list) =
        let scheduleJob acc jix =
            let j = al.Item(jix)
            if (*oc.Item(jix)*) oc.Item(j-1) then
                Map.add j (numsGeq (ps.LastPredFinishingTime acc j) |> Seq.find (ps.EnoughCapacityForJob (fun r t -> ps.ZMax r) acc j)) acc
            else
                Map.add j (numsGeq (ps.LastPredFinishingTime acc j) |> Seq.find (ps.EnoughCapacityForJob (fun r t -> 0) acc j)) acc
        List.fold scheduleJob (Map.ofList [(Seq.head al, 0)]) [1..al.Length-1]

    let optimizeIndividual (ps:ProjectStructure) (utility:Individual->float) =
        let generationSize = 80

        let numRes = Seq.length ps.Resources
        let numPeriods = ps.TimeHorizon.Length
        let numPeriodsMutate = int ((float numPeriods) * 0.05)

        let initOc = List.init ps.Jobs.Count (fun j -> false)
        let initpop = multiplex (List.map (fun al -> {al=al; oc=initOc})) (ActivityListOptimizer.initialActivityListPopulation ps None generationSize)        

        //==================================================================================================================
        let mutateAl = swapNeighborhood ps.Jobs ps.Preds

        let mutateOc (oc: bool list) =
            let r = rand 0 (oc.Length-1)
            oc |>
            List.mapi (fun i e -> if i = r then not(e) else e)

        let mutateIndividual (indiv:Individual) = {al = mutateAl indiv.al; oc = mutateOc indiv.oc}

        let mutationStepGender population = population @ (List.map mutateIndividual population)
        let mutationStep = multiplex mutationStepGender

        //==================================================================================================================
        let crossoverOc (father: bool list) (mother: bool list) =
            let ix = rand (father.Length/2+1) (father.Length-1)
            recombine ix father mother

        let crossoverIndividual (father:Individual) (mother:Individual) =
            {al = onePointCrossoverDaughter father.al mother.al;
             oc = crossoverOc father.oc mother.oc}

        let crossoverStep population =
            let (fathers, mothers) = population
            let pairs = fathers >< mothers |> Seq.toList
            let daughters = pairs |> List.map (fun (f,m) -> crossoverIndividual f m)
            let sons = pairs |> List.map (fun (f,m) -> crossoverIndividual m f)
            (fathers @ sons, mothers @ daughters)

        //==================================================================================================================
        let selectionStep population =
            let selectBest individuals =
                individuals
                |> PSeq.sortBy (fun iv -> -(utility iv))
                |> Seq.take (generationSize/2)
                |> List.ofSeq
            multiplex selectBest population

        //==================================================================================================================
        let iterationStep = selectionStep << crossoverStep << Utils.bypassAndPrint << mutationStep

        let numGenerations = 10
        let (bestMales, bestFemales) = foldItselfTimes iterationStep initpop numGenerations

        printf "Best = %O\n" (Seq.head bestMales).oc
        
        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) =
        stopwatchStart ()

        let scheduleForIndividual (iv:Individual) = helberSSGS ps iv.al iv.oc

        let utility = ps.Profit << scheduleForIndividual
            
        let sts = scheduleForIndividual (optimizeIndividual ps utility |> List.head)
        let solveTime = stopwatchStop ()

        (sts, solveTime)