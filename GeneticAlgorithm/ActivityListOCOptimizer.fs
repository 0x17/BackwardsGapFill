namespace RCPSP

open Microsoft.FSharp.Collections

open Utils
open TopologicalSorting
open Operators

module ActivityListOCOptimizer =
    type Individual = { al: int list; oc: int list list }    

    let optimizeIndividual (ps:ProjectStructure) (utility:Individual->float) =
        let generationSize = 80

        let numRes = Seq.length ps.Resources
        let numPeriods = ps.TimeHorizon.Length
        let numPeriodsMutate = int ((float numPeriods) * 0.05)

        let initOc = List.init numRes (fun r -> List.init numPeriods (fun t -> 0))
        let initpop = multiplex (List.map (fun al -> {al=al; oc=initOc})) (ActivityListOptimizer.initialActivityListPopulation ps None generationSize)        

        //==================================================================================================================
        let mutateAl = swapNeighborhood ps.Jobs ps.Preds

        let mutateOc (oc: int list list) =
            let mutateRow i row =
                let zmaxHalf = (ps.ZMax (i+1))/2
                let mutateIndices = pickRandomNums numPeriodsMutate 0 (numPeriods-1)
                row
                |> List.mapi (fun j ocVal ->
                    if contains j mutateIndices then
                        max 0 (min (ps.ZMax (i+1)) (ocVal + (rand -zmaxHalf zmaxHalf)))
                    else
                        ocVal)
            List.mapi mutateRow oc

        let mutateIndividual (indiv:Individual) = {al = mutateAl indiv.al; oc = mutateOc indiv.oc}

        let mutationStepGender population = population @ (List.map mutateIndividual population)
        let mutationStep = multiplex mutationStepGender

        //==================================================================================================================
        let crossoverOc (father: int list list) (mother: int list list) =
            let crossoverRes fatherOc motherOc =
                let ix = rand (numPeriods/2+1) (numPeriods-1)
                recombine ix fatherOc motherOc
            List.mapi (fun i e -> crossoverRes (father.Item (i)) e) mother

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
        let iterationStep = selectionStep << crossoverStep << mutationStep

        let numGenerations = 10
        let (bestMales, bestFemales) = foldItselfTimes iterationStep initpop numGenerations
        
        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) =
        stopwatchStart ()

        let scheduleForIndividual (iv:Individual) =
            let zfunc r t = iv.oc.Item(r-1).Item(t-1)
            FastSSGS.solve ps zfunc iv.al

        let utility = ps.ProfitForRemainingCapacity << scheduleForIndividual
            
        let (sts, remainingRes) = scheduleForIndividual (optimizeIndividual ps utility |> List.head)
        let solveTime = stopwatchStop ()

        (sts, solveTime)

