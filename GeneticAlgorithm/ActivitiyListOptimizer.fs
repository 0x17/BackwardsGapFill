namespace RCPSP

open Microsoft.FSharp.Collections

open Utils
open TopologicalSorting
open Operators

module ActivityListOptimizer =
    let initialActivityListPopulation (ps:ProjectStructure) (additionalCandidate:Option<int list>) targetSize =
        let baseCandidates = (topSort ps.Jobs ps.Preds) :: (PriorityRules.allRules |> List.map (fun pr -> pr ps))
        let candidates =
            match additionalCandidate with
                | Some addc -> addc :: baseCandidates
                | None -> baseCandidates

        let filledUpCandidates = 
            if targetSize > candidates.Length then
                candidates @ (List.init (80-candidates.Length) (fun i -> swapNeighborhood ps.Jobs ps.Preds baseCandidates.Head))
            else
                List.ofSeq (Seq.take targetSize candidates)

        match filledUpCandidates with
        | [a] -> ([a], [a])
        | [a;b] -> ([a], [b])
        | _ -> splitAt (candidates.Length/2) candidates

    let optimizeActivityList (ps:ProjectStructure) (additionalCandidate:Option<int list>) utility =
        let generationSize = 80
        let initpop = initialActivityListPopulation ps additionalCandidate generationSize

        let mutationStepGender population = List.map (swapNeighborhood ps.Jobs ps.Preds) population
        let mutationStep = multiplex mutationStepGender

        let crossoverStep population =
            let pairs = fst population >< snd population |> List.ofSeq
            //let pairs = randomPairs (fst population) (snd population)
            let daughters = pairs |> List.map (fun (f,m) -> onePointCrossoverDaughter f m)
            let sons = pairs |> List.map (fun (f,m) -> onePointCrossoverSon f m)
            (sons, daughters)

        let selectionStep population =
            let selectBest individuals =
                individuals
                |> PSeq.sortBy (fun iv -> -(utility iv))
                |> Seq.take generationSize
                |> List.ofSeq
            multiplex selectBest population

        let iterationStep = selectionStep << mutationStep << crossoverStep

        let numGenerations = 25
        let (bestMales, bestFemales) = foldItselfTimes iterationStep initpop numGenerations

        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) additionalCandidate =
        stopwatchStart ()
        let utility = (ps.Profit << ModifiedSSGS.cleverSsgsHeuristic ps)
        let sts = ModifiedSSGS.cleverSsgsHeuristic ps (optimizeActivityList ps additionalCandidate utility |> List.head |> List.toSeq)
        let solveTime = stopwatchStop ()
        (sts, solveTime)
