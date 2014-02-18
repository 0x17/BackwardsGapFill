namespace RCPSP

open Microsoft.FSharp.Collections

open Utils
open TopologicalSorting
open Operators

module ActivityListOptimizer =
    let optimizeActivityList (ps:ProjectStructure) (additionalCandidate:Option<int list>) utility =
        let initpop =
            let baseCandidates = (topSort ps.Jobs ps.Preds) :: (PriorityRules.allRules |> List.map (fun pr -> pr ps))
            let candidates =
                match additionalCandidate with
                    | Some addc -> addc :: baseCandidates
                    | None -> baseCandidates
            match candidates with
            | [a] -> ([a], [a])
            | [a;b] -> ([a], [b])
            | _ -> splitAt (candidates.Length/2) candidates

        let generationSize = min (fst initpop).Length (snd initpop).Length

        let mutationStepGender population =
            List.map (swapNeighborhood ps.Jobs ps.Preds) population

        let mutationStep = multiplex mutationStepGender

        let crossoverStep population =
            let pairs = fst population >< snd population |> List.ofSeq
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

        let iterationStep = selectionStep << crossoverStep << mutationStep       

        let numGenerations = 4
        let (bestMales, bestFemales) = foldItselfTimes iterationStep initpop numGenerations

        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) additionalCandidate =
        stopwatchStart ()
        let utility = (ps.Profit << ModifiedSSGS.cleverSsgsHeuristic ps)
        let sts = ModifiedSSGS.cleverSsgsHeuristic ps (optimizeActivityList ps additionalCandidate utility |> List.head |> List.toSeq)
        let solveTime = stopwatchStop ()
        (sts, solveTime)
