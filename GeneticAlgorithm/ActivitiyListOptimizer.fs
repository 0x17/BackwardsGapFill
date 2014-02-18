namespace RCPSP

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
            let mutations = population |> List.map (fun individual -> foldItselfTimes (swapNeighborhood ps.Jobs ps.Preds) individual (rand 1 10))
            population @ mutations

        let mutationStep = multiplex mutationStepGender

        let crossoverStep (population: int list list * int list list) =
            let pairs = fst population >< snd population |> Seq.toList
            //let pairs = List.map2 (fun m f -> (m,f)) (fst population) (snd population)
            let daughters = pairs |> List.map (fun (f,m) -> onePointCrossoverDaughter f m)
            let sons = pairs |> List.map (fun (f,m) -> onePointCrossoverSon f m)
            (sons, daughters)

        let selectionStep population =
            let selectBest individuals =
                individuals
                |> List.sortBy (fun iv -> -(utility iv))
                |> Seq.take generationSize
                |> List.ofSeq
            multiplex selectBest population

        let iterationStep = selectionStep << crossoverStep << mutationStep       

        //let maxUtil = multiplex (fun p -> List.map utility p |> List.max)
        //let (bestMales, bestFemales) = foldItselfConvergeHash iterationStep maxUtil initpop

        let numGenerations = 16
        let (bestMales, bestFemales) = foldItselfTimes iterationStep initpop numGenerations

        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) additionalCandidate =
        stopwatchStart ()
        let utility = (ps.Profit << ModifiedSSGS.cleverSsgsHeuristic ps)
        let sts = ModifiedSSGS.cleverSsgsHeuristic ps (optimizeActivityList ps additionalCandidate utility |> List.head |> List.toSeq)
        let solveTime = stopwatchStop ()
        (sts, solveTime)
