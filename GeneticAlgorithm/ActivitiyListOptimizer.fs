namespace RCPSP

open Utils
open TopologicalSorting
open Operators

module ActivityListOptimizer =
    let optimizeActivityList (ps:ProjectStructure) utility =
        let initialPopulation =
            let candidates = (topSort ps.Jobs ps.Preds) :: (PriorityRules.allRules |> List.map (fun pr -> pr ps))
            let maxUtility = List.map utility candidates |> List.max
            let elite = List.filter (fun i -> utility i = maxUtility) candidates
            List.partition (fun j -> rand 1 2 = 1) elite

        let mutationStepGender population =
            let mutations = population |> List.map (fun individual -> foldItselfTimes (swapNeighborhood ps.Jobs ps.Preds) individual (rand 1 10))
            let allConsidered = (population @ mutations)
            let curMax = List.maxBy utility allConsidered
            let maxUtility = utility curMax
            let bestIndividuals = List.filter (fun m -> utility m = maxUtility) allConsidered
            bestIndividuals

        let mutationStep population =
            let males = mutationStepGender (fst population)
            let females = mutationStepGender (snd population)
            (males, females)

        let crossoverStep population =
            let pairs = fst population >< snd population |> Seq.toList
            let daughters = pairs |> List.map (fun (f,m) -> onePointCrossoverDaughter f m)
            let sons = pairs |> List.map (fun (f,m) -> onePointCrossoverSon f m)
            (sons, daughters)

        let iterationStep = crossoverStep << mutationStep

        let maxUtil population =
            (List.map utility (fst population) |> List.max,
             List.map utility (snd population) |> List.max)

        let rec folder acc =
            let oldAcc = acc
            let newAcc = iterationStep acc
            if maxUtil oldAcc = maxUtil newAcc then oldAcc
            else folder newAcc      

        let (bestMales, bestFemales) = folder initialPopulation
        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) =
        let utility = (ps.Profit << ps.CleverSSGSHeuristic)
        ps.CleverSSGSHeuristic (optimizeActivityList ps utility |> List.maxBy utility |> List.toSeq)