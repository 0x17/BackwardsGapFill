namespace RCPSP

open Utils
open TopologicalSorting

module ActivityListGA =
    let exchange λ rix oix =
        let r = List.nth λ rix
        let o = List.nth λ oix
        List.mapi (fun i e -> if i = rix then o else if i = oix then r else e) λ

    let optimizeActivityList (ps:ProjectStructure) utility =
        let exchangeFeasible λ rix oix =
            let len = List.length λ
            if rix = oix || rix < 0 || oix < 0 || rix >= len || oix >= len then false
            else feasibleTopSort ps.Jobs ps.Preds (exchange λ rix oix)

        let mutate λ =
            let len = List.length λ
            let rix = rand 0 (len-1)
            let oix = (shuffle [0..len-1]) @ [len] |> Seq.find (fun i -> exchangeFeasible λ rix i || i = len) 
            if oix = len then λ
            else exchange λ rix oix

        let mutationStep population =
            printf "Step"
            let mutations = population |> List.map (fun individual -> foldItselfTimes mutate individual (rand 1 10))
            let allConsidered = (population @ mutations)
            let curMax = List.maxBy utility allConsidered
            let maxUtility = utility curMax
            let bestIndividuals = List.filter (fun m -> utility m = maxUtility) allConsidered
            (maxUtility, bestIndividuals)       

        let rec folder f pair =
            let npair = f (snd pair)
            if fst npair = fst pair then snd npair
            else folder f npair

        let maxUtil individuals = List.map utility individuals |> List.max

        let initialPopulation =
            let feasibles =
                (topSort ps.Jobs ps.Preds) :: (PriorityRules.allRules |> List.map (fun pr -> pr ps))
                |> List.filter (feasibleTopSort ps.Jobs ps.Preds)
            let maxUtility = maxUtil feasibles
            feasibles
            |> List.filter (fun i -> utility i = maxUtility)

        folder mutationStep (maxUtil initialPopulation, initialPopulation)

    let optimizeHeuristic (ps:ProjectStructure) =
        let utility = (ps.Profit << ps.CleverSSGSHeuristic)
        ps.CleverSSGSHeuristic (optimizeActivityList ps utility |> List.maxBy utility |> List.toSeq)