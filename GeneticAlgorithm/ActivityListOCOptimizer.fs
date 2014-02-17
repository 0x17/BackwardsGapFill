namespace RCPSP

open Utils
open TopologicalSorting
open Operators

module ActivityListOCOptimizer =
    type Individual = { al: int list; oc: int list list }    

    let optimizeIndividual (ps:ProjectStructure) (utility:Individual->float) =
        let multiplex transform population =
            (transform (fst population),
             transform (snd population))

        let numRes = Seq.length ps.Resources
        let numPeriods = ps.TimeHorizon.Length

        let initOc = List.init numRes (fun r -> List.init numPeriods (fun t -> 0))

        let initpop =
            let alCandidates = (topSort ps.Jobs ps.Preds) :: (PriorityRules.allRules |> List.map (fun pr -> pr ps))
            let candidates = alCandidates |> List.map (fun al -> {al=al; oc=initOc})
            let maxUtility = List.map utility candidates |> List.max
            let elite = List.filter (fun i -> utility i = maxUtility) candidates
            match elite with
                | [a] -> ([a], [a])
                | [a;b] -> ([a], [b])
                | _ -> splitAt (elite.Length/2) elite

        //==================================================================================================================
        let mutateAl al =
            foldItselfTimes (swapNeighborhood ps.Jobs ps.Preds) al (rand 1 10)

        let mutateOc (oc: int list list) =
            (*List.mapi (fun i e ->
                let period = rand 0 (numPeriods-1)
                List.mapi (fun j f -> if j = period then rand 0 (ps.ZMax (i+1)) else f) e) oc*)
            List.mapi (fun i row -> List.map (fun col -> rand 0 (ps.ZMax (i+1))) row) oc

        let mutateIndividual (indiv:Individual) =
            {al = mutateAl indiv.al;
             oc = mutateOc indiv.oc}

        let mutationStepGender population = population @ List.map mutateIndividual population
        let mutationStep = multiplex mutationStepGender

        //==================================================================================================================
        let crossoverOc (father: int list list) (mother: int list list) =
            let crossoverRes fatherOc motherOc =
                let ix = rand (numPeriods/2+1) (numPeriods-1)
                let fatherPartOc = Seq.take ix fatherOc |> Seq.toList
                let motherPartOc = Seq.skip ix motherOc |> Seq.toList
                fatherPartOc @ motherPartOc
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
                let bestUtility = List.map utility individuals |> List.max
                printf "Best utility = %.2f\n" bestUtility
                List.filter (fun iv -> utility iv = bestUtility) individuals
            printf "\n"
            multiplex selectBest population

        //==================================================================================================================
        let iterationStep = selectionStep << crossoverStep << mutationStep
        //let maxUtil = multiplex (fun p -> List.map utility p |> List.max)
        //let (bestMales, bestFemales) = foldItselfConvergeHash iterationStep maxUtil initpop
        let numGenerations = 2
        let (bestMales, bestFemales) = foldItselfTimes iterationStep initpop numGenerations
        bestMales @ bestFemales

    let optimizeHeuristic (ps:ProjectStructure) =
        let scheduleForIndividual (iv:Individual) =
            let zfunc r t = iv.oc.Item(r-1).Item(t-1)
            ps.SerialScheduleGenerationSchemeWithOC zfunc (iv.al |> List.toSeq)

        let utility = ps.Profit << scheduleForIndividual
            
        scheduleForIndividual (optimizeIndividual ps utility |> List.head)