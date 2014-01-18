namespace RCPSP

open Utils
open TopologicalSorting

module ActivityListGA =
    let optimizeActivityList jobs preds utility =
        let exchange λ rix oix =
            let r = List.nth λ rix
            let o = List.nth λ oix
            List.mapi (fun i e -> if i = rix then o else if i = oix then r else e) λ

        let exchangeFeasible λ rix oix =
            let len = List.length λ
            if rix = oix || rix < 0 || oix < 0 || rix >= len || oix >= len then false
            else feasibleTopSort jobs preds (exchange λ rix oix)

        let mutate λ =
            let len = List.length λ
            let rix = rand 0 (len-1)
            let oix = (shuffle [0..len-1]) @ [len] |> Seq.find (fun i -> exchangeFeasible λ rix i || i = len) 
            if oix = len then λ
            else exchange λ rix oix

        let mutationStep acc =
            printf "Step"
            let mutations = [1..100] |> List.map (fun i -> foldItselfTimes mutate acc (rand 1 10))
            let curMax = List.maxBy (fun m -> utility m) (acc :: mutations)
            printf "%.2f" (utility curMax)
            curMax

        let nsteps = 100
        foldItselfTimes mutationStep (topSort jobs preds) nsteps

        //let cross λ1 λ2 =
            //let rval = rand 0 (List.length λ1)
