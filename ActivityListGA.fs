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
            if rix < 0 || oix < 0 || rix >= len || oix >= len then false
            else feasibleTopSort jobs preds (exchange λ rix oix)

        let mutate =
            let f n = (pown -1 (n+1)) * (n/2 + 1)
            fun λ ->
                let rix = rand 0 (List.length λ)
                let oscSeq = Seq.initInfinite (fun n -> rix + f n)
                let oix = Seq.find (fun i -> exchangeFeasible λ rix i || i > List.length λ) oscSeq
                if oix > List.length λ then λ
                else exchange λ rix oix

        //let cross λ1 λ2 =
            //let rval = rand 0 (List.length λ1)

        let mutationStep acc =
            printf "Step"
            let mutations = [1..10] |> List.map (fun i -> mutate acc)
            List.maxBy (fun m -> utility m) (acc :: mutations)

        let nsteps = 100
        foldItselfTimes mutationStep (topSort jobs preds) nsteps

