namespace RCPSP

open Utils
open TopologicalSorting

module Operators =
    let swap λ rix oix =
        let r = List.nth λ rix
        let o = List.nth λ oix
        List.mapi (fun i e -> if i = rix then o else if i = oix then r else e) λ

    let swapFeasible jobs preds λ rix oix =
        let len = List.length λ
        if rix = oix || rix < 0 || oix < 0 || rix >= len || oix >= len then false
        else feasibleTopSort jobs preds (swap λ rix oix)

    let swapNeighborhood jobs preds λ =
        let len = List.length λ
        let rix = rand 0 (len-1)
        let oix = (shuffle [0..len-1]) @ [len] |> Seq.find (fun i -> swapFeasible jobs preds λ rix i || i = len)
        if oix = len then λ
        else swap λ rix oix

    let adjacentInterchange λ lix =
        swap λ lix (inc lix)

    let onePointCrossoverDaughter mother father =
        let len = List.length mother
        let q = rand 0 (len-1)
        let fromMother = List.ofSeq (Seq.take q mother)
        let fromFather = remove (fun j -> contains j fromMother) father
        fromMother @ fromFather

    let onePointCrossoverSon mother father = onePointCrossoverDaughter father mother

    let twoPointCrossoverDaughter mother father =
        let len = List.length mother
        let (q1,q2) = // TODO: Assert r1 strictly less than r2
            let (r1,r2) = (rand 0 (len-1), rand 0 (len-1))
            (min r1 r2, max r1 r2)
        let fromMotherLeft = List.ofSeq (Seq.take q1 mother)
        let fromMotherRight = List.ofSeq (Seq.skip q2 mother)
        let fromFather = remove (fun j -> contains j fromMotherLeft || contains j fromMotherRight) father
        fromMotherLeft @ fromFather @ fromMotherRight

    let twoPointCrossoverSon mother father = twoPointCrossoverDaughter father mother

    let rec randomPairs (males: int list list) (females: int list list) =
        if males.IsEmpty then []
        else
            let female = females.Item (rand 0 (females.Length-1))
            (males.Head, female) :: randomPairs males.Tail (without female females)
