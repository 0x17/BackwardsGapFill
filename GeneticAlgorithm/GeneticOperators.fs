namespace RCPSP

open Utils

module GeneticOperators =
    let onePointCrossoverPermuts mother father =
        let q = rand 1 (List.length mother)
        let fromMother = mother |> Seq.take q |> Seq.toList        
        let fromFather = diff father fromMother
        fromMother @ fromFather

    let neighborhoodSwapPermuts order =
        let swapIx = rand 1 (List.length order - 1)
        let ixMap i =
            if i = swapIx-1 then swapIx
            else if i = swapIx then swapIx-1
            else i
        List.permute ixMap order

