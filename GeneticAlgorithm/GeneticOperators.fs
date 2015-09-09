namespace RCPSP

open Utils

module GeneticOperators =
    let onePointCrossoverSliceAt q mother father =
        let fromMother = List.take q mother
        let fromFather = diff father fromMother
        fromMother @ fromFather
        
    let onePointCrossover mother father =
        let q = rand 1 (List.length mother)
        onePointCrossoverSliceAt q mother father

    let swapGenes swapIx order =
        let ixMap i =
            if i = swapIx-1 then swapIx
            else if i = swapIx then swapIx-1
            else i
        List.permute ixMap order

    let neighborhoodSwapPermuts order = swapGenes (rand 1 (List.length order - 1)) order

    let swapFeasible swapIx preds order =
        List.item swapIx order
        |> preds
        |> Set.contains (List.item (swapIx-1) order)
        |> not

    let neighborhoodSwapAtIndex swapIx preds order =
        if swapFeasible swapIx preds order then swapGenes swapIx order
        else order

    let neighborhoodSwap preds order =
        let swapIx = rand 1 (List.length order - 1)
        neighborhoodSwapAtIndex swapIx preds order
