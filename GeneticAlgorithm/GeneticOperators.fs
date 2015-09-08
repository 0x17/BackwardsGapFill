namespace RCPSP

open Utils

module GeneticOperators =
    let onePointCrossover mother father =
        let q = rand 1 (List.length mother)
        let fromMother = List.take q mother
        let fromFather = diff father fromMother
        fromMother @ fromFather

    let neighborhoodSwapPermuts order =
        let swapIx = rand 1 (List.length order - 1)
        let ixMap i =
            if i = swapIx-1 then swapIx
            else if i = swapIx then swapIx-1
            else i
        List.permute ixMap order

    let neighborhoodSwap preds order =
        let swapIx = rand 1 (List.length order - 1)
        if Set.contains (List.item (swapIx-1) order) (preds (List.item swapIx order)) then
            order
        else
            let ixMap i =
                if i = swapIx-1 then swapIx
                else if i = swapIx then swapIx-1
                else i
            List.permute ixMap order
