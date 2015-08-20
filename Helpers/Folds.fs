namespace RCPSP

module Folds =
    let rec foldItselfUntil f seed pred =
        let v = f seed
        if pred v then v
        else foldItselfUntil f v pred

    let rec foldItselfUntilMaxSteps f seed pred n =
        if n = 0 then seed
        else
            let v = f seed
            if pred v then v
            else foldItselfUntilMaxSteps f v pred (n-1)

    let rec foldItselfTimes f seed n =
        if n = 1 then f seed
        else f (foldItselfTimes f seed (n-1))

    let rec foldItselfConvergeHash f h seed =
        let v = f seed
        if h v <> h seed then foldItselfConvergeHash f h v
        else seed

    let foldItselfConverge f seed = foldItselfConvergeHash f (fun x -> x) seed

