namespace RCPSP

open System.Collections.Generic

module Utils =
    let memoize f =
        let cache = Dictionary<_,_>()
        fun x ->
            if not(cache.ContainsKey x) then
                cache.Add(x, f x)
            cache.[x]

    let inc = (+) 1
    let dec = (+) -1
    let neg = (*) -1

    let fst3 (a, _, _) = a
    let snd3 (_, b, _) = b
    let trd3 (_, _, c) = c

    let numsGeq x = Seq.initInfinite ((+) x)
    let numsLeq x = Seq.initInfinite ((-) x)
    let repeat x = Seq.initInfinite (fun _ -> x)
    let allBut lb ub j = [lb..j-1]@[j+1..ub]

    let identity x = x
    let same (a,b) = a = b

    let contains o = Seq.exists ((=) o)
    let notin coll o = not (contains o coll)
    let indexOf seq elem = Seq.findIndex ((=) elem) seq
    let remove pred = List.filter (not << pred)
    let diff colla collb = List.filter (notin collb) colla
    let without o = remove ((=) o)
    let withoutOnce o lst =
        let ix = List.findIndex ((=) o) lst
        (List.take ix lst) @ (List.skip (ix+1) lst)

    let boolToInt v = if v then 1 else 0
    let boolToFloat = float << boolToInt

    let mapToFunc m k = Map.find k m
    let arrayToFunc s k = Map.ofArray s |>  Map.find k
    let map2DToFunc m k1 k2 = Map.find k2 <| Map.find k1 m

    let keys m = Map.toSeq m |> Seq.map fst
    let keyset m = keys m |> Set.ofSeq
    let vals m = Map.toSeq m |> Seq.map snd

    let (><) xs ys = Seq.collect (fun x -> Seq.map (fun y -> (x,y)) ys) xs      

    let parts (str:string) = Array.filter (fun (s:string) -> s.Length > 0) (str.Split [|' '|])
        
    let (rand, randFloat: unit -> float, randomlyChoose: seq<obj> -> obj) =
        let rgen = System.Random 23
        ((fun lb ub -> rgen.Next (lb, inc ub)),
         (fun _ -> rgen.NextDouble ()),
         (fun nums -> Seq.item (rgen.Next (0, Seq.length nums)) nums))

    let pickRandomNums n lb ub =
        let rec helper acc n =
            if n = 0 then acc
            else
                let mutable c = rand lb ub
                while contains c acc do
                    c <- rand lb ub
                helper (c :: acc) (n-1)
        helper [] n

    let replace oldChar newChar = String.map (fun c -> if c = oldChar then newChar else c)

    let rec shuffle lst =
        if List.isEmpty lst then lst
        else
            let rix = rand 0 (lst.Length-1)
            lst.Item(rix) :: shuffle (List.filter (fun x -> x <> lst.Item(rix)) lst)

    let gap (opt:float) (approx:float) = abs ((opt - approx) / (opt + boolToFloat (opt = 0.0)))

    let rec foldItselfConvergeHash f h seed =
        let v = f seed
        if h v <> h seed then foldItselfConvergeHash f h v
        else seed

    let foldItselfConverge f seed = foldItselfConvergeHash f id seed

    let rec foldItselfTimes f seed n =
        if n = 1 then f seed
        else f (foldItselfTimes f seed (n-1))

    let transitiveHull nodeToSet =
        memoize ((foldItselfConverge (fun acc -> Seq.append [acc] (Seq.map nodeToSet acc) |> Set.unionMany)) << nodeToSet)

    let swap (a,b) = (b,a)

    let removeRandomElement (lst: List<'T>) =
        let rix = rand 0 (lst.Count-1)
        let elem = lst.[rix]
        lst.RemoveAt(rix)
        elem

    let lookupOrVal key mapping v = if Map.containsKey key mapping then Map.find key mapping else v

    let withProbabilityOrElse p thenfunc elsefunc =
        if rand 1 100 <= p then thenfunc
        else elsefunc

    let pickWithDiscreteDistribution (distribution:Map<int, float>) =
        let plessthan j = distribution |> Map.filter (fun k pk -> k <= j) |> vals |> Seq.sum
        let cumulativeProbs = Map.map (fun j pj -> plessthan j) distribution
        let rval = randFloat ()
        Map.findKey (fun j cpj -> rval >= (if j = 0 then 0.0 else Map.find (dec j) cumulativeProbs) && rval < cpj) cumulativeProbs

    let pairmap f (a,b) = (f a, f b)

    let hours n = 3600.0 * float(n)
    let minutes n = 60.0 * float(n)

