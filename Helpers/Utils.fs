namespace RCPSP

open System.Collections.Generic
open System
open System.Diagnostics

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

    let numsGeq x = Seq.initInfinite ((+) x)
    let repeat x = Seq.initInfinite (fun _ -> x)
    let allBut lb ub j = [lb..j-1]@[j+1..ub]

    let identity x = x
    let same (a,b) = a = b

    let contains o = Seq.exists ((=) o)
    let notin coll o = not (contains o coll)
    let indexOf seq elem = Seq.findIndex ((=) elem) seq
    let remove pred = List.filter (not << pred)
    let diff colla collb = colla |> List.filter (notin collb)
    let without o = remove ((=) o)
    let withoutOnce o lst =
        let ix = List.findIndex ((=) o) lst
        (Seq.toList (Seq.take ix lst)) @ (Seq.toList (Seq.skip (ix+1) lst))

    let boolToInt v = if v then 1 else 0
    let boolToFloat = float << boolToInt

    let mapToFunc m k = Map.find k m
    let arrayToFunc s k = Map.ofArray s |>  Map.find k
    let map2DToFunc m k1 k2 = Map.find k2 <| Map.find k1 m

    let keys m = Map.toSeq m |> Seq.map fst
    let vals m = Map.toSeq m |> Seq.map snd

    let (><) xs ys = Seq.collect (fun x -> Seq.map (fun y -> (x,y)) ys) xs      

    let parts (str:string) = Array.filter (fun (s:string) -> s.Length > 0) (str.Split [|' '|])
        
    let (rand, randomlyChoose: seq<obj> -> obj) =
        let rgen = System.Random 23
        ((fun lb ub -> rgen.Next (lb, inc ub)),
         (fun nums -> Seq.nth (rgen.Next (0, Seq.length nums)) nums))

    let pickRandomNums n lb ub =
        let rec helper acc n =
            if n = 0 then acc
            else
                let mutable c = rand lb ub
                while contains c acc do
                    c <- rand lb ub
                helper (c :: acc) (n-1)
        helper List.empty n

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
        else f (foldItselfTimes f seed (dec n))

    let rec foldItselfConvergeHash f h seed =
        let v = f seed
        if h v <> h seed then foldItselfConvergeHash f h v
        else seed

    let foldItselfConverge f seed = foldItselfConvergeHash f identity seed

    type RunBehavior =
        | Blocking
        | NonBlocking

    let runCmd behavior cmd args =
        let psi = ProcessStartInfo(cmd)
        psi.Arguments <- args
        let p = Process.Start psi
        match behavior with
        | Blocking -> p.WaitForExit ()
        | NonBlocking -> ()

    let replace oldChar newChar = String.map (fun c -> if c = oldChar then newChar else c)

    let onWindows = Environment.OSVersion.Platform = PlatformID.Win32NT

    let rec shuffle lst =
        if List.isEmpty lst then lst
        else
            let rix = rand 0 (lst.Length-1)
            lst.Item(rix) :: shuffle (List.filter (fun x -> x <> lst.Item(rix)) lst)

    let gap (opt:float) (approx:float) = abs ((opt - approx) / (opt + boolToFloat (opt = 0.0)))

    let transitiveHull nodeToSet =
        memoize (fun startNode -> foldItselfConverge (fun acc -> Seq.append [acc] (Seq.map nodeToSet acc) |> Set.unionMany) (nodeToSet startNode))

    let splitAt index lst =
        (Seq.take index lst |> Seq.toList,
         Seq.skip index lst |> Seq.toList)

    let multiplex transform pair =
        (transform (fst pair), transform (snd pair))

    let recombine index lstA lstB =
        let partA = Seq.take index lstA |> Seq.toList
        let partB = Seq.skip index lstB |> Seq.toList
        partA @ partB

    let (stopwatchStart, stopwatchStop) =
        let sw = Stopwatch ()
        ((fun () -> sw.Reset (); sw.Start ()),
         (fun () -> sw.Stop(); sw.Elapsed))

    let bypassAndPrint x =
        printf "%O\n" x
        x

    let arrayMapInPlace transform array =
        let mutable i = 0
        while i < Array.length array do
            array.[i] <- transform array.[i]
            i <- i + 1

    let swap (a,b) = (b,a)

    let removeRandomElement (lst: List<'T>) =
        let rix = rand 0 (lst.Count-1)
        let elem = lst.[rix]
        lst.RemoveAt(rix)
        elem
