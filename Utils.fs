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
    let numsGeq x = Seq.initInfinite ((+) x)
    let repeat x = Seq.initInfinite (fun _ -> x)
    let allBut lb ub j = [lb..j-1]@[j+1..ub]

    let contains o = Seq.exists ((=) o)
    let indexOf seq elem = Seq.findIndex ((=) elem) seq
    let remove pred = List.filter (not << pred)
    let without o = remove ((=) o)

    let boolToInt v = if v then 1 else 0

    let mapToFunc m k = Map.find k m
    let arrayToFunc s k = Map.ofArray s |>  Map.find k
    let map2DToFunc m k1 k2 = Map.find k2 <| Map.find k1 m

    let keys m = Map.toSeq m |> Seq.map fst
    let vals m = Map.toSeq m |> Seq.map snd

    let (><) xs ys = Seq.collect (fun x -> Seq.map (fun y -> (x,y)) ys) xs      

    let parts (str:string) = Array.filter (fun (s:string) -> s.Length > 0) (str.Split [|' '|])

    let rgen = System.Random ()
    let rand lb ub = rgen.Next (lb, inc ub)
    let randomlyChoose nums = Seq.nth (rgen.Next (0, Seq.length nums)) nums

    let rec foldItselfTimes f seed n =
        if n = 1 then f seed
        else f (foldItselfTimes f seed (dec n))

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
