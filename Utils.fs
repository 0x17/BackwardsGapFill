﻿namespace RCPSP

open System.Collections.Generic
open System
open System.IO
open System.Diagnostics

module Utils =
    let numsGeq x = Seq.initInfinite (fun i -> x+i)

    let contains o = Seq.exists (fun e -> e=o)
    let indexOf seq elem = Seq.findIndex (fun e -> e=elem) seq
    let remove pred = List.filter (not << pred)
    let without o = remove (fun e -> e=o)

    let mapToFunc m k = Map.find k m
    let arrayToFunc s k = Map.ofArray s |>  Map.find k
    let map2DToFunc m k1 k2 = Map.find k2 <| Map.find k1 m

    let keys m = Map.toSeq m |> Seq.map fst
    let vals m = Map.toSeq m |> Seq.map snd

    let cartesianProduct xs ys = Seq.collect (fun x -> Seq.map (fun y -> (x,y)) ys) xs

    let mapToStr (m:Map<int,int>) =
        String.Join("\n", Seq.map (fun k -> k.ToString () + "->" + (m.Item k).ToString ()) (keys m))
    let mapFromStr (s:string) t =
        let parseLine (line:string) =
            let lhsAndRhs = line.Split ([|"->"|], StringSplitOptions.None)
            (int lhsAndRhs.[0], t lhsAndRhs.[1])
        s.Split [|'\n'|] |> Array.map parseLine |> Map.ofArray
    let array2DToStr (a:int [,]) =
        let rowStr i = Seq.fold (fun acc j -> acc + " " + a.[i,j].ToString ())  "" [0..(Array2D.length2 a)-1]
        System.String.Join ("\n", Seq.map rowStr [0..(Array2D.length1 a)-1])

    let parts (str:string) = Array.filter (fun (s:string) -> s.Length > 0) (str.Split [|' '|])

    let rgen = new System.Random ()
    let rand lb ub = rgen.Next (lb, ub+1)

    let rec topSort jobs preds =
        if Set.isEmpty jobs then []
        else
            let x = Seq.find (Set.isEmpty << (Set.intersect jobs) << preds) jobs
            x :: topSort (Set.remove x jobs) preds

    let allTopSorts jobs preds =
        let rec traversePath lambda rest =
            if Set.isEmpty rest then [lambda]
            else
                List.ofSeq rest
                |> List.filter (Set.isEmpty << (Set.intersect rest) << preds)
                |> List.collect (fun candidate -> traversePath (lambda @ [candidate]) (Set.remove candidate rest))
        traversePath [] jobs

    let slurp = File.ReadAllText
    let slurpLines = File.ReadAllLines
    let spit filename content = File.WriteAllText (filename, content)
    let spitAppend filename content = File.AppendAllText (filename, content)

    let spitMap filename m = spit filename << mapToStr
    let slurpMap filename = mapFromStr <| slurp filename

    type RunBehavior =
        | Blocking
        | NonBlocking

    let runCmd behavior cmd args =
        let psi = new ProcessStartInfo(cmd)
        psi.Arguments <- args
        let p = Process.Start psi
        match behavior with
        | Blocking -> p.WaitForExit ()
        | NonBlocking -> ()