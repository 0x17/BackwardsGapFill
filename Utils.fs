namespace RCPSP

open System.Collections.Generic
open System
open System.IO

module Utils =
    let mapToFunc m k = Map.find k m
    let arrayToFunc s k = Map.ofArray s |>  Map.find k
    let map2DToFunc m k1 k2 = Map.find k2 <| Map.find k1 m
    let keys m = Map.toSeq  m |> Seq.map fst
    let vals m = Map.toSeq m |> Seq.map snd

    let cartesianProduct xs ys = Seq.collect (fun x -> Seq.map (fun y -> (x,y)) ys) xs

    let dictToStr (d:Dictionary<int,int>) =
        String.Join("\n", Seq.map (fun k -> k.ToString() + "->" + d.[k].ToString()) d.Keys)

    let mapToStr (m:Map<int,int>) =
        String.Join("\n", Seq.map (fun k -> k.ToString() + "->" + m.Item(k).ToString()) (keys m))

    let mapFromStr (s:string) =
        let parseLine (line:string) =
            let lhsAndRhs = line.Split([|"->"|], StringSplitOptions.None)
            (int lhsAndRhs.[0], int lhsAndRhs.[1])
        s.Split([|'\n'|]) |> Array.map parseLine |> Map.ofArray

    let array2DToStr (a:int [,]) =
        let rowStr i = Seq.fold (fun acc j -> acc + " " + a.[i,j].ToString())  "" [0..(Array2D.length2 a)-1]
        System.String.Join("\n", Seq.map rowStr [0..(Array2D.length1 a)-1])

    let parts (str:string) = Array.filter (fun (s:string) -> s.Length > 0) (str.Split([|' '|]))

    let rgen = new System.Random()
    let rand lb ub = rgen.Next(lb, ub+1)

    let rec topSort jobs preds =
        if Set.isEmpty jobs then []
        else
            let x = Seq.find (Set.isEmpty << (Set.intersect jobs) << preds) jobs
            x :: topSort (Set.remove x jobs) preds

    let slurp filename = File.ReadAllText(filename)
    let slurpLines filename = File.ReadAllLines(filename)
    let spit filename content = File.WriteAllText(filename, content)
