namespace RCPSP

open System.IO
open System

open Utils

module Serialization =
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

    let slurp = File.ReadAllText
    let slurpLines = File.ReadAllLines
    let spit filename content = File.WriteAllText (filename, content)
    let spitAppend filename content = File.AppendAllText (filename, content)

    let spitMap filename m = spit filename << mapToStr
    let slurpMap filename = mapFromStr (slurp filename) int

