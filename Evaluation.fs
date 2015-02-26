namespace RCPSP

open System.IO

module Evaluation =
    let profitsToRanking inFilename =
        let lines = File.ReadAllLines(inFilename)

        let indexOf p = Seq.findIndex ((=) p)

        let skipOdds vals =
            vals
            |> Seq.mapi (fun i e -> (i,e))
            |> Seq.filter (fun pair -> (fst pair) % 2 = 0)
            |> Seq.map snd

        let profitsLineToRanks (line:string) =
            let columns = line.Split([|';'|])
            let profits = columns |> Seq.skip 1 |> skipOdds |> Seq.map System.Double.Parse
            let descProfits = profits |> Seq.distinct |> Seq.sortBy (fun p -> -p)
            let ranks = profits |> Seq.map (fun p -> (indexOf p descProfits) + 1) |> Seq.map string
            Seq.append [columns.[0]] ranks

        let rankings = Seq.map profitsLineToRanks (Seq.skip 1 lines)

        let stringsToCsvLine cols =
            let res = cols |> Seq.fold (fun acc col -> acc + col + ";") ""
            res.Remove(res.Length-1)

        let outStr = rankings |> Seq.fold (fun acc ranking -> acc + "\n" + (stringsToCsvLine ranking)) (Seq.head lines)

        File.WriteAllText(inFilename.Replace(".txt", "") + "rankings.txt", outStr)