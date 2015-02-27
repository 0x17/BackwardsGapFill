namespace RCPSP

open System
open System.IO

module Evaluation =
    module Helpers =
        let drop n (s:string) = s.Remove(s.Length-n)
        let intersperse ch strs =        
            strs
            |> Seq.fold (fun acc s -> acc + s + ch) "" 
            |> drop (String.length ch)
        let csvSplitters = [|';'|]
        let toCsv = intersperse ";"

    open Helpers

    let profitsToRanking resultsFn limitIx =
        let lines = File.ReadAllLines(resultsFn)
        let indexOf p = Seq.findIndex ((=) p)
        
        let profitsLineToRanks (line:string) =
            let columns = line.Split(csvSplitters)
            let profits = columns |> Seq.skip 1 |> Seq.map (fun col -> col.Split([|':'|]) |> Seq.nth limitIx |> double)
            let descProfits = profits |> Seq.distinct |> Seq.sortBy (fun p -> -p)
            let ranks = profits |> Seq.map (fun p -> (indexOf p descProfits) + 1) |> Seq.map string
            Seq.append [columns.[0]] ranks

        let rankings = Seq.map profitsLineToRanks (Seq.skip 1 lines)

        let outStr = rankings |> Seq.fold (fun acc ranking -> acc + "\n" + (toCsv ranking)) (Seq.head lines)

        File.WriteAllText(resultsFn.Replace(".txt", "") + "rankings.txt", outStr)

    let evaluateResultsToTex resultsFn limitIx (optsFn:Option<string>) =
        let lines = File.ReadAllLines(resultsFn)

        let headCols = (Seq.head lines).Split(csvSplitters)
        let heurNames = Seq.skip 1 headCols |> Seq.map (fun hname -> "$"+hname+"$")
        let numHeurs = Seq.length heurNames

        let selectProfit (s:string) = s.Split([|':'|]) |> Seq.nth limitIx |> Double.Parse

        let profits heurIx =            
            lines
            |> Seq.skip 1
            |> Seq.map (fun line -> line.Split(csvSplitters) |> Seq.nth (heurIx + 1) |> selectProfit)

        let bestProfits =
            if optsFn.IsSome then
                let projToProfit = File.ReadAllLines(optsFn.Value)
                                   |> Seq.skip 1
                                   |> Seq.map (fun line -> (line.Split(csvSplitters).[0], line.Split(csvSplitters).[1]))
                                   |> Map.ofSeq
                lines
                |> Seq.skip 1
                |> Seq.map (fun line -> projToProfit |> Map.find (line.Split(csvSplitters).[0]) |> Double.Parse)
            else
                lines
                |> Seq.skip 1
                |> Seq.map (fun line -> line.Split(csvSplitters) |> Seq.skip 1 |> Seq.map selectProfit |> Seq.max)

        let gap profit optProfit =
            if optProfit = 0.0 then 0.0
            else (optProfit - profit) / optProfit
        let gaps heurIx = Seq.map2 gap (profits heurIx) bestProfits

        let avgDev = Seq.average << gaps
        let maxDev = Seq.max << gaps
        let varCoeffDev heurIx =
            let expVal = avgDev heurIx
            let stddev = gaps heurIx |> Seq.averageBy (fun g -> (g - expVal) ** 2.0)
            stddev / expVal

        let numBest heurIx =
            Seq.zip (profits heurIx) bestProfits
            |> Seq.fold (fun acc pair -> if (fst pair) = (snd pair) then acc + 1.0 else acc) 0.0

        let percBest heurIx = numBest heurIx / (float(lines.Length)-1.0)

        let rounding (n:float) = Math.Round(n, 2)
        let inPercent n = string(rounding (n * 100.0)) + "\\%"
        let characteristics = Map.ofList [("$\\varnothing$ deviation", inPercent << avgDev);
                                          ("max. deviation", inPercent << maxDev);
                                          ("varcoeff(deviation)", string << rounding << varCoeffDev); 
                                          ("perc. best known solution", inPercent << percBest); 
                                          ("\\# best", string << int << numBest)]

        let colFormat = String.replicate headCols.Length "c"                
        let headRow = "representation & " + intersperse " & " heurNames + "\\\\[3pt]"
                
        let applyToAllHeurs fn = [0..numHeurs-1] |> Seq.map fn
        let body = Map.fold (fun acc ch fn -> acc + "\n\\hline\n" + ch + "&" + intersperse "&" (applyToAllHeurs fn) + "\\\\") "" characteristics

        let contents = "\\begin{tabular}{" + colFormat + "}\n\\hline\n" + headRow + body + "\\hline\n\\end{tabular}"
        File.WriteAllText("results.tex", File.ReadAllText("skeleton.tex").Replace("%%CONTENTS%%", contents));