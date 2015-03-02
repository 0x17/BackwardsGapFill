namespace RCPSP

open System
open System.IO
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

module Evaluation =
    module Helpers =
        open OfficeOpenXml

        let drop n (s:string) = s.Remove(s.Length-n)
        let intersperse ch strs =        
            strs
            |> Seq.fold (fun acc s -> acc + s + ch) "" 
            |> drop (String.length ch)

        let csvSplitter = [|';'|]
        let colonSplitter = [|':'|]
        let newlineSplitter = [|'\n'|]

        let toCsv = intersperse ";"

        let deleteAuxFiles outFn =
            let outBase = Path.GetFileNameWithoutExtension(outFn)
            [|".log"; ".aux"|] |> Seq.iter (fun ext -> File.Delete(outBase + ext)) 

        let private fillWorkSheetWithCsv (workSheet: ExcelWorksheet) (csvData:string) =
            let toActualType s = if Regex.IsMatch(s, "^\d+$") then (Int32.Parse s) :> obj else s :> obj
            csvData.Split(newlineSplitter)
            |> Seq.iteri (fun rowIx line -> line.Split(csvSplitter) |> Seq.iteri (fun colIx cell-> workSheet.Cells.[rowIx+1, colIx+1].Value <- toActualType cell))

        let csvToExcel csvData outFn =
            if File.Exists(outFn) then File.Delete(outFn)
            use pkg = new ExcelPackage(FileInfo(outFn))
            let workSheet = pkg.Workbook.Worksheets.Add("Results")
            fillWorkSheetWithCsv workSheet csvData            
            pkg.Save()

        let csvToExcelSheets (captions: string seq) (csvDatas: string seq) outFn =
            if File.Exists(outFn) then File.Delete(outFn)
            use pkg = new ExcelPackage(FileInfo(outFn))

            let sheetFromCsv caption csvData =
                let ws = pkg.Workbook.Worksheets.Add(caption)
                fillWorkSheetWithCsv ws csvData

            Seq.iter2 sheetFromCsv captions csvDatas
            pkg.Save()

        let texSymToUtf8 (s:string) =
            let mapping = [|("\\tau", "τ"); ("\\lambda", "λ"); ("\\beta", "β")|]
            Array.fold (fun (acc:string) ((src, dest):string*string) -> acc.Replace(src, dest)) s mapping

    open Helpers

    let private limits = ["0.01"; "0.1"; "0.5"; "1"; "2"; "5"; "10"; "30"]
    let private countLimits (line: String) = line.Split(csvSplitter).[1].Split(colonSplitter).Length

    let profitsToRanking resultsFn =
        let lines = File.ReadAllLines(resultsFn)
        let indexOf p = Seq.findIndex ((=) p)            

        let rankings limitIx =
            let profitsLineToRanks (line:string) =
                let columns = line.Split(csvSplitter)
                let profits = columns |> Seq.skip 1 |> Seq.map (fun col -> col.Split(colonSplitter) |> Seq.nth limitIx |> Double.Parse)
                let descProfits = profits |> Seq.distinct |> Seq.sortBy (fun p -> -p)
                let ranks = profits |> Seq.map (fun p -> (indexOf p descProfits) + 1 |> string)
                Seq.append [columns.[0]] ranks
            Seq.map profitsLineToRanks (Seq.skip 1 lines)

        let csvRankings limitIx = rankings limitIx |> Seq.fold (fun acc ranking -> acc + "\n" + (toCsv ranking)) (Seq.head lines |> texSymToUtf8)

        let numLimits = countLimits lines.[1]
        let captions = limits |> Seq.take numLimits |> Seq.map (fun l -> l + "s")
        let csvDatas = Seq.init numLimits csvRankings
        csvToExcelSheets captions csvDatas (resultsFn.Replace(".txt", "") + "rankings.xlsx")

    let multipleProfitsToRankings: (string seq) -> unit = Seq.iter profitsToRanking

    let evaluateResultsToTexTable resultsFn limitIx (optsFn:Option<string>) =
        let lines = File.ReadAllLines(resultsFn)
        let contentLines = Seq.skip 1 lines

        let headCols = (Seq.head lines).Split(csvSplitter)
        let heurNames = Seq.skip 1 headCols |> Seq.map (fun hname -> "$"+hname+"$")
        let numHeurs = Seq.length heurNames

        let selectProfit (s:string) = s.Split(colonSplitter) |> Seq.nth limitIx |> Double.Parse

        let profits heurIx = contentLines |> Seq.map (fun line -> line.Split(csvSplitter) |> Seq.nth (heurIx + 1) |> selectProfit)

        let projToProfit optsFilename =
            File.ReadAllLines(optsFilename)
            |> Seq.skip 1
            |> Seq.map (fun line -> (line.Split(csvSplitter).[0], line.Split(csvSplitter).[1]))
            |> Map.ofSeq

        let bestProfits =
            match optsFn with
            | Some(optsFn) -> contentLines |> Seq.map (fun line -> projToProfit optsFn |> Map.find (line.Split(csvSplitter).[0]) |> Double.Parse)
            | None -> contentLines |> Seq.map (fun line -> line.Split(csvSplitter) |> Seq.skip 1 |> Seq.map selectProfit |> Seq.max)

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
        let characteristics = [("$\\varnothing$ deviation", inPercent << avgDev);
                               ("max. deviation", inPercent << maxDev);
                               ("varcoeff(deviation)", string << rounding << varCoeffDev); 
                               ("\\% " + (if optsFn.IsSome then "optimal" else "best known solution"), inPercent << percBest); 
                               ("\\# best", string << int << numBest)]

        let colFormat = String.replicate headCols.Length "c"                
        let headRow = "representation & " + intersperse " & " heurNames + "\\\\[3pt]"
                
        let applyToAllHeurs fn = [0..numHeurs-1] |> PSeq.map fn
        let body = List.fold (fun acc (ch, fn) -> acc + "\n\\hline\n" + ch + "&" + intersperse "&" (applyToAllHeurs fn) + "\\\\") "" characteristics

        "\\begin{tabular}{" + colFormat + "}\n\\hline\n" + headRow + body + "\\hline\n\\end{tabular}\n"

    let private insertIntoSkeletonBuildAndCleanup contents outFn =
        File.WriteAllText(outFn, File.ReadAllText("skeleton.tex").Replace("%%CONTENTS%%", contents));
        Diagnostics.Process.Start("pdflatex", outFn).WaitForExit ()
        deleteAuxFiles outFn

    let evaluateResultsToTexFile resultsFn limitIx (optsFn:Option<string>) =
        let tableTex = evaluateResultsToTexTable resultsFn limitIx optsFn
        let outFn = Path.GetFileNameWithoutExtension(resultsFn) + "Aggregated.tex"
        insertIntoSkeletonBuildAndCleanup tableTex outFn

    let evaluateMultipleResultsToTexFile resultsCaptionsOpts =
        let pbreak ctr = if ctr > 0 && (ctr + 1) % 3 = 0 then "\n\\newpage\n" else ""
        let tablesForEachLimit acc (fn, caption, optsfn) =
            let nlimits = countLimits (File.ReadLines(fn) |> Seq.skip 1 |> Seq.head)
            let tableFrame (s,ctr) limitIx =
                (s + caption + " (limit=" + (Seq.nth limitIx limits) + "s)\\\\" + (evaluateResultsToTexTable fn limitIx optsfn) + "\\\\[15pt]" + pbreak ctr, ctr+1)
            [|0..nlimits-1 |] |> Array.fold tableFrame acc
        let tablesTex = resultsCaptionsOpts |> Seq.fold tablesForEachLimit ("", 0) |> fst
        insertIntoSkeletonBuildAndCleanup tablesTex "combinedResults.tex"
