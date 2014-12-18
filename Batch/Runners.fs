namespace RCPSP

open System.IO
open Serialization

module Runners =
    let testFilename = @"Projekte/j30/j3021_9.sm"
    let testProjectStructure () =  PSPLibParser.parse testFilename

    let private pspLibExt = ".sm"

    let batchComputePriorityRules path =
        let files = Directory.GetFiles(path, "*"+pspLibExt, SearchOption.AllDirectories)
        for f in files do
            let ps = PSPLibParser.parse f
            let orders = List.map (fun pr -> pr ps) PriorityRules.allRules
            for order in orders do
                spitAppend (f+".PRULES") (System.String.Join(" ", order)+"\n")

    let convertBatchSmToGdx force path =        
        let files = Directory.GetFiles(path, "*"+pspLibExt, SearchOption.AllDirectories)
        for f in files do
            let prefix = f.Replace(pspLibExt, "")
            if force || not(File.Exists(prefix)) then
                printf "Converting %s\n" f
                let ps = PSPLibParser.parse f
                GamsSolver.writeGdxFile ps prefix

    let forceConvertBatchSmToGdx = convertBatchSmToGdx true

    let batchExtractFromGdxInPath (col1f,col2f,col3f) path outFilename =
        let files = Directory.GetFiles(path, "*_tmin_results.gdx", SearchOption.AllDirectories)
        spit outFilename "filename;profit;tmin;tmax\n"
        for f in files do
            let col1 = col1f (f.Replace("_tmin_", "_"))
            let col2 = col2f f
            let col3 = col3f (f.Replace("_tmin_", "_tmax_"))
            spitAppend outFilename (f.Replace("_tmin_results.gdx", "").Replace(path+"\\", "") +
                ";"+string(col1).Replace(".",",")+
                ";"+string(col2).Replace(".",",")+
                ";"+string(col3).Replace(".",",")+"\n")

    let convertResultsGdxToCsv = batchExtractFromGdxInPath (GamsSolver.extractProfitFromResult, GamsSolver.extractMakespanFromResult, GamsSolver.extractMakespanFromResult)
    let extractSolveStatsFromGdx = batchExtractFromGdxInPath (GamsSolver.extractSolveStatFromResult, GamsSolver.extractSolveStatFromResult, GamsSolver.extractSolveStatFromResult)
 
    let copyRelevantInstances smPath (srcPath:string) (destPath:string) =
        let derivedFilenames (fn:string) =
            let tminFn = fn.Replace("tmax", "tmin")
            let resultFn = fn.Replace("_tmax", "")
            let smFn = fn.Replace("_tmax_results", "")
            (tminFn, resultFn, smFn)

        let minMaxMsEqual tminFn tmaxFn =
            (GamsSolver.extractMakespanFromResult tminFn) = (GamsSolver.extractMakespanFromResult tmaxFn)

        let copyRelatedFiles fn =
            let (tminFn, resultFn, smFn) = derivedFilenames fn
            [smFn; resultFn; tminFn; fn]
            |> List.iter (fun gfn -> File.Copy(gfn, gfn.Replace(srcPath, destPath)))
            let instPath = fn.Replace("_tmax_results.gdx", "")
            let srcSmFn = instPath.Replace(srcPath, smPath) + ".sm"
            File.Copy(srcSmFn, srcSmFn.Replace(smPath, destPath))

        if not(System.IO.Directory.Exists(destPath)) then
            Directory.CreateDirectory(destPath) |> ignore
            let files = Directory.GetFiles(srcPath, "*_tmax_results.gdx", SearchOption.AllDirectories)
            for fn in files do
                let (tminFn, resultFn, smFn) = derivedFilenames fn
                let solved = [ fn; tminFn; resultFn ] |> List.map GamsSolver.extractSolveStatFromResult |> List.forall ((=) 1.0)
                if solved && not(minMaxMsEqual tminFn fn) then copyRelatedFiles fn

