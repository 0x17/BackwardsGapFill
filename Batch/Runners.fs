namespace RCPSP

open System.IO
open Serialization

module Runners =
    let testFilename = @"Projekte/j30/j3021_9.sm"
    let testProjectStructure () =  PSPLibParser.parse testFilename
    let pspLibExt = ".sm"

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

    let precomputeOptimalSchedules path =
        let fst3 (a,_,_) = a
        PSPLibParser.foreachProjInPath path (fun f ps ->
            let outfn = f+".OPTSCHED"
            if not (System.IO.File.Exists(outfn)) then
                spitMap outfn (fst3 (GamsSolver.solve ps)))

    let projFilenames dirPath = Directory.GetFiles(dirPath, "*.sm", SearchOption.AllDirectories)

    let addAdditionalDataToProjs dirPath =
        for fn in projFilenames dirPath do
            let nres = PSPLibParser.parseNumResources fn
            let res = [1..nres]

            let kappaVal = 0.5
            let kappa = res |> Seq.map (fun r -> (r, kappaVal)) |> Map.ofSeq
            PSPLibParser.serializeKappa kappa fn

            let zmaxFactor = 0.5 // 0.25
            let caps = PSPLibParser.parseCapsOnly fn
            let zmax = res |> Seq.map (fun r -> (r,int(float caps.[r-1] * zmaxFactor))) |> Map.ofSeq
            PSPLibParser.serializeZMax zmax fn

    let stripAdditionalData dirPath =
        for fn in projFilenames dirPath do
            let origLines = File.ReadAllLines(fn) |> Seq.takeWhile (fun line -> not(line.StartsWith("OVERCAPACITY")))
            File.WriteAllLines(fn, origLines)

    let copyRelevantInstances smPath srcPath destPath =
        if not(System.IO.Directory.Exists(destPath)) then
            Directory.CreateDirectory(destPath) |> ignore
            let files = Directory.GetFiles(srcPath, "*_tmax_results.gdx", SearchOption.AllDirectories)
            for fn in files do
                let tminFn = fn.Replace("tmax", "tmin")
                let resultFn = fn.Replace("_tmax", "")
                let smFn = fn.Replace("_tmax_results", "")

                let solved =
                    [ fn; tminFn; resultFn ]
                    |> List.map GamsSolver.extractSolveStatFromResult
                    |> List.forall ((=) 1.0)

                if solved then
                    let tminValue = GamsSolver.extractMakespanFromResult tminFn
                    let tmaxValue = GamsSolver.extractMakespanFromResult fn
                    if tminValue <> tmaxValue then 
                        [smFn; resultFn; tminFn; fn]
                        |> List.iter (fun gfn -> File.Copy(gfn, gfn.Replace(srcPath, destPath)))
                        let instPath = fn.Replace("_tmax_results.gdx", "")
                        let srcSmFn = instPath.Replace(srcPath, smPath) + ".sm"
                        File.Copy(srcSmFn, srcSmFn.Replace(smPath, destPath))

