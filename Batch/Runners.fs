namespace RCPSP

open System.IO
open Serialization

module Runners =
    let testFilename = @"Projekte/j30/j3021_9.sm"
    let testProjectStructure () =  PSPLibParser.parse testFilename

    let convertBatchSmToGdx force path =
        let pspLibExt = ".sm"
        let files = Directory.GetFiles(path, "*"+pspLibExt, SearchOption.AllDirectories)
        for f in files do
            let prefix = f.Replace(pspLibExt, "")
            if force || not(File.Exists(prefix)) then
                printf "Converting %s\n" f
                let ps = PSPLibParser.parse f
                GamsSolver.writeGdxFile ps prefix

    let forceConvertBatchSmToGdx = convertBatchSmToGdx true

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