namespace RCPSP

open System.IO

module BatchRunner =
    let projExtension = "DAT"
    let projFilenames dirPath = Directory.GetFiles(dirPath, "*."+projExtension, SearchOption.AllDirectories)

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