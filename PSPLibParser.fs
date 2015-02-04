namespace RCPSP

open System
open System.Text.RegularExpressions
open System.IO

open Utils
open Serialization
open TopologicalSorting

module PSPLibParser =
    let offsetStartingWith str lines =
        try Seq.findIndex (fun (line:string) -> line.StartsWith(str)) lines
        with | :? System.Collections.Generic.KeyNotFoundException -> -1

    let parseNumJobs filename = Int32.Parse(File.ReadAllLines(filename).[5].Split([|':'|]).[1].Trim())
    let parseNumResources filename = Int32.Parse((File.ReadAllLines(filename).[8].Split([|':'|]).[1].Split() |> Array.filter (fun s -> s <> "")).[0])
    let parseCapsOnly filename =
        let lines = File.ReadAllLines(filename)
        let resOffset = offsetStartingWith "RESOURCEAVAILABILITIES" lines
        lines.[resOffset+2].Split() |> Array.filter (fun s -> s <> "") |> Array.map (fun cstr -> Int32.Parse cstr)

    let serializeCommon title mapping filename =
        title+":\n"+
        mapToStr mapping + "\n"
        |> replace ',' '.'
        |> spitAppend filename

    let serializeKappa : Map<int,float> -> string -> unit =
        serializeCommon "OVERCAPACITY COSTS"
    let serializeZMax : Map<int,int> -> string -> unit =
        serializeCommon "MAX OVERCAPACITY"

    let parseSuccs lines =
        let parseSuccLine line = 
            let p = Array.map int <| parts line
            (p.[0], Seq.ofArray p.[3..3+p.[2]-1])            
        Map.ofSeq <| Seq.map parseSuccLine lines

    let succsToPreds (succs:Map<int,int seq>) =
        let findPreds j = Seq.filter (fun i -> succs |> Map.find i |> Seq.exists (fun x -> x = j)) (keys succs)
        let arr = Array.zeroCreate (Seq.length (keys succs))
        for i in 0..arr.Length-1 do arr.[i] <- set (findPreds (i+1))
        arr

    let parseDurations lines = [| for line in lines -> int (parts line).[2] |]
    let parseDemands numRes lines = [| for line in lines -> Array.map int ((parts line).[3..3+numRes-1]) |]
    let parseCapacities numRes line = Seq.take numRes (parts line) |> Seq.map int |> Array.ofSeq
    let countRes capLine = Regex.Matches(capLine, "R \d").Count
    
    let parse filename =
        let lines = File.ReadAllLines(filename)

        let titles = ["PRECEDENCE RELATIONS"; "REQUESTS/DURATIONS"; "RESOURCEAVAILABILITIES"; "OVERCAPACITY COSTS"; "MAX OVERCAPACITY"]
        let shortTitles = [|"prec"; "reqdur"; "cap"; "oc-costs"; "max-oc"|]
        let offsets = titles |> List.mapi (fun i title -> (shortTitles.[i], offsetStartingWith (title+":") lines)) |> Map.ofList |> mapToFunc

        let numRes = countRes lines.[offsets("cap")+1]

        let preds = parseSuccs lines.[offsets("prec")+2..offsets("reqdur")-2] |> succsToPreds
        let predsFunc j = Array.get preds (j-1)
                
        let reqDurLines = lines.[offsets("reqdur")+3..offsets("cap")-2]
        let durations = parseDurations reqDurLines
        let demands = parseDemands numRes reqDurLines
        
        let capacities = parseCapacities numRes lines.[offsets("cap")+2]

        let numJobs = Seq.length durations
        let jobs = set [1..numJobs]
        let resources = set [1..numRes]       

        let linesToFuncCommon beginTitle endTitle t =
            if offsets beginTitle = -1 then (fun x -> t "0")
            else
                let last = (if endTitle = "EOF" then lines.Length else offsets endTitle) - 1
                let relevantLines = lines.[(offsets beginTitle)+1..last]
                let relevantLinesStr = System.String.Join("\n", relevantLines)
                let mapping = mapFromStr relevantLinesStr t
                mapToFunc mapping

        let kappaFunc = (fun r -> 0.5)
        let zmaxFunc = (fun r -> int(0.5 * (float capacities.[r-1])))

        let ordering = topSort jobs predsFunc
        ProjectStructure.Create(jobs, durations, demands,
                                capacities, predsFunc,
                                resources, kappaFunc, zmaxFunc)

    let foreachProjInPath path func =
        let projFiles = System.IO.Directory.GetFiles(path, "*.DAT", System.IO.SearchOption.AllDirectories)
        for f in projFiles do
            let ps = parse f
            func f ps