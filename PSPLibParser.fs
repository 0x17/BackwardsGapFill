namespace RCPSP

open System
open System.Text.RegularExpressions
open System.IO

open Utils

module PSPLibParser =
    let offsetStartingWith str lines = Seq.findIndex (fun (line:string) -> line.StartsWith(str)) lines

    let parseNumJobs filename = Int32.Parse(File.ReadAllLines(filename).[5].Split([|':'|]).[1].Trim())
    let parseNumResources filename = Int32.Parse((File.ReadAllLines(filename).[8].Split([|':'|]).[1].Split() |> Array.filter (fun s -> s <> "")).[0])
    let parseCapsOnly filename =
        let lines = File.ReadAllLines(filename)
        let resOffset = offsetStartingWith "RESOURCEAVAILABILITIES" lines
        lines.[resOffset+2].Split() |> Array.filter (fun s -> s <> "") |> Array.map (fun cstr -> Int32.Parse cstr)

    let serializeCommon title mapping filename =
        title+":\n"+
        mapToStr mapping + "\n"
        |> spitAppend filename

    let serializeKappa = serializeCommon "OVERCAPACITY COSTS"
    let serializeZMax = serializeCommon "MAX OVERCAPACITY"
    let serializeCosts = serializeCommon "COSTS"

    let serializeReachedLevels (rlevels:Set<int>) filename =
        "REACHED LEVELS:\n" +
        System.String.Join("\n", Set.toArray rlevels |> Array.map string)
        |> spitAppend filename

    let deserializeReachedLevels (levelsLines:string[]) = (Array.map int >> set) levelsLines

    let parseSuccs lines =
        let parseSuccLine line = 
            let p = Array.map int <| parts line
            (p.[0], Seq.ofArray p.[3..3+p.[2]-1])            
        Map.ofSeq <| Seq.map parseSuccLine lines

    let succsToPreds (succs:Map<int,int seq>) =
        let findPreds j = Seq.filter (fun i -> succs |> Map.find i |> Seq.exists (fun x -> x = j)) (keys succs)
        let arr = Array.zeroCreate (Seq.length (keys succs))
        for i in [0..arr.Length-1] do arr.[i] <- set (findPreds (i+1))
        arr

    let parseDurations lines = [| for line in lines -> int (parts line).[2] |]
    let parseDemands numRes lines = [| for line in lines -> Array.map int ((parts line).[3..3+numRes-1]) |]
    let parseCapacities numRes line = Seq.take numRes (parts line) |> Seq.map int |> Array.ofSeq
    let countRes capLine = Regex.Matches(capLine, "R \d").Count

    let parse filename =
        let lines = File.ReadAllLines(filename)

        let titles = ["PRECEDENCE RELATIONS"; "REQUESTS/DURATIONS"; "RESOURCEAVAILABILITIES"; "OVERCAPACITY COSTS";
                      "MAX OVERCAPACITY"; "COSTS"; "REACHED LEVELS"]
        let shortTitles = [|"prec"; "reqdur"; "cap"; "oc-costs"; "max-oc"; "costs"; "rlevels"|]
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
            let relevantLines = lines.[offsets(beginTitle)+1..offsets(endTitle)-1]
            let relevantLinesStr = System.String.Join("\n", relevantLines)
            let mapping = mapFromStr relevantLinesStr t
            mapToFunc mapping

        let kappaFunc = linesToFuncCommon "oc-costs" "max-oc" float
        let zmaxFunc = linesToFuncCommon "max-oc" "costs" int
        let costsFunc = linesToFuncCommon "costs" "rlevels" float

        let levelsLines = lines.[offsets("rlevels")+1..lines.Length-1]
        let reachedLevels = deserializeReachedLevels levelsLines

        ProjectStructure.Create(jobs, durations, demands,
                                costsFunc, capacities, predsFunc,
                                resources, topSort jobs predsFunc,
                                reachedLevels, kappaFunc, zmaxFunc)