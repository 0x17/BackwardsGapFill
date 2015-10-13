namespace RCPSP

open System.Text.RegularExpressions
open System.IO

open Utils

module PSPLibParser =
    let private offsetStartingWith str lines =
        try Seq.findIndex (fun (line:string) -> line.StartsWith(str)) lines
        with | :? System.Collections.Generic.KeyNotFoundException -> -1

    let private parseSuccs lines =
        let parseSuccLine line = 
            let p = Array.map int <| parts line
            (p.[0], p.[3..3+p.[2]-1])
        Map.ofArray <| Array.map parseSuccLine lines

    let private succsToPreds (succs:Map<int,int[]>) =
        let findPreds j = Seq.filter (fun i -> succs |> Map.find i |> Array.exists (fun x -> x = j)) (keys succs)
        let arr = Array.zeroCreate (Seq.length (keys succs))
        for i in 0..arr.Length-1 do arr.[i] <- set (findPreds (i+1))
        arr

    let private parseDurations lines = [| for line in lines -> int (parts line).[2] |]
    let private parseDemands numRes lines = [| for line in lines -> Array.map int ((parts line).[3..3+numRes-1]) |]
    let private parseCapacities numRes line = Array.take numRes (parts line) |> Array.map int
    let private countRes capLine = Regex.Matches(capLine, "R \d").Count
    
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

        let numJobs = Array.length durations
        let jobs = set [1..numJobs]
        let resources = set [1..numRes]

        let kappaFunc = (fun r -> 0.5)
        let zmaxFunc = (fun r -> int(0.5 * (float capacities.[r-1])))

        ProjectStructure.Create(jobs, durations, demands,
                                capacities, predsFunc,
                                resources, kappaFunc, zmaxFunc)

    let foreachProjInPath path func =
        System.IO.Directory.GetFiles(path, "*.DAT", System.IO.SearchOption.AllDirectories)
        |> Array.iter (fun f -> func f (parse f))