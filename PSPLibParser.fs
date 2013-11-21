namespace RCPSP

open Utils
open System.Text.RegularExpressions

module PSPLibParser =
    let offsetStartingWith str lines = Seq.findIndex (fun (line:string) -> line.StartsWith(str)) lines

    let parseSuccs lines =
        let parseSuccLine line = 
            let p = Array.map int <| parts line
            (p.[0], Seq.ofArray p.[3..3+p.[2]-1])            
        Map.ofSeq <| Seq.map parseSuccLine lines

    let succsToPreds (succs:Map<int,int seq>) =
        let findPreds j = Seq.filter (fun i -> succs |> Map.find i |> Seq.exists (fun x -> x = j)) (keys succs)
        let arr = Array.zeroCreate (Seq.length (keys succs))
        for i in [0..arr.Length-1] do arr.[i] <- Set.ofSeq (findPreds (i+1))
        arr

    let parseDurations lines = [| for line in lines -> int (parts line).[2] |]

    let parseDemands numRes lines = [| for line in lines -> Array.map int ((parts line).[3..3+numRes-1]) |]

    let parseCapacities numRes line = Seq.take numRes (parts line) |> Seq.map int |> Array.ofSeq

    let countRes capLine = Regex.Matches(capLine, "R \d").Count

    let parse filename =
        let lines = System.IO.File.ReadAllLines(filename)

        let precOffset = offsetStartingWith "PRECEDENCE RELATIONS:" lines
        let reqDurOffset = offsetStartingWith "REQUESTS/DURATIONS:" lines
        let capOffset = offsetStartingWith "RESOURCEAVAILABILITIES:" lines

        let numRes = countRes lines.[capOffset+1]

        let preds = parseSuccs lines.[precOffset+2..reqDurOffset-2] |> succsToPreds
        let predsFunc j = Array.get preds (j-1)
                
        let reqDurLines = lines.[reqDurOffset+3..capOffset-2]
        let durations = parseDurations reqDurLines
        let demands = parseDemands numRes reqDurLines
        
        let capacities = parseCapacities numRes lines.[capOffset+2]

        let numJobs = Seq.length durations
        let jobs = Set.ofSeq [1..numJobs]
        let resources = Set.ofSeq [1..numRes]

        let costs = RandomData.deserializeCosts (filename+".costs.txt")
        let costsFunc = mapToFunc costs
        let reachedLevels = RandomData.deserializeReachedLevels (filename+".rlevels.txt")

        let kappa = (fun r -> 1)
        let zmax = (fun r -> 0)

        ProjectStructure.Create(jobs, durations, demands,
                                costsFunc, capacities, predsFunc,
                                resources, topSort jobs predsFunc,
                                reachedLevels, kappa, zmax)