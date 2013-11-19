namespace RCPSP

open Utils
open System.IO
open System

module RandomData =
    let randomCosts numJobs =
        Array.map (fun j -> (j,rand 1 10)) [|2..numJobs-1|] |> Map.ofArray

    let randomReachedLevels() = Set.ofArray [|(rand 1 5)..5|]

    let serializeCosts costs filename = mapToStr costs |> spit filename

    let serializeReachedLevels (rlevels:Set<int>) filename =
        System.String.Join("\n", Set.toArray rlevels |> Array.map string) |> spit filename 

    let deserializeCosts = slurp >> mapFromStr

    let deserializeReachedLevels = slurpLines >> Array.map int >> Set.ofSeq