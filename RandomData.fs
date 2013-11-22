namespace RCPSP

open Utils
open System.IO
open System

module RandomData =
    let randomCosts numJobs =
        Array.map (fun j -> (j,rand 1 10)) [|2..numJobs-1|] |> Map.ofArray

    let randomReachedLevels() = Set.ofArray [|(rand 1 5)..5|]