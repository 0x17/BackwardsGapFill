﻿namespace RCPSP

open Utils
open System.Collections.Generic

module GeneticAlgorithm =
    let randomPairs (elems: 'I[]) =
        let pivot = elems.Length / 2
        let elite = elems |> Array.take pivot
        let mutRest = new List<'I>(elems |> Array.skip pivot)
        Array.map (fun e -> (e, removeRandomElement mutRest)) elite
    
    let randomPairApply (elems:'I[]) (pairwiseFunc: ('I*'I) -> 'I) =
        let pairs = randomPairs elems
        let daughters = pairs |> Array.map pairwiseFunc
        let sons = pairs |> Array.map (pairwiseFunc << swap)
        Array.concat [daughters;sons]

    let selectBest (fitness: 'I -> float) (parents: 'I[]) (children: 'I[]) =
        let sortedDescFitness = Array.concat [parents;children] |> Array.sortBy (((*) -1.0) << fitness)
        Array.sub sortedDescFitness 0 parents.Length

    let solve (init: ProjectStructure -> 'I[]) (crossover: ('I*'I) -> 'I) (mutate: 'I -> 'I) (fitness: 'I -> float) numGens (ps: ProjectStructure) =
        let rec iterate pop gen =
            if gen = 0 then pop
            else iterate (randomPairApply pop crossover |> Array.map mutate |> selectBest fitness pop) (gen-1)
        iterate (init ps) numGens |> Seq.head