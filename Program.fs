namespace RCPSP

open ScheduleVisualisation
open Utils
open PSPLibParser

module Program =
    (*let exampleProject() =
        let predRelation = [|(1, []); (2, [1]); (3, [1]); (4, [2]); (5, [3]); (6, [1]); (7, [4; 5; 6])|]
                           |> Array.map (fun (j, predsj) -> (j, Set.ofSeq predsj))
                           |> arrayToFunc
        let jobset = Set.ofArray [|1..7|]
        ProjectStructure.Create(jobs=jobset,
                                durations=[|0; 1; 2; 2; 3; 1; 0|],
                                demands=[|[|0|]; [|2|]; [|2|]; [|1|]; [|1|]; [|1|]; [|0|]|],
                                costs=mapToFunc (RandomData.randomCosts 7),
                                capacities=[|2|],
                                preds=predRelation,
                                resources=Set.ofList [1],
                                topOrdering=topSort jobset predRelation,
                                reachedLevels=RandomData.randomReachedLevels(),
                                kappa=(fun r -> 1),
                                zmax=(fun r -> 0))*)

    (*let oldMain argv =
        let testFilename = "../../Testmodell.dat"
        let ps = exampleProject()
        PSPLibParser.serializeCosts (RandomData.randomCosts ps.Jobs.Count) testFilename
        PSPLibParser.serializeReachedLevels (RandomData.randomReachedLevels()) testFilename

        let sw = new System.Diagnostics.Stopwatch()
        sw.Start()
        let os = ps.ComputeOptimalSchedule()
        let os = ps.SerialScheduleGenerationScheme (fun r t -> 0) 
        sw.Stop()
        printf "Time elapsed: %O\n" sw.Elapsed

        printf "%s\n" (dictToStr os)

        //let grid = ps.ScheduleToGrid (fun r -> 2) os 1
        //printf "%s" (array2DToStr grid)
        //System.IO.File.WriteAllText("test.txt", (array2DToStr grid))
        System.Console.ReadKey() |> ignore*)

    let testVis() =
        let testFilename = "../../Projekte/Modellendogen0005.DAT"
        let ps = PSPLibParser.parse testFilename

        let (z1,sts1) = GamsSolver.solve ps
        let (z2,sts2) = ps.ComputeOptimalSchedule()

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1, z1); ("Heuristik", ps, sts2, z2)]

    let convertProjs() =
        BatchRunner.addCostsAndLevelsToProjs "../../Projekte"

    [<EntryPoint>]
    let main argv =
        //BatchRunner.stripAdditionalData "../../Projekte"
        //convertProjs()
        testVis()        
        0
    
