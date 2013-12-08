namespace RCPSP

open Utils

module ProjectGenerator =
    let generateNetwork numJobs numStartJobsRange numFinishJobsRange =
        let jobs = [1..numJobs]

        let numStartJobs = randomlyChoose numStartJobsRange
        let numFinishJobs = randomlyChoose numFinishJobsRange

        let initialArcs =
            [for j in [2..numStartJobs] do yield (1,j)] @
            [for j in [numJobs-numFinishJobs+1..numJobs-1] do yield (j,numJobs)]

        let arcsWithRandPred =
            initialArcs @ [for j in [2..numJobs] do yield (randomlyChoose (allBut 1 (numJobs-1) j), j)]

        let arcsWithRandPredAndSucc =
            let hasNoSucc j =
                not(arcsWithRandPred |> Seq.exists (fun pair -> fst pair = j))
            let jobsWithoutSucc = jobs |> Seq.filter hasNoSucc
            arcsWithRandPred @ [for j in jobsWithoutSucc do yield (j, randomlyChoose (allBut 1 (numJobs-1) j))]
        ()
            