namespace RCPSP

open Utils

module FastSSGS =
    let solveCommon (ps:ProjectStructure) (sts:int[]) (fts:int[]) (resRemaining:int[,]) order =
        let numJobs = ps.Jobs.Count
        let numRes = Seq.length ps.Resources

        let mutable infeasible = true
        let mutable enoughCapacity = true
        let mutable t = 0
        let mutable res = 0
        let mutable tau = 0        

        for j in order do
            // Periode, wenn spätester Vorgänger fertig
            t <- 0
            for i in ps.Preds j do
                if fts.[i-1] > t then
                    t <- fts.[i-1]

            // Inkrementiere, solange Ressourcenunzulässig
            infeasible <- true            
            while infeasible do
                enoughCapacity <- true

                // Für jede Ressource r...
                let durj = ps.Durations j
                res <- 1
                while enoughCapacity && res <= numRes do
                    let kjr = ps.Demands j res                    
                    if kjr > 0 then
                        // Für jede Periode t prüfe, ob genug Restkapazität von r
                        tau <- t
                        while enoughCapacity && tau < t + durj do
                            if resRemaining.[res-1, tau] < kjr then
                                enoughCapacity <- false
                            tau <- tau + 1
                    res <- res + 1                    

                if enoughCapacity then infeasible <- false
                else t <- t + 1

            // Plane zu t ein
            sts.[j-1] <- t
            fts.[j-1] <- t + ps.Durations j
            
            // Ziehe dafür entsprechende Nachfragewerte bei verbleibenden Kapazitäten ab
            tau <- t
            while tau < fts.[j-1] do
                res <- 0

                while res < numRes do
                    resRemaining.[res,tau] <- resRemaining.[res,tau] - ps.Demands j (res+1)
                    res <- res + 1

                tau <- tau + 1

        // Convert sts-Array to F#-map
        let schedule = [0..numJobs-1] |> List.map (fun i -> (i+1, sts.[i])) |> Map.ofList
        (schedule, resRemaining)

    let solvePartial (ps:ProjectStructure) (z: int -> int -> int) (partialSts:Map<int,int>) order =
        let mutable tau = 0        
        let mutable res = 0
        let mutable i = 0

        let numJobs = ps.Jobs.Count
        let numRes = Seq.length ps.Resources

        let sts = Array.create numJobs 0
        let fts = Array.create numJobs 0

        let resRemaining = Array2D.init numRes ps.TimeHorizon.Length (fun r t -> ps.Capacities (r+1) + z (r+1) (t+1))

        // Synchronisiere sts, fts und resRemaining mit gegebenem partiellen Schedule
        let partialArr = Map.toArray partialSts

        while i < partialArr.Length do
            let (j, stj) = partialArr.[i]

            sts.[j-1] <- stj
            fts.[j-1] <- stj + ps.Durations j

            tau <- stj
            while tau < fts.[j-1] do
                res <- 0
                while res < numRes do
                    resRemaining.[res,tau] <- resRemaining.[res,tau] - ps.Demands j (res+1)
                    res <- res + 1
                tau <- tau + 1

            i <- i+1

        solveCommon ps sts fts resRemaining order

    let solve (ps:ProjectStructure) (z: int -> int -> int) order =
        let numJobs = ps.Jobs.Count
        let numRes = Seq.length ps.Resources

        let sts = Array.create numJobs 0
        let fts = Array.create numJobs 0

        let resRemaining = Array2D.init numRes ps.TimeHorizon.Length (fun r t -> ps.Capacities (r+1) + z (r+1) (t+1))

        let firstJob = Seq.head order

        sts.[firstJob-1] <- 0
        fts.[firstJob-1] <- 0

        solveCommon ps sts fts resRemaining (Seq.skip 1 order)
