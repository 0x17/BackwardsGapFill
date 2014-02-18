namespace RCPSP

open Utils

module FastSSGS =
    let solvePartial (ps:ProjectStructure) (z: int -> int -> int) (partialSts:Map<int,int>) order =
        let mutable infeasible = true
        let mutable enoughCapacity = true

        let mutable t = 0        
        let mutable tau = 0
        
        let mutable res = 0

        let numJobs = ps.Jobs.Count
        let numRes = Seq.length ps.Resources

        let sts = Array.create numJobs 0
        let fts = Array.create numJobs 0

        let resRemaining = Array2D.init numRes ps.TimeHorizon.Length (fun r t -> ps.Capacities (r+1) + z (r+1) (t+1))

        for j in keys partialSts do
            let stj = partialSts.Item(j)
            sts.[j-1] <- stj
            fts.[j-1] <- stj + ps.Durations j
            for tau in stj..fts.[j-1]-1 do
                for r in ps.Resources do
                    resRemaining.[r-1,tau] <- resRemaining.[r-1,tau] - ps.Demands j r

        for j in order do
            // Periode, wenn spätester Vorgänger fertig
            t <- 0
            for i in ps.Preds j do
                if fts.[i-1] > t then
                    t <- fts.[i-1]

            // Inkrementiere, solange Ressourcenunzulässig
            infeasible <- true            
            while infeasible do
                tau <- t
                enoughCapacity <- true

                while enoughCapacity && tau < t + ps.Durations j do
                    res <- 1

                    while enoughCapacity && res <= numRes do
                        if resRemaining.[res-1, tau] < ps.Demands j res then
                            enoughCapacity <- false
                        res <- res + 1

                    tau <- tau + 1

                if enoughCapacity then infeasible <- false
                else t <- t + 1

            // Plane zu t ein
            sts.[j-1] <- t
            fts.[j-1] <- t + ps.Durations j
            for tau in t..fts.[j-1]-1 do
                for r in ps.Resources do
                    resRemaining.[r-1,tau] <- resRemaining.[r-1,tau] - ps.Demands j r

        let schedule = [0..numJobs-1] |> List.map (fun i -> (i+1, sts.[i])) |> Map.ofList
        (schedule, resRemaining)

    let solve (ps:ProjectStructure) (z: int -> int -> int) order =
        let mutable infeasible = true
        let mutable enoughCapacity = true

        let mutable t = 0        
        let mutable tau = 0
        
        let mutable res = 0

        let numJobs = ps.Jobs.Count
        let numRes = Seq.length ps.Resources

        let sts = Array.create numJobs 0
        let fts = Array.create numJobs 0

        let resRemaining = Array2D.init numRes ps.TimeHorizon.Length (fun r t -> ps.Capacities (r+1) + z (r+1) (t+1))

        let firstJob = Seq.head order

        sts.[firstJob-1] <- 0
        fts.[firstJob-1] <- 0

        for j in (Seq.skip 1 order) do
            // Periode, wenn spätester Vorgänger fertig
            t <- 0
            for i in ps.Preds j do
                if fts.[i-1] > t then
                    t <- fts.[i-1]

            // Inkrementiere, solange Ressourcenunzulässig
            infeasible <- true            
            while infeasible do
                tau <- t
                enoughCapacity <- true

                while enoughCapacity && tau < t + ps.Durations j do
                    res <- 1

                    while enoughCapacity && res <= numRes do
                        if resRemaining.[res-1, tau] < ps.Demands j res then
                            enoughCapacity <- false
                        res <- res + 1

                    tau <- tau + 1

                if enoughCapacity then infeasible <- false
                else t <- t + 1

            // Plane zu t ein
            sts.[j-1] <- t
            fts.[j-1] <- t + ps.Durations j
            for t in t..fts.[j-1]-1 do
                for r in ps.Resources do
                    resRemaining.[r-1,t] <- resRemaining.[r-1,t] - ps.Demands j r

        [0..numJobs-1] |> List.map (fun i -> (i+1, sts.[i])) |> Map.ofList