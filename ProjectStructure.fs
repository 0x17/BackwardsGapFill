namespace RCPSP

open Microsoft.FSharp.Collections

open Utils
open TopologicalSorting

type ProjectStructure(jobs, durations, demands, preds: int -> Set<int>, resources, capacities, kappa, zmax) =
    let firstJob = Set.minElement jobs
    let lastJob = Set.maxElement jobs
    let actualJobs = Set.difference jobs (set [firstJob; lastJob])

    let T = Seq.sumBy durations jobs
    let horizon = [1..T]

    let transPreds = transitiveHull preds
    let succs = memoize (fun i -> jobs |> Seq.filter (fun j -> (preds j).Contains i) |> Set.ofSeq)
    let transSuccs = transitiveHull succs

    let canonicalOrdering = seq { 1 .. Set.count jobs}
    let topOrdering = topSort jobs preds
    let revTopOrdering = topSort jobs succs

    let sttoft j stj = stj + durations j
    let fttost j ftj = ftj - durations j

    let ft sts j = sttoft j (Map.find j sts)
    let st fts j = fttost j (Map.find j fts)

    let ftstosts fts = Map.map fttost fts
    let ststofts sts = Map.map sttoft sts

    let zeroOc r t = 0
    let maxOc r t = zmax r

    //#region schedule properties
    let lastPredCore predfunc sts = Seq.max << Seq.map (ft sts) << predfunc
    let firstSuccCore succfunc fts = Seq.min << Seq.map (st fts) << succfunc

    let lastPredFinishingTime = lastPredCore preds
    let firstSuccStartingTime = firstSuccCore succs

    let executionPeriods j stj = [stj+1..stj+durations j]

    let isActiveInPeriod sts t j = (Map.find j sts) < t && t <= ft sts j        
    let activeInPeriodSet sts t = keys sts |> Seq.filter (isActiveInPeriod sts t)

    let demandInPeriod sts r t = activeInPeriodSet sts t |> Seq.sumBy (fun j -> demands j r)

    let residualCapacity sts r t = capacities r - demandInPeriod sts r t

    let enoughCapacityForJob z sts j stj =
        resources >< executionPeriods j stj
        |> Seq.forall (fun (r,t) -> residualCapacity sts r t + z r t >= demands j r)

    let makespan sts = ft sts lastJob

    let neededOvercapacityInPeriod sts r t = max 0 (-residualCapacity sts r t)
    let totalOvercapacityCosts sts =
        resources >< [0..makespan sts]
        |> Seq.sumBy (fun (r,t) -> kappa r * float (neededOvercapacityInPeriod sts r t))

    let isScheduleResourceFeasible sts =
        Seq.forall (fun (r,t) -> (activeInPeriodSet sts t |> Seq.sumBy (fun j -> demands j r)) <= capacities r + zmax r) (resources >< horizon)
    let isSchedulePrecedenceFeasible sts =
        Seq.forall (fun j -> Map.find j sts >= lastPredFinishingTime sts j) jobs
    //#endregion

    //#region basic schedule generation schemes
    let (computeEsts, computeLfts) =        
        let elsftForJob seedKey seedVal func sfts j =
            Map.add j (if j = seedKey then seedVal else func sfts j) sfts
        ((fun () -> Seq.fold (elsftForJob firstJob 0 lastPredFinishingTime) Map.empty topOrdering),
         (fun deadline -> Seq.fold (elsftForJob lastJob deadline firstSuccStartingTime) Map.empty revTopOrdering))

    let ests = computeEsts ()
    let efts = ft ests

    let lfts = computeLfts T
    let lsts = st lfts

    let (computeEstsForPartial, computeLftsForPartial) =
        let determineElsft func selfunc partialsfts sts j =
            let elsft = func sts j
            if Map.containsKey j partialsfts then selfunc elsft (Map.find j partialsfts)
            else elsft
        let elsftForJob selfunc partialsfts seedKey seedVal func sts j =
            Map.add j (if j = seedKey then lookupOrVal seedKey partialsfts seedVal else determineElsft func selfunc partialsfts sts j) sts
        ((fun partialsts -> Seq.fold (elsftForJob max partialsts firstJob 0 lastPredFinishingTime) Map.empty topOrdering),
         (fun partialfts deadline -> Seq.fold (elsftForJob min partialfts lastJob deadline firstSuccStartingTime) Map.empty revTopOrdering))

    let earliestTimeAndResourceFeasiblePeriod z sts j =
        (numsGeq (lastPredFinishingTime sts j) |> Seq.find (enoughCapacityForJob z sts j))

    let scheduleJob z acc j = Map.add j (earliestTimeAndResourceFeasiblePeriod z acc j) acc

    let ssgsCore z sts λ = Seq.fold (scheduleJob z) sts λ
    let ssgs z λ = ssgsCore z (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)

    let quickssgs z λ =     
        let numres = Seq.length resources
        let resRem = Array2D.init numres T (fun rix tix -> capacities (rix+1) + z (rix+1) (tix+1))        

        (*resources >< executionPeriods j stj |> Seq.forall (fun (r,t) -> resRem.[r-1,t-1] >= demands j r)*)
        let enoughCap j stj =
            let mutable rix = 0
            let mutable t = stj+1    
            let mutable enough = true
            while enough && rix < numres do
                while enough && t <= stj + durations j do
                    if resRem.[rix,t-1] < demands j (rix+1) then
                        enough <- false
                    t <- t + 1
                rix <- rix + 1
            enough

        (*resources >< executionPeriods j stj |> Seq.iter (fun (r,t) -> resRem.[r-1,t-1] <- resRem.[r-1,t-1] - demands j r)*)
        let removeCap j stj =
            let mutable rix = 0
            let mutable t = stj+1            
            while rix < numres do
                while t <= stj + durations j do
                    resRem.[rix,t-1] <- resRem.[rix,t-1] - demands j (rix+1)
                    t <- t + 1
                rix <- rix + 1

        let schedjob sts j =
            let stj = Seq.find (enoughCap j) (numsGeq (lastPredFinishingTime sts j))
            removeCap j stj
            Map.add j stj sts

        Seq.fold schedjob (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)
    //#endregion

    //#region scheduling with deadline
    let baseIntervalUb j cests = Map.find j cests + durations j
    let baseIntervalLb j clsts = Map.find j clsts + 1

    let periodInBaseInterval cests clsts t j =
        t >= baseIntervalLb j clsts && t <= baseIntervalUb j cests

    let baseIntervalInPeriodSet unscheduled cests clsts t =
        Set.filter (periodInBaseInterval cests clsts t) unscheduled

    let activeInPeriodSetWithBaseInterval unscheduled sts cests clsts t =
        Set.union (Set.ofSeq (activeInPeriodSet sts t)) (baseIntervalInPeriodSet unscheduled cests clsts t)

    let demandInPeriodWithBaseInterval unscheduled sts cests clsts r t =
        activeInPeriodSetWithBaseInterval unscheduled sts cests clsts t |> Seq.sumBy (fun j -> demands j r)

    let neededOvercapacityInPeriodForJob sts r t j =
        max 0 (demands j r - residualCapacity sts r t)

    let extensionCosts sts j stj =
        resources >< [stj+1..stj+durations j]
        |> Seq.sumBy (fun (r,t) -> kappa r * float (neededOvercapacityInPeriodForJob sts r t j))

    let residualCapacityWithBaseInterval unscheduled sts cests clsts r t =
        capacities r - demandInPeriodWithBaseInterval unscheduled sts cests clsts r t

    let enoughCapacityForJobWithBaseInterval unscheduled z sts cests clsts j stj =
        resources >< executionPeriods j stj
        |> Seq.forall (fun (r,t) -> residualCapacityWithBaseInterval unscheduled sts cests clsts r t + z r t >= demands j r)
    //#endregion

    //#region SGS with consideration of deadline
    let completionTimes sts =
        Map.toList sts
        |> List.map (fun (j,stj) -> stj + durations j)
        |> Set.ofList

    let startingTimes sts =
        vals sts
        |> Set.ofSeq

    let decisionTimesForRD sts cests clsts j =
        let timeWindowPredFeas = Set.ofList [ Map.find j cests .. Map.find j clsts ]
        Set.unionMany [
            set [timeWindowPredFeas.MinimumElement; timeWindowPredFeas.MaximumElement];
            Set.intersect timeWindowPredFeas (completionTimes sts);
            timeWindowPredFeas |> Set.filter (fun t -> Set.contains (t + durations j) (startingTimes sts))]
    //#endregion

    let deadline = Map.find lastJob lfts

    //#region profit/revenue computation
    let minMaxMakespanBounds =
        let tkappar r = System.Math.Ceiling(float (Seq.sumBy (fun j -> durations j * demands j r) jobs) / float (capacities r + zmax r)) |> int
        let tkappa = resources |> Seq.map tkappar |> Seq.max
        let minMakespanApprox = max (makespan ests) tkappa
        let maxMakespanApprox = makespan (ssgs zeroOc topOrdering)
        (minMakespanApprox, maxMakespanApprox)

    let u =
        let maxOcCosts = totalOvercapacityCosts ests
        let (minMakespanApprox, maxMakespanApprox) = minMaxMakespanBounds
        let ufunc t = maxOcCosts - maxOcCosts / System.Math.Pow(float(maxMakespanApprox-minMakespanApprox), 2.0) * System.Math.Pow(float(t - minMakespanApprox), 2.0) 
        if same minMaxMakespanBounds then fun t -> float(maxMakespanApprox-t)
        else ufunc

    let revenue = u << makespan

    let profit = memoize (fun sts -> (revenue sts) - totalOvercapacityCosts sts)
    //#endregion

    //#region advanced schedule generation schemes
    let ssgsWindow chooser λ =
        let scheduleJob acc job =
            let tlower = lastPredFinishingTime acc job
            let tupper = numsGeq tlower |> Seq.find (enoughCapacityForJob zeroOc acc job)
            Map.add job (chooser acc job tlower tupper) acc

        Seq.fold scheduleJob (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)

    let ssgsOc λ =
        let completeWithoutOC sts j t =
            let candidate = Map.add j t sts
            let jix = indexOf λ j
            let subλ = Seq.skip (inc jix) λ
            ssgsCore zeroOc candidate subλ

        let chooseBestPeriod sts j tlower tupper =
            [tlower..tupper]
            |> Seq.filter (fun t -> enoughCapacityForJob maxOc sts j t)
            |> Seq.maxBy ((completeWithoutOC sts j) >> profit)

        ssgsWindow chooseBestPeriod λ

    let ssgsTau λ τ =
        let chooseWithTau sts j tlower tupper =
            let lb = tupper - int(floor(float(tupper) - float(tlower)) * (Seq.item j τ))
            numsGeq lb |> Seq.find (enoughCapacityForJob maxOc sts j)
        ssgsWindow chooseWithTau λ

    let ssgsBeta λ β = ssgsTau λ (Seq.map float β)

    // Implementation of modified priority rule method for the resource deviation problem (RD objective function)
    // Source: Zimmermann, Stark, Rieck - Projektplanung (2010)
    let deadlineCostMinHeur (d̅: int) (λ: int seq) =
        let selectBestStartingTime sts j decisionTimes =
            if Set.isEmpty decisionTimes then -1
            else
                decisionTimes
                |> Set.map (fun dt -> (dt, extensionCosts sts j dt))
                |> Set.toList
                |> List.minBy snd
                |> fst
            
        let scheduleJobTime sts j =
            let cests = computeEstsForPartial sts
            let clsts = computeLftsForPartial (ststofts sts) d̅ |> ftstosts
            let dtimes = decisionTimesForRD sts cests clsts j
            let unscheduled = Set.difference jobs (keyset sts)
            if Set.isEmpty dtimes then -1
            else
                dtimes
                |> Set.filter (fun dt -> enoughCapacityForJobWithBaseInterval unscheduled maxOc sts cests clsts j dt)
                |> selectBestStartingTime sts j

        let rec iterate sts rest =
            if Seq.isEmpty rest then sts
            else
                let j = Seq.head rest
                let stj = scheduleJobTime sts j
                if stj = -1 then sts
                else
                    iterate (Map.add j stj sts) (Seq.skip 1 rest)

        let sts = iterate (Map.ofList [(firstJob, 0)]) (Seq.filter (fun j -> j <> firstJob) λ)
        if Seq.length (keys sts) < jobs.Count then None
        else Some(sts)
    //#endregion

    //#region accessors
    member ps.FinishingTimesToStartingTimes fts =
        Map.map (fun j ftj -> ftj - durations j) fts
            
    member ps.Jobs = jobs
    member ps.Durations = durations
    member ps.Demands = demands
    member ps.Capacities = capacities
    member ps.Preds = preds
    member ps.Succs = succs
    member ps.TransSuccs = transSuccs
    member ps.Resources = resources
    member ps.Kappa = kappa
    member ps.ZMax = zmax
    member ps.U = u
    member ps.TimeHorizon = horizon   

    member ps.EarliestStartingTimes = mapToFunc ests
    member ps.LatestStartingTimes = lsts
    member ps.EarliestFinishingTimes = efts
    member ps.LatestFinishingTimes = mapToFunc lfts

    member ps.Deadline = deadline
    member ps.Makespan = makespan

    member ps.ActiveInPeriodSet = activeInPeriodSet
    member ps.EnoughCapacityForJob = enoughCapacityForJob
    member ps.LastPredFinishingTime = lastPredFinishingTime

    member ps.TotalOvercapacityCosts = totalOvercapacityCosts
    member ps.NeededOCForSchedule = neededOvercapacityInPeriod

    member ps.SerialSGS = ssgs
    member ps.QuickSerialSGS = quickssgs
    member ps.SerialSGSOC = ssgsOc
    member ps.SerialSGSBeta = ssgsBeta
    member ps.SerialSGSTau = ssgsTau
    member ps.DeadlineCostMinHeur = deadlineCostMinHeur
    member ps.Profit = profit

    member ps.UpperBoundForMakespanWithOC oc = makespan (ssgs oc canonicalOrdering)

    member ps.CanonicalOrder = canonicalOrdering
    member ps.TopologicalOrder = topOrdering

    member ps.IsScheduleResourceFeasible = isScheduleResourceFeasible
    member ps.IsSchedulePrecedenceFeasible = isSchedulePrecedenceFeasible
    member ps.IsScheduleFeasible sts = (isSchedulePrecedenceFeasible sts) && (isScheduleResourceFeasible sts)
    //#endregion

    static member Create(jobs, durations, demands, capacities, preds, resources, kappa, zmax) =
        let arrayToBaseOneMap arr = Array.mapi (fun ix e -> (inc ix,e)) arr |> Map.ofSeq
        ProjectStructure(jobs, arrayToBaseOneMap durations |> mapToFunc,
                         demands |> Array.map arrayToBaseOneMap |> arrayToBaseOneMap |> map2DToFunc,
                         preds, resources, capacities |> arrayToBaseOneMap |> mapToFunc, kappa, zmax)