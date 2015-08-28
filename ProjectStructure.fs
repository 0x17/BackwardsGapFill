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

    let topOrdering = topSort jobs preds
    let revTopOrdering = topSort jobs succs

    let sttoft j stj = stj + durations j
    let fttost j ftj = ftj - durations j

    let ft sts j = sttoft j (Map.find j sts)
    let st fts j = fttost j (Map.find j fts)

    let zeroOc r t = 0
    let maxOc r t = zmax r

    //#region schedule properties
    let lastPredFinishingTime sts = Seq.max << Seq.map (ft sts) << preds
    let firstSuccStartingTime fts = Seq.min << Seq.map (st fts) << succs

    let isFinishedAtEndOfPeriod sts t j = Map.containsKey j sts && ft sts j <= t
    let arePredsFinished sts j t = preds j |> Set.forall (isFinishedAtEndOfPeriod sts t)

    let executionPeriods j stj = [stj+1..stj+durations j]

    let isActiveInPeriod sts t j = (Map.find j sts) < t && t <= ft sts j        
    let activeInPeriodSet sts t = keys sts |> Seq.filter (isActiveInPeriod sts t)

    let demandInPeriod sts r t = activeInPeriodSet sts t |> Seq.sumBy (fun j -> demands j r)

    let residualCapacity sts r t = capacities r - demandInPeriod sts r t

    let enoughCapacityForJob z sts j stj =
        resources >< executionPeriods j stj
        |> Seq.forall (fun (r,t) -> residualCapacity sts r t + z r t >= demands j r)

    let predsAndCapacityFeasible z sts j t = arePredsFinished sts j t && enoughCapacityForJob z sts j t

    let makespan sts = ft sts lastJob

    let neededOvercapacityInPeriod sts r t = max 0 (-residualCapacity sts r t)
    let totalOvercapacityCosts sts =
        resources >< [0..makespan sts]
        |> Seq.sumBy (fun (r,t) -> kappa r * float (neededOvercapacityInPeriod sts r t))
    //#endregion

    //#region basic schedule generation schemes
    let (computeEsts, computeLfts) =        
        let elsftForJob seedKey seedVal func sts j =
            Map.add j (if j = seedKey then seedVal else func sts j) sts
        ((fun () -> Seq.fold (elsftForJob firstJob 0 lastPredFinishingTime) Map.empty topOrdering),
         (fun deadline -> Seq.fold (elsftForJob lastJob deadline firstSuccStartingTime) Map.empty revTopOrdering))

    let ests = computeEsts ()
    let efts = ft ests

    let lfts = computeLfts T
    let lsts = st lfts

    let earliestTimeAndResourceFeasiblePeriod z sts j =
        (numsGeq (lastPredFinishingTime sts j) |> Seq.find (enoughCapacityForJob z sts j))

    let scheduleJob z acc j = Map.add j (earliestTimeAndResourceFeasiblePeriod z acc j) acc
    let ssgsCore z sts λ = Seq.fold (scheduleJob z) sts λ
    let ssgs z λ = ssgsCore z (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)
    //#endregion

    //#region scheduling with deadline
    let timeWindow j cests clsts = Map.find j clsts - Map.find j cests
    let isFixed j cests clsts = timeWindow j cests clsts = 0
    let isPartiallyFixed j cests clsts =
        let tw = timeWindow j cests clsts
        tw > 0 && tw < durations j
    let baseInterval j cests clsts = [Map.find j clsts .. Map.find j cests + durations j]

    let baseIntervalInPeriodSet unscheduled cests clsts t =
        unscheduled |> Set.filter (fun j -> baseInterval j cests clsts |> List.exists ((=) t))        
    let activeInPeriodSetWithBaseInterval sts cests clsts t =
        let unscheduled = Set.difference jobs (keyset sts)
        Set.union (Set.ofSeq (activeInPeriodSet sts t)) (baseIntervalInPeriodSet unscheduled cests clsts t)

    let demandInPeriodWithBaseInterval sts cests clsts r t =
        activeInPeriodSetWithBaseInterval sts cests clsts t |> Seq.sumBy (fun j -> demands j r)

    let neededOvercapacityInPeriodForJob sts r t j = max 0 (demands j r - residualCapacity sts r t)
    let extensionCosts sts j stj =
        resources >< [stj+1..stj+durations j]
        |> Seq.sumBy (fun (r,t) -> kappa r * float (neededOvercapacityInPeriodForJob sts r t j))

    let residualCapacityWithBaseInterval sts cests clsts r t = capacities r - demandInPeriodWithBaseInterval sts cests clsts r t

    let enoughCapacityForJobWithBaseInterval z sts cests clsts j stj =
        resources >< executionPeriods j stj
        |> Seq.forall (fun (r,t) -> residualCapacityWithBaseInterval sts cests clsts r t + z r t >= demands j r)
    //#endregion

    //#region SGS with consideration of deadline
    let latestTimeAndResourceFeasiblePeriod z d̅ sts j =
        (numsLeq (firstSuccStartingTime sts j) |> Seq.find (enoughCapacityForJob z sts j))

    let timeAndResourceFeasiblePeriods z d̅ sts j =
        let lb = earliestTimeAndResourceFeasiblePeriod z sts j
        let ub = latestTimeAndResourceFeasiblePeriod z d̅ sts j
        let between = [lb + 1 .. ub - 1] |> List.filter (enoughCapacityForJob z sts j) |> Set.ofList
        Set.union (set [lb; ub]) between

    let completionTimes sts = sts |> Map.toList |> List.map (fun (j,stj) -> stj + durations j) |> Set.ofList
    let startingTimes sts = sts |> vals |> Set.ofSeq

    let decisionTimesForRD d̅ sts j =
        let wtilde = timeAndResourceFeasiblePeriods maxOc d̅ sts j        
        Set.unionMany [
            set [wtilde.MinimumElement; wtilde.MaximumElement];
            Set.intersect wtilde (completionTimes sts);
            wtilde |> Set.filter (fun t -> Set.contains (t + durations j) (startingTimes sts))]
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
        if same minMaxMakespanBounds then fun t -> -float(t)
        else ufunc

    let revenue = u << makespan

    let profit sts = (revenue sts) - totalOvercapacityCosts sts
    //#endregion

    //#region advanced schedule generation schemes
    let ssgsWindow chooser λ =
        let scheduleJob acc job =
            let tlower = lastPredFinishingTime acc job
            let tupper = numsGeq tlower |> Seq.find (enoughCapacityForJob zeroOc acc job)
            Map.add job (chooser acc job tlower tupper) acc

        Seq.fold scheduleJob (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)

    let ssgsoc λ =
        let completeWithoutOC sts j t =
            let candidate = Map.add j t sts
            let jix = indexOf λ j
            let subλ = Seq.skip (inc jix) λ
            ssgsCore zeroOc candidate subλ

        let chooseBestPeriod sts j tlower tupper =
            [tlower..tupper]
            |> Seq.filter (fun t -> enoughCapacityForJob maxOc sts j t)
            |> Seq.maxBy (fun t -> completeWithoutOC sts j t |> profit)

        ssgsWindow chooseBestPeriod λ

    let ssgsTau λ τ =
        let chooseWithTau sts j tlower tupper =
            let lb = tlower + int(floor((float(tupper) - float(tlower)) / 100.0 * float(Seq.nth j τ)))
            numsGeq lb |> Seq.find (enoughCapacityForJob maxOc sts j)
        ssgsWindow chooseWithTau λ

    let ssgsBeta λ β =
        let betaToTau b = if b = 1 then 0 else 100
        ssgsTau λ (Seq.map betaToTau β)     

    // Implementation of modified priority rule method for the resource deviation problem (RD objective function)
    // Source: Zimmermann, Stark, Rieck - Projektplanung (2010)
    let deadlineCostMinHeur (d̅: int) (λ: int seq) =
        let baseEsts = computeEsts () |> mapToFunc
        let baseLsts = computeLfts d̅ |> st

        let computeELstsForSchedule sts baseVals seedKey seedVal rel relfunc selector ordering =
            let stForSchedule sts j =
                if j = seedKey then seedVal
                else selector (baseVals j) (if rel j |> Set.isEmpty then seedVal else relfunc sts j)
            Seq.fold (fun acc j -> Map.add j (stForSchedule sts j) acc) Map.empty ordering

        let selectBestStartingTime sts j decisionTimes =
            decisionTimes
            |> Set.map (fun dt -> (dt, extensionCosts sts j dt))
            |> Set.toList
            |> List.minBy snd
            |> fst

        let scheduleJob sts j =
            let cests = computeELstsForSchedule sts baseEsts firstJob 0 preds lastPredFinishingTime max topOrdering
            let clsts = computeELstsForSchedule sts baseLsts lastJob d̅ succs firstSuccStartingTime min revTopOrdering
            let stj =
                let dtimes = decisionTimesForRD d̅ sts j
                if Set.isEmpty dtimes then -1
                else
                    dtimes
                    |> Set.filter (fun dt -> enoughCapacityForJobWithBaseInterval maxOc sts cests clsts j dt)
                    |> selectBestStartingTime sts j
            Map.add j stj sts
            
        let sts = Seq.fold scheduleJob (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)
        if Map.exists (fun k v -> v = -1) sts then None
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
    member ps.DeadlineCostMinHeur = deadlineCostMinHeur
    member ps.Profit = profit
    //#endregion

    static member Create(jobs, durations, demands, capacities, preds, resources, kappa, zmax) =
        let arrayToBaseOneMap arr = Array.mapi (fun ix e -> (inc ix,e)) arr |> Map.ofSeq
        ProjectStructure(jobs, arrayToBaseOneMap durations |> mapToFunc,
                         demands |> Array.map arrayToBaseOneMap |> arrayToBaseOneMap |> map2DToFunc,
                         preds, resources, capacities |> arrayToBaseOneMap |> mapToFunc, kappa, zmax)