namespace RCPSP

open System.Collections.Generic
open Utils

type IntMap = Dictionary<int,int>
type IntLst = List<int>

type ProjectStructure(jobs:         Set<int>,
                      durations:    int -> int,
                      demands:      int -> int -> int,
                      costs:        int -> float,
                      preds:        int -> Set<int>,
                      resources:    Set<int>,
                      capacities:   int -> int,
                      topOrdering:  int seq,
                      reachedLevels:Set<int>,
                      kappa:        int->float,
                      zmax:         int->int) =
    let firstJob = Set.minElement jobs
    let lastJob = Set.maxElement jobs
    let actualJobs = Set.difference jobs (Set.ofSeq [firstJob; lastJob])

    let T = Seq.sumBy durations jobs
    let horizon = [1..T]

    let ft (sts:IntMap) j = sts.[j] + durations j
    let st (fts:IntMap) j = fts.[j] - durations j

    let lastPredFinishingTime (sts:IntMap) = Seq.max << Seq.map (ft sts) << preds

    let jobWithoutPreds = Set.isEmpty << preds

    let computeEsts () =
        let ests = new IntMap ()
        for j in Seq.filter jobWithoutPreds jobs do
            ests.Add (j, 0)
        for j in topOrdering do
            if not(ests.ContainsKey j) then
                ests.Add(j, lastPredFinishingTime ests j)
        ests

    let ests = computeEsts ()
    let efts = (fun j -> ft ests j)

    let computeLfts () =
        let lfts = new IntMap ()
        let rec traversePreds j =
            for i in preds j do
                let stj = st lfts j
                lfts.[i] <- if lfts.ContainsKey i then Seq.min [lfts.[i]; stj] else stj
                traversePreds i
        lfts.Add (lastJob, T)
        traversePreds lastJob
        lfts

    let lfts = computeLfts ()
    let lsts = (fun j -> st lfts j)    

    let isFinishedAtEndOfPeriod (sts:IntMap) t j = ft sts j <= t
    let arePredsFinished (sts:IntMap) j t = Seq.forall (isFinishedAtEndOfPeriod sts t) <| preds j

    let isActiveInPeriod (sts:IntMap) t j = sts.[j] < t && t <= ft sts j        
    let activeInPeriodSet (sts:IntMap) t = Seq.filter (isActiveInPeriod sts t) sts.Keys

    let demandInPeriod (sts:IntMap) r t = activeInPeriodSet sts t |> Seq.sumBy (fun j -> demands j r)

    let residualCapacity z (sts:IntMap) r t = capacities r + z r t - demandInPeriod sts r t

    let enoughCapacityForJob z (sts:IntMap) j stj =
        cartesianProduct resources [stj+1..stj+durations j]
        |> Seq.forall (fun (r,t) -> residualCapacity z sts r t >= demands j r)
    
    let ssgs z =
        let sts = new IntMap ()
        for job in topOrdering do
            let mutable t = ests.[job]
            while not(arePredsFinished sts job t) || not(enoughCapacityForJob z sts job t) do
                t <- t+1
            sts.Add (job,t)
        sts

    let afterLatestGaps (sts:IntMap) =
        let alg = new IntLst ()
        if sts.[lastJob] > ests.[lastJob] then            
            let rest = new Stack<int> ([lastJob])
            while rest.Count > 0 do
                let j = rest.Pop ()
                alg.Add j
                let latestFt = lastPredFinishingTime sts j
                if latestFt = sts.[j] then
                    preds j |> Seq.filter (fun i -> ft sts i = latestFt) |> Seq.iter rest.Push
        alg

    let onePeriodLeftShift (sts:IntMap) js = Seq.iter (fun j -> sts.[j] <- sts.[j]-1) js

    let zeroOc r t = 0

    let makespan (sts:IntMap) = ft sts lastJob

    let neededOvercapacityInPeriod (sts:IntMap) r t = List.max [0; -residualCapacity zeroOc sts r t]
    let totalOvercapacityCosts (sts:IntMap) =
        cartesianProduct resources [0..makespan sts]
        |> Seq.sumBy (fun (r,t) -> kappa r * float(neededOvercapacityInPeriod sts r t))

    let tryDecrementMakespan (sts:IntMap) =
        let alg = afterLatestGaps sts
        if alg.Count > 0 then
            onePeriodLeftShift sts alg;
            cartesianProduct resources [0..makespan sts]
            |> Seq.forall (fun (r,t) -> neededOvercapacityInPeriod sts r t < zmax r)
        else false       

    let urel = [|(1, 0.0); (2, 0.05); (3, 0.10); (4, 0.15); (5, 0.20)|] |> arrayToFunc
    let umax = 2.0 * Seq.sumBy costs actualJobs
    let u l t = float(umax) * float(T-t)/float(T) * (1.0 - urel l)
    let ustar t = reachedLevels |> Seq.map (fun l -> u l t) |> Seq.max |> int

    let profit (sts:IntMap) =
        let revenue = ustar (makespan sts)
        let tcosts = totalOvercapacityCosts sts + Seq.sumBy costs actualJobs
        revenue - int tcosts

    let computeBestSchedule () =
        let profitsToSchedules = new Dictionary<int, IntMap> ()
        let schedule = ssgs zeroOc        
        profitsToSchedules.Add (profit schedule, new IntMap(schedule))
        while tryDecrementMakespan schedule do
            let p = profit schedule
            if not(profitsToSchedules.ContainsKey p) then
                profitsToSchedules.Add(p, new IntMap(schedule))
        let bestSchedule = profitsToSchedules.[Seq.max profitsToSchedules.Keys]
        ((fun r t -> neededOvercapacityInPeriod bestSchedule r t), bestSchedule)

    let scheduleToGrid sts r =
        let nrows = capacities r + zmax r
        let grid = Array2D.zeroCreate nrows T
        for t in horizon do
            let actJobs = activeInPeriodSet sts t
            let mutable colCtr = nrows - 1
            for j in actJobs do
                for k in [1..demands j r] do
                    Array2D.set grid colCtr (t-1) j
                    colCtr <- colCtr - 1
        grid

    // Interface methods
    member ps.Profit = profit
    member ps.AfterLatestGaps = afterLatestGaps
    member ps.ComputeOptimalSchedule = computeBestSchedule
    member ps.SerialScheduleGenerationScheme = ssgs
    member ps.ScheduleToGrid = scheduleToGrid    
    member ps.FinishingTimesToStartingTimes (fts:IntMap) =
        let sts = new IntMap ()
        for j in jobs do sts.Add (j, fts.[j] - durations j)
        sts
    // Interface properties
    member ps.Jobs = jobs
    member ps.ActualJobs = actualJobs
    member ps.Durations = durations
    member ps.Demands = demands
    member ps.Costs = costs
    member ps.Capacities = capacities
    member ps.Preds = preds
    member ps.Resources = resources
    member ps.ReachedLevels = reachedLevels
    member ps.TimeHorizon = horizon
    member ps.EarliestStartingTimes = ests
    member ps.LatestStartingTimes = lsts
    member ps.EarliestFinishingTimes = efts
    member ps.LatestFinishingTimes = lfts
    member ps.Kappa = kappa
    member ps.UStar = ustar
    member ps.ZMax = zmax   

    static member Create(jobs, durations, demands, costs, capacities, preds, resources, topOrdering, reachedLevels, kappa, zmax) =
        let arrayToBaseOneMap arr = Array.mapi (fun ix e -> (ix+1,e)) arr |> Map.ofSeq
        new ProjectStructure(jobs, arrayToBaseOneMap durations |> mapToFunc, demands |> Array.map arrayToBaseOneMap |> arrayToBaseOneMap |> map2DToFunc,
                             costs, preds, resources, capacities |> arrayToBaseOneMap |> mapToFunc, topOrdering, reachedLevels, kappa, zmax)