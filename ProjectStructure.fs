namespace RCPSP

open System.Collections.Generic
open Utils

type IntMap = Dictionary<int,int>
type IntLst = List<int>

type ProjectStructure(jobs:Set<int>, durations:int -> int, demands:int -> int -> int, costs:int -> int,
                      preds:int -> Set<int>, resources:Set<int>, capacities:int -> int,
                      topOrdering:int seq, reachedLevels:Set<int>, horizon: int, kappa:int->int, zmax:int->int) =
    let lastJob = jobs.Count

    let urel = [|(1, 0.0); (2, 0.05); (3, 0.10); (4, 0.15); (5, 0.20)|] |> arrayToFunc

    let umax = 2 * Seq.sumBy costs [2..lastJob-1]

    let T = horizon
    let u l t = float(umax) * float(T-t)/float(T) * (1.0 - urel l)
    let ustar t = Seq.maxBy (fun l -> u l t) reachedLevels

    let ft (sts:IntMap) j = sts.[j] + durations j

    let lastPredFinishedTime (sts:IntMap) = Seq.max << Seq.map (ft sts) << preds

    let isJobWithoutPreds = Set.isEmpty << preds 

    let computeEsts() =
        let ests = new IntMap()
        for j in Seq.filter isJobWithoutPreds jobs do
            ests.Add(j, 1)
        for j in topOrdering do
            if not(ests.ContainsKey(j)) then
                ests.Add(j, lastPredFinishedTime ests j)
        ests

    let ests = computeEsts()
    let efts = (fun j -> ests.[j]+durations(j))

    let computeLfts() =
        let lfts = new IntMap()
        let rec traversePreds j =
            for i in preds j do
                let stj = lfts.[j]-durations j
                lfts.[i] <- if lfts.ContainsKey(i) then Seq.min [lfts.[i]; stj] else stj
                traversePreds i
        lfts.Add(lastJob, T)
        traversePreds lastJob
        lfts

    let lfts = computeLfts()
    let lsts = (fun j -> lfts.[j]-durations(j))    

    let isFinishedInPeriod (sts:IntMap) t j = ft sts j <= t
    let arePredsFinished (sts:IntMap) j t = Seq.forall (isFinishedInPeriod sts t) <| preds j

    let isActiveInPeriod (sts:IntMap) t j = sts.[j] <= t && t < ft sts j        
    let activeInPeriodSet (sts:IntMap) t = Seq.filter (isActiveInPeriod sts t) sts.Keys

    let demandInPeriod (sts:IntMap) r t = activeInPeriodSet sts t |> Seq.map (fun j -> demands j r) |> Seq.sum

    let residualCapacity z (sts:IntMap) r t = capacities r + z r t - demandInPeriod sts r t

    let enoughCapacityForJob z (sts:IntMap) j stj =
        cartesianProduct resources [stj..stj+(durations j)-1]
        |> Seq.forall (fun (r,t) -> residualCapacity z sts r t >= demands j r)

    let zeroOc r t = 0
    
    let ssgs z =
        let sts = new IntMap()
        for job in topOrdering do
            let mutable t = ests.[job]
            while not(arePredsFinished sts job t) || not(enoughCapacityForJob z sts job t) do
                t <- t+1
            sts.Add(job,t)
        sts

    let afterLatestGaps (sts:IntMap) =
        let alg = new IntLst()
        let rec tryAddChildsRec parentJob =
            let latestFt = lastPredFinishedTime sts parentJob
            if latestFt = sts.[parentJob] then
                for i in preds parentJob do
                    if ft sts i = latestFt then
                        alg.Add(i)
                        tryAddChildsRec i
        if sts.[lastJob] > ests.[lastJob] then
            alg.Add(lastJob)
            tryAddChildsRec lastJob
        alg

    let onePeriodLeftShift (sts:IntMap) js = Seq.iter (fun j -> sts.[j] <- sts.[j]-1) js

    let neededOvercapacityInPeriod (sts:IntMap) r t = List.max [0; -residualCapacity zeroOc sts r t]
    let totalOvercapacityCosts (sts:IntMap) =
        cartesianProduct resources [1..sts.[lastJob]]
        |> Seq.sumBy (fun (r,t) -> kappa r * neededOvercapacityInPeriod sts r t)

    let tryDecrementMakespan (sts:IntMap) =
        let alg = afterLatestGaps sts
        if alg.Count > 0 then
            onePeriodLeftShift sts alg;
            cartesianProduct resources [1..sts.[lastJob]]
            |> Seq.forall (fun (r,t) -> neededOvercapacityInPeriod sts r t < zmax r)
        else false
        
    let makespan (sts:IntMap) = ft sts lastJob
    let profit (sts:IntMap) =
        let revenue = ustar (makespan sts)
        let tcosts = totalOvercapacityCosts sts// + Seq.sumBy costs [2..lastJob-1]
        revenue - tcosts

    let computeBestSchedule() =
        let profitsToSchedules = new Dictionary<int, IntMap>()
        let schedule = ssgs zeroOc
        let rec iterateMakespanDecrement() =
            if tryDecrementMakespan schedule then
                let p = profit schedule
                if not(profitsToSchedules.ContainsKey(p)) then
                    profitsToSchedules.Add(p, new IntMap(schedule))
                iterateMakespanDecrement()
        profitsToSchedules.Add(profit(schedule), new IntMap(schedule))
        iterateMakespanDecrement()
        profitsToSchedules.[Seq.max profitsToSchedules.Keys]

    let scheduleToGrid zmax sts r =
        let T = Seq.sumBy durations jobs
        let nrows = capacities(r) + zmax r
        let grid = Array2D.zeroCreate nrows T
        for t in [1..T] do
            let actJobs = activeInPeriodSet sts t
            let mutable colCtr = nrows - 1
            for j in actJobs do
                for k in [1..demands j r] do
                    Array2D.set grid colCtr (t-1) j
                    colCtr <- colCtr - 1
        grid

    // Interface methods
    member ps.ComputeOptimalSchedule = computeBestSchedule
    member ps.SerialScheduleGenerationScheme = ssgs
    member ps.ScheduleToGrid = scheduleToGrid    
    // Interface properties
    member ps.Jobs = jobs
    member ps.Durations = durations
    member ps.Demands = demands
    member ps.Costs = costs
    member ps.Capacities = capacities
    member ps.Preds = preds
    member ps.Resources = resources
    member ps.ReachedLevels = reachedLevels
    member ps.TimeHorizon = [1..T]
    member ps.EarliestStartingTimes = ests
    member ps.LatestStartingTimes = lsts
    member ps.EarliestFinishingTimes = efts
    member ps.LatestFinishingTimes = lfts
    member ps.Kappa = kappa
    member ps.UStar = ustar
    member ps.ZMax = zmax

    static member Create(jobs, durations, demands, costs, capacities, preds, resources, topOrdering, reachedLevels, horizon, kappa, zmax) =
        let arrayToBaseOneMap arr = Array.mapi (fun ix e -> (ix+1,e)) arr |> Map.ofSeq
        new ProjectStructure(jobs, arrayToBaseOneMap durations |> mapToFunc, demands |> Array.map arrayToBaseOneMap |> arrayToBaseOneMap |> map2DToFunc, costs,
                             preds, resources, capacities |> arrayToBaseOneMap |> mapToFunc, topOrdering, reachedLevels, horizon, kappa, zmax)