namespace RCPSP

open System.Collections.Generic
open Utils

type IntMap = Map<int,int>
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
                      kappa:        int -> float,
                      zmax:         int -> int) =
    let firstJob = Set.minElement jobs
    let lastJob = Set.maxElement jobs
    let actualJobs = Set.difference jobs (set [firstJob; lastJob])

    let T = Seq.sumBy durations jobs
    let horizon = [1..T]

    let ft (sts:IntMap) j = sts.[j] + durations j
    let st (fts:IntMap) j = fts.[j] - durations j

    let lastPredFinishingTime sts = Seq.max << Seq.map (ft sts) << preds
    
    let computeEsts () =
        let estForJob ests j =
            let t = if j = firstJob then 0 else lastPredFinishingTime ests j
            Map.add j t ests
        Seq.fold estForJob Map.empty topOrdering

    let ests = computeEsts ()
    let efts = (fun j -> ft ests j)

    let computeLfts () =
        let rec traversePreds lfts j =
            let addPred acc i =
                let stj = st lfts j
                let t = if Map.containsKey i acc then Seq.min [acc.[i]; stj] else stj
                traversePreds (acc.Add(i,t)) i
            preds j |> Seq.fold addPred lfts
        traversePreds (Map.ofList [(lastJob, T)]) lastJob 

    let lfts = computeLfts ()
    let lsts = (fun j -> st lfts j)    

    let isFinishedAtEndOfPeriod (sts:IntMap) t j = sts.ContainsKey j && ft sts j <= t
    let arePredsFinished sts j t = preds j |> Set.forall (isFinishedAtEndOfPeriod sts t)

    let isActiveInPeriod (sts:IntMap) t j = sts.[j] < t && t <= ft sts j        
    let activeInPeriodSet sts t = keys sts |> Seq.filter (isActiveInPeriod sts t)

    let demandInPeriod sts r t = activeInPeriodSet sts t |> Seq.sumBy (fun j -> demands j r)

    let residualCapacity z sts r t = capacities r + z r t - demandInPeriod sts r t

    let enoughCapacityForJob z sts j stj =
        cartesianProduct resources [stj+1..stj+durations j]
        |> Seq.forall (fun (r,t) -> residualCapacity z sts r t >= demands j r)

    let predsAndCapacityFeasible sts job z t = arePredsFinished sts job t && enoughCapacityForJob z sts job t
    
    let ssgsCore sts λ z =
        let scheduleJob acc j =
            let t = numsGeq ests.[j] |> Seq.find (predsAndCapacityFeasible acc j z)
            Map.add j t acc
        Seq.fold scheduleJob sts λ

    let ssgs λ z = ssgsCore Map.empty λ z
    
    let psgs λ z =
        let nextDecisionPoint sts running = running |> Seq.map (ft sts) |> Seq.min
        let running sts t = (keys sts) |> Seq.filter (fun j -> t <= ft sts j)
        
        let rec scheduleEligible sts rest t =
            let eligible = Seq.filter (fun j -> predsAndCapacityFeasible sts j z t) rest
            if Seq.isEmpty eligible then (sts,rest)
            else
                let j = eligible |> Seq.minBy (indexOf rest)
                scheduleEligible (Map.add j t sts) (without j rest) t

        let rec buildSchedule sts rest t =
            if List.isEmpty rest then sts
            else
                let nt = nextDecisionPoint sts (running sts t)
                let pair = scheduleEligible sts rest nt
                buildSchedule (fst pair) (snd pair) (nt+1)

        let rest = List.ofSeq (Seq.skip 1 λ)
        buildSchedule (Map.ofSeq [(Seq.head λ, 0)]) rest 0

    let afterLatestGaps (sts:IntMap) =
        let alg = new IntLst ()
        if sts.[lastJob] > ests.[lastJob] then            
            let rest = new Stack<int> ([lastJob])
            while rest.Count > 0 do
                let j = rest.Pop ()
                alg.Add j
                let latestFt = lastPredFinishingTime sts j
                if latestFt = sts.[j] then
                    preds j
                    |> Seq.filter (fun i -> ft sts i = latestFt)
                    |> Seq.iter rest.Push
        alg

    let onePeriodLeftShift sts js =
        Map.map (fun j stj -> stj-(if contains j js then 1 else 0)) sts

    let zeroOc r t = 0

    let makespan sts = ft sts lastJob

    let neededOvercapacityInPeriod sts r t = List.max [0; -residualCapacity zeroOc sts r t]
    let totalOvercapacityCosts sts =
        cartesianProduct resources [0..makespan sts]
        |> Seq.sumBy (fun (r,t) -> kappa r * float(neededOvercapacityInPeriod sts r t))

    let scheduleFeasibleWithMaxOC sts =
        cartesianProduct resources [0..makespan sts]
        |> Seq.forall (fun (r,t) -> neededOvercapacityInPeriod sts r t <= zmax r)

    let tryDecrementMakespan sts =
        let alg = afterLatestGaps sts
        if alg.Count > 0 then
            let nsts = onePeriodLeftShift sts alg
            if scheduleFeasibleWithMaxOC nsts then Some(nsts)
            else None
        else None

    let urel = [|(1, 0.0); (2, 0.05); (3, 0.10); (4, 0.15); (5, 0.20)|] |> arrayToFunc
    let umax = 2.0 * Seq.sumBy costs actualJobs
    let u l t = float(umax) * float(T-t)/float(T) * (1.0 - urel l)
    let ustar t = reachedLevels |> Seq.map (fun l -> u l t) |> Seq.max

    let profit sts =
        let revenue = ustar (makespan sts)
        let tcosts = totalOvercapacityCosts sts + Seq.sumBy costs actualJobs
        revenue - tcosts

    let modifiedSgsHeuristic sgsfunc λ () =
        let schedule = sgsfunc λ (fun r t -> zmax r)
        let z = (fun r t -> neededOvercapacityInPeriod schedule r t)
        (z, schedule)

    let modifiedPsgsHeuristic = modifiedSgsHeuristic psgs
    let modifiedSsgsHeuristic = modifiedSgsHeuristic ssgs

    let backwardsGapFillHeuristic λ () =
        let rec buildProfitToSchedulesMapping acc sts =
            let nstsOption = tryDecrementMakespan sts
            if nstsOption.IsSome then
                let nsts = nstsOption.Value
                let p = profit nsts
                let nval = if Map.containsKey p acc then sts else nsts
                buildProfitToSchedulesMapping (Map.add p nval acc) nsts
            else acc

        let schedule = ssgs λ zeroOc
        let profitsToSchedules = buildProfitToSchedulesMapping (Map.ofList [profit schedule, schedule]) schedule

        let bestSchedule = profitsToSchedules.[Seq.max (keys profitsToSchedules)]
        ((fun r t -> neededOvercapacityInPeriod bestSchedule r t), bestSchedule)

    // Idee: ohne weiter Planung nötige ZK für einplanen in jetzigem t berechnen und variieren von tlower..tupper?
    let cleverSsgs λ z =
        let computeCandidateSchedule sts j t =
            let candidate = Map.add j t sts
            let jix = Seq.findIndex (fun i -> i = j) λ
            let subλ = Seq.skip (jix+1) λ
            ssgsCore candidate subλ z

        let scoreForScheduleCandidate candidate =
            (ustar << makespan) candidate - totalOvercapacityCosts candidate

        let chooseBestPeriod sts j tlower tupper =
            [tlower..tupper]
            |> Seq.maxBy (scoreForScheduleCandidate << computeCandidateSchedule sts j)

        let scheduleJob acc job =
            let tlower = numsGeq ests.[job] |> Seq.find (arePredsFinished acc job)
            let tupper = numsGeq tlower |> Seq.find (enoughCapacityForJob z acc job)
            Map.add job (chooseBestPeriod acc job tlower tupper) acc
        let sts = Seq.fold scheduleJob Map.empty λ

        ((fun r t -> neededOvercapacityInPeriod sts r t), sts)

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

    let finishingTimesToStartingTimes fts =
        Map.map (fun j ftj -> ftj - durations j) fts

    let calculateGap optimalSts sts =
        let optProfit = profit optimalSts
        (optProfit - profit sts) / optProfit
    
    member val Jobs = jobs
    member val ActualJobs = actualJobs
    member val Durations = durations
    member val Demands = demands
    member val Costs = costs
    member val Capacities = capacities
    member val Preds = preds
    member val Resources = resources    
    member val Kappa = kappa
    member val UStar = ustar
    member val ZMax = zmax
    member val TimeHorizon = horizon
    member val ReachedLevels = reachedLevels

    member val EarliestStartingTimes = ests
    member val LatestStartingTimes = lsts
    member val EarliestFinishingTimes = efts
    member val LatestFinishingTimes = lfts

    member ps.Profit = profit
    member ps.Makespan = makespan
    member ps.TotalOvercapacityCosts = totalOvercapacityCosts
    member ps.AfterLatestGaps = afterLatestGaps
    member ps.ModifiedPsgsHeuristicDefault = modifiedPsgsHeuristic topOrdering 
    member ps.ModifiedPsgsHeuristic = modifiedPsgsHeuristic
    member ps.ModifiedSsgsHeuristicDefault = modifiedSsgsHeuristic topOrdering
    member ps.ModifiedSsgsHeuristic = modifiedSsgsHeuristic
    member ps.BackwardsGapFillHeuristicDefault = backwardsGapFillHeuristic topOrdering
    member ps.BackwardsGapFillHeuristic = backwardsGapFillHeuristic
    member ps.SerialScheduleGenerationScheme = ssgs topOrdering
    member ps.CleverSSGS = cleverSsgs topOrdering
    member ps.ParallelScheduleGenerationScheme = psgs topOrdering
    member ps.ScheduleToGrid = scheduleToGrid
    member ps.FinishingTimesToStartingTimes = finishingTimesToStartingTimes
    member ps.CalculateGap = calculateGap

    static member Create(jobs, durations, demands, costs, capacities, preds, resources, topOrdering, reachedLevels, kappa, zmax) =
        let arrayToBaseOneMap arr = Array.mapi (fun ix e -> (ix+1,e)) arr |> Map.ofSeq
        new ProjectStructure(jobs, arrayToBaseOneMap durations |> mapToFunc, demands |> Array.map arrayToBaseOneMap |> arrayToBaseOneMap |> map2DToFunc,
                             costs, preds, resources, capacities |> arrayToBaseOneMap |> mapToFunc, topOrdering, reachedLevels, kappa, zmax)