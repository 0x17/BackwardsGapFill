namespace RCPSP

open Microsoft.FSharp.Collections
open System.Collections.Generic

open Utils
open TopologicalSorting

type IntMap = Map<int,int>

type ProjectStructure(jobs, durations, demands, preds: int -> Set<int>, resources, capacities, kappa, zmax) =
    let firstJob = Set.minElement jobs
    let lastJob = Set.maxElement jobs
    let actualJobs = Set.difference jobs (set [firstJob; lastJob])

    let T = Seq.sumBy durations jobs
    let mutable horizon = [1..T]

    let transPreds = transitiveHull preds
    let succs = memoize (fun i -> jobs |> Seq.filter (fun j -> (preds j).Contains i) |> Set.ofSeq)
    let transSuccs = transitiveHull succs

    let topOrdering = topSort jobs preds
    let revTopOrdering = topSort jobs succs

    let ft (sts:IntMap) j = sts.[j] + durations j
    let st (fts:IntMap) j = fts.[j] - durations j

    let lastPredFinishingTime sts = Seq.max << Seq.map (ft sts) << preds
    let firstSuccStartingTime fts = Seq.min << Seq.map (st fts) << succs
    
    let (computeEsts, computeLfts) =        
        let elsftForJob seedKey seedVal func sts j =
            Map.add j (if j = seedKey then seedVal else func sts j) sts
        ((fun () -> Seq.fold (elsftForJob firstJob 0 lastPredFinishingTime) Map.empty topOrdering),
         (fun () -> Seq.fold (elsftForJob lastJob T firstSuccStartingTime) Map.empty revTopOrdering))

    let ests = computeEsts ()
    let efts = ft ests

    let lfts = computeLfts ()
    let lsts = st lfts

    let deadline = lfts.[lastJob]

    let isFinishedAtEndOfPeriod (sts:IntMap) t j = sts.ContainsKey j && ft sts j <= t
    let arePredsFinished sts j t = preds j |> Set.forall (isFinishedAtEndOfPeriod sts t)

    let isActiveInPeriod (sts:IntMap) t j = sts.[j] < t && t <= ft sts j        
    let activeInPeriodSet sts t = keys sts |> Seq.filter (isActiveInPeriod sts t)

    let demandInPeriod sts r t = activeInPeriodSet sts t |> Seq.sumBy (fun j -> demands j r)

    let residualCapacity sts r t = capacities r - demandInPeriod sts r t
        
    let enoughCapacityForJob z sts j stj =
        resources >< [stj+1..stj+durations j]
        |> Seq.forall (fun (r,t) -> residualCapacity sts r t + z r t >= demands j r)

    let predsAndCapacityFeasible z sts job t = arePredsFinished sts job t && enoughCapacityForJob z sts job t
    
    let ssgsCore z sts λ =
        let scheduleJob acc j =
            Map.add j (numsGeq (lastPredFinishingTime acc j) |> Seq.find (enoughCapacityForJob z acc j)) acc
        Seq.fold scheduleJob sts λ

    let ssgs z λ =
        ssgsCore z (Map.ofList [(Seq.head λ, 0)]) (Seq.skip 1 λ)
    
    let psgs z λ =
        let nextDecisionPoint sts running = running |> Seq.map (ft sts) |> Seq.min
        let running sts t = keys sts |> Seq.filter (fun j -> t <= ft sts j)
        
        let rec scheduleEligible sts rest t =
            let eligible = Seq.filter (fun j -> predsAndCapacityFeasible z sts j t) rest
            if Seq.isEmpty eligible then (sts,rest)
            else
                let j = eligible |> Seq.minBy (indexOf rest)
                scheduleEligible (Map.add j t sts) (without j rest) t

        let rec traverseDecisionPoints sts rest t =
            if List.isEmpty rest then sts
            else
                let nt = nextDecisionPoint sts (running sts t)
                let pair = scheduleEligible sts rest nt
                traverseDecisionPoints (fst pair) (snd pair) (inc nt)

        traverseDecisionPoints (Map.ofSeq [(Seq.head λ, 0)]) (List.ofSeq (Seq.skip 1 λ)) 0

    let makespan sts = ft sts lastJob

    let neededOvercapacityInPeriod sts r t = max 0 (-residualCapacity sts r t)
    let totalOvercapacityCosts sts =
        resources >< [0..makespan sts]
        |> Seq.sumBy (fun (r,t) -> kappa r * float (neededOvercapacityInPeriod sts r t))

    let zeroOc r t = 0
    let maxOc r t = zmax r

    let u =        
        let maxOcSchedule = ssgs maxOc topOrdering
        let minMakespanApprox = makespan maxOcSchedule 
        let maxMakespanApprox = makespan (ssgs zeroOc topOrdering)

        let minOcCosts = 0.0
        let maxOcCosts = totalOvercapacityCosts maxOcSchedule 

        let c = (maxOcCosts - minOcCosts) / float (maxMakespanApprox - minMakespanApprox + boolToInt (minMakespanApprox = maxMakespanApprox))
        fun t -> -c * float t + c * float maxMakespanApprox

    let revenue = u << makespan

    let profit sts = (revenue sts) - totalOvercapacityCosts sts

    member ps.ProfitForRemainingCapacity (sts: Map<int,int>, remainingRes: int[,]) =
        let rev = revenue sts
        let mutable tcosts = 0.0
        for res in 0..(Array2D.length1 remainingRes)-1 do
            for period in 0..(Array2D.length2 remainingRes)-1 do
                if remainingRes.[res,period] < 0 then
                    tcosts <- tcosts + (-(float remainingRes.[res,period]) * (kappa (res+1)))
        rev - tcosts

    member ps.Revenue = revenue
    member ps.Profit = profit    

    member ps.ScheduleToGrid sts r =
        let nrows = capacities r + zmax r
        let grid = Array2D.zeroCreate nrows T
        for t in horizon do
            let actJobs = activeInPeriodSet sts t
            let mutable colCtr = dec nrows
            for j in actJobs do
                for k in 1..demands j r do
                    Array2D.set grid colCtr (dec t) j
                    colCtr <- dec colCtr
        grid

    member ps.FinishingTimesToStartingTimes fts =
        Map.map (fun j ftj -> ftj - durations j) fts

    member ps.CalculateGap optimalSts sts =
        gap (profit optimalSts) (profit sts)

    member ps.ScheduleFeasibleWithMaxOC sts =
        resources >< [0..makespan sts]
        |> Seq.forall (fun (r,t) -> neededOvercapacityInPeriod sts r t <= zmax r)
            
    member ps.Jobs = jobs
    member ps.LastJob = lastJob
    member ps.ActualJobs = actualJobs
    member ps.Durations = durations
    member ps.Demands = demands
    member ps.Capacities = capacities
    member ps.Preds = preds
    member ps.Succs = succs
    member ps.TransPreds = transPreds
    member ps.TransSuccs = transSuccs
    member ps.Resources = resources    
    member ps.Kappa = kappa
    member ps.U = u
    member ps.ZMax = zmax
    member ps.TimeHorizon with get () = horizon
    member ps.TimeHorizon with set (value) = horizon <- value

    member ps.EarliestStartingTimes = mapToFunc ests
    member ps.LatestStartingTimes = lsts
    member ps.EarliestFinishingTimes = efts
    member ps.LatestFinishingTimes = mapToFunc lfts
    member ps.Deadline = deadline

    member ps.EnoughCapacityForJob = enoughCapacityForJob
    member ps.LastPredFinishingTime = lastPredFinishingTime

    member ps.Makespan = makespan
    member ps.TotalOvercapacityCosts = totalOvercapacityCosts

    member ps.SerialScheduleGenerationScheme () = ssgs zeroOc topOrdering
    member ps.SerialScheduleGenerationSchemeWithOC = ssgs     

    member ps.SerialScheduleGenerationSchemeCore = ssgsCore

    member ps.ParallelScheduleGenerationScheme () = psgs zeroOc topOrdering

    member ps.NeededOCForSchedule sts = neededOvercapacityInPeriod sts

    member ps.DemandInPeriod = demandInPeriod

    member ps.ArePredsFinished = arePredsFinished

    static member Create(jobs, durations, demands, capacities, preds, resources, kappa, zmax) =
        let arrayToBaseOneMap arr = Array.mapi (fun ix e -> (inc ix,e)) arr |> Map.ofSeq
        ProjectStructure(jobs, arrayToBaseOneMap durations |> mapToFunc,
                         demands |> Array.map arrayToBaseOneMap |> arrayToBaseOneMap |> map2DToFunc,
                         preds, resources, capacities |> arrayToBaseOneMap |> mapToFunc, kappa, zmax)