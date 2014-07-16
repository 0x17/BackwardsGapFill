namespace RCPSP

open GAMS

open Utils

module GamsSolver =
    let addSetEntries (set:GAMSSet) name entries =
        Seq.iter (fun e -> set.AddRecord(name+string(e)) |> ignore) entries

    let addParamEntries (param:GAMSParameter) name entries mapping =
        Seq.iter (fun e -> param.AddRecord(name+string(e)).Value <- mapping(e)) entries

    let addParamEntriesToF (param:GAMSParameter) name entries (mapping:int->int) =
        addParamEntries param name entries (float << mapping)

    let createDatabase (ws:GAMSWorkspace) (ps:ProjectStructure) =
        let db = ws.AddDatabase "ProjectStructureData"

        let addSets () =
            let jset = db.AddSet ("j", 1, "Arbeitsgänge")
            addSetEntries jset "j" ps.Jobs
            let tset = db.AddSet ("t", 1, "Perioden")
            addSetEntries tset "t" (0 :: ps.TimeHorizon)
            let rset = db.AddSet ("r", 1, "Ressourcen")
            addSetEntries rset "r" ps.Resources
            let predSet = db.AddSet ("pred", 2, "yes gdw. i Vorgänger von j ist")
            for j in ps.Jobs do
                for i in ps.Preds j do
                    predSet.AddRecord ("j"+string(i), "j"+string(j)) |> ignore

        let addParams () =
            let zMaxParam = db.AddParameter ("zmax", 1, "Maximale ZK von r")
            addParamEntriesToF zMaxParam "r" ps.Resources ps.ZMax
            let kappaParam = db.AddParameter ("kappa", 1, "Kosten pro Einheit ZK")
            addParamEntries kappaParam "r" ps.Resources ps.Kappa
            let capacitiesParam = db.AddParameter ("capacities", 1, "Kapazitäten")
            addParamEntriesToF capacitiesParam "r" ps.Resources ps.Capacities
            let durationsParam = db.AddParameter ("durations", 1, "Dauern")
            addParamEntriesToF durationsParam "j" ps.Jobs ps.Durations
            let uParam = db.AddParameter ("u", 1, "Erlös bei Makespan t")
            addParamEntries uParam "t" ps.TimeHorizon ps.U
            let u2Param = db.AddParameter ("u2", 1, "ErlösCaro bei Makespan t")
            addParamEntries u2Param "t" ps.TimeHorizon ps.U2

            let demandsParam = db.AddParameter ("demands", 2, "Bedarf")
            ps.Jobs >< ps.Resources
            |> Seq.iter (fun (j,r) -> demandsParam.AddRecord("j"+string(j), "r"+string(r)).Value <- float (ps.Demands j r))

            let eftsParam = db.AddParameter ("efts", 1, "Früheste Startzeitpunkte")
            addParamEntriesToF eftsParam "j" ps.Jobs ps.EarliestFinishingTimes
            let lftsParam = db.AddParameter ("lfts", 1, "Späteste Endzeitpunkte")
            addParamEntriesToF lftsParam "j" ps.Jobs ps.LatestFinishingTimes

            let dparam = db.AddParameter("deadline", 0)
            dparam.AddRecord().Value <- -1.0

        addSets ()
        addParams ()
        db

    let processOutput (outdb:GAMSDatabase) =
        let xvar = outdb.GetVariable "x"
        let zvar = outdb.GetVariable "z"

        let parseKey (str:string) = System.Int32.Parse (str.Substring 1)
        let symToRec (vrecsym:GAMSSymbolRecord) : GAMSVariableRecord = downcast vrecsym

        let zRecords = [for vrecsym in zvar do yield symToRec vrecsym]
        let addZEntry acc (vrec:GAMSVariableRecord) =
            Map.add ((parseKey vrec.Keys.[0]),(parseKey vrec.Keys.[1])) (int vrec.Level) acc
        let z = Seq.fold addZEntry Map.empty zRecords 
        let zFunc = (fun r t -> z.[(r,t)])
        
        let relevantXRecords = [for vrecsym in xvar do if (symToRec vrecsym).Level = 1.0 then yield symToRec vrecsym]
        let addFinishTimeEntry acc (vrec:GAMSVariableRecord) =
            Map.add (parseKey vrec.Keys.[0]) (parseKey vrec.Keys.[1]) acc
        let fts = Seq.fold addFinishTimeEntry Map.empty relevantXRecords

        let parval (param:GAMSParameter) = param.FirstRecord().Value
        let solveTime = outdb.GetParameter "solveTime" |> parval
        let solveStat = outdb.GetParameter "slvStat" |> parval

        (fts, solveTime, solveStat)

    let optTopSort jobs (optSchedule:IntMap) =
        jobs |> Seq.sortBy (fun j -> optSchedule.[j]) |> Seq.toList

    exception SolveError of float

    let solveCommon ps incfile (timeout:Option<float>) (additionalData:Option<GAMSDatabase->unit>) =
        let ws = GAMSWorkspace (workingDirectory=".", debug=DebugLevel.Off)
        let opt = ws.AddOptions ()
        opt.License <- @"C:\GAMS\gamslice.txt"
        opt.MIP <- "GUROBI"
        opt.OptCR <- 0.0
        opt.ResLim <- if timeout.IsNone then System.Double.MaxValue else timeout.Value
        opt.Threads <- 8
        opt.Defines.Add("IncFile", incfile+".inc")
        let job = ws.AddJobFromFile "model.gms"
        let db = createDatabase ws ps
        if additionalData.IsSome then additionalData.Value db
        let writer = System.Console.Out
        try
            job.Run (opt, writer, db)
        with
            | :? GAMS.GAMSExceptionExecution ->
                raise (SolveError(-1.0))
        opt.Dispose ()
        (ws, job)

    let startingTimesForOutDB (ps:ProjectStructure) db =
        let (fts, solveTime, solveStat) = processOutput db
        if solveStat <> 1.0 then
            raise (SolveError(solveStat))
        (ps.FinishingTimesToStartingTimes fts, solveTime)

    let startingTimesForFinishedJob ps (job:GAMSJob) =
        startingTimesForOutDB ps job.OutDB

    let solve ps =
        let job = snd (solveCommon ps "rcpspoc" (Some 3600.0) None)
        startingTimesForFinishedJob ps job

    let solveVariants ps =
        let (ws, job) = solveCommon ps "rcpspvariants" (Some 3600.0) None
        ["results.gdx"; "results2.gdx"; "resultsmincost.gdx"; "resultsminms.gdx"]
        |> List.map (fun fn -> ws.AddDatabaseFromGDX(fn))
        |> List.map (fun db -> startingTimesForOutDB ps db |> fst |> ps.Makespan)

    let solveMinimalCostsWithDeadline ps deadline =
        let setDeadlineParam (db:GAMSDatabase) =
            let dparam = db.GetParameter("deadline")
            dparam.FirstRecord().Value <- float deadline
            db

        let zeroOutRevenue (db:GAMSDatabase) =
            let uparam = db.GetParameter("u")
            for t in 1..uparam.NumberRecords do
                uparam.FindRecord("t"+string(t)).Value <- 0.0
            db       

        let job = snd (solveCommon ps "rcpspdl" None (Some (ignore << zeroOutRevenue << setDeadlineParam)))
        ps.TotalOvercapacityCosts (startingTimesForFinishedJob ps job |> fst)

    let solveRCPSP (ps:ProjectStructure) =
        let ps2 = new ProjectStructure(ps.Jobs, ps.Durations, ps.Demands, ps.Preds, ps.Resources, ps.Capacities, (fun r -> 0.0), (fun r -> 0))
        let job = snd (solveCommon ps2 "rcpspoc" None None)        
        startingTimesForFinishedJob ps job

    let minMaxMakespan (ps:ProjectStructure) =
        let newcapsf r = ps.Capacities r + ps.ZMax r
        let ps2 = new ProjectStructure(ps.Jobs, ps.Durations, ps.Demands, ps.Preds, ps.Resources, newcapsf, ps.Kappa, ps.ZMax)
        let minMakespan = solveRCPSP ps2 |> fst |> ps.Makespan
        let maxMakespan = solveRCPSP ps |> fst |> ps.Makespan
        (minMakespan, maxMakespan)