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

            let demandsParam = db.AddParameter ("demands", 2, "Bedarf")
            ps.Jobs >< ps.Resources
            |> Seq.iter (fun (j,r) -> demandsParam.AddRecord("j"+string(j), "r"+string(r)).Value <- float (ps.Demands j r))

            let eftsParam = db.AddParameter ("efts", 1, "Früheste Startzeitpunkte")
            addParamEntriesToF eftsParam "j" ps.Jobs ps.EarliestFinishingTimes
            let lftsParam = db.AddParameter ("lfts", 1, "Späteste Endzeitpunkte")
            addParamEntriesToF lftsParam "j" ps.Jobs ps.LatestFinishingTimes

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

    let solve ps =
        let ws = GAMSWorkspace (workingDirectory=".", debug=DebugLevel.Off)
        let opt = ws.AddOptions ()
        opt.License <- @"C:\GAMS\gamslice.txt"
        opt.MIP <- "GUROBI"
        opt.OptCR <- 0.0
        opt.ResLim <- 3600.0//System.Double.MaxValue 
        opt.Threads <- 8
        let job = ws.AddJobFromFile "model.gms"
        let db = createDatabase ws ps
        let writer = System.Console.Out
        job.Run (opt, writer, db)
        opt.Dispose ()
        let (fts, solveTime, solveStat) = processOutput job.OutDB
        if solveStat <> 1.0 then
            raise (SolveError(solveStat))
        (ps.FinishingTimesToStartingTimes fts, solveTime)
