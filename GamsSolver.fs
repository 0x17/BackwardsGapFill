namespace RCPSP

open GAMS

module GamsSolver =
    let addSetEntries (set:GAMSSet) name entries =
        Seq.iter (fun e -> set.AddRecord(name+string(e)) |> ignore) entries

    let addParamEntries (param:GAMSParameter) name entries mapping =
        Seq.iter (fun e -> param.AddRecord(name+string(e)).Value <- float(mapping(e))) entries

    let createDatabase (ws:GAMSWorkspace) (ps:ProjectStructure) =
        let db = ws.AddDatabase("ProjectStructureData")

        let addSets() =
            let jset = db.AddSet("j", 1, "Arbeitsgänge")
            addSetEntries jset "j" ps.Jobs
            let tset = db.AddSet("t", 1, "Perioden")
            addSetEntries tset "t" (0 :: ps.TimeHorizon)
            let rset = db.AddSet("r", 1, "Ressourcen")
            addSetEntries rset "r" ps.Resources
            let predSet = db.AddSet("pred", 2, "yes gdw. i Vorgänger von j ist")
            for j in ps.Jobs do
                for i in ps.Preds j do
                    predSet.AddRecord("j"+string(i), "j"+string(j)) |> ignore

        let addParams() =
            let zMaxParam = db.AddParameter("zmax", 1, "Maximale ZK von r")
            addParamEntries zMaxParam "r" ps.Resources ps.ZMax
            let kappaParam = db.AddParameter("kappa", 1, "Kosten pro Einheit ZK")
            addParamEntries kappaParam "r" ps.Resources ps.Kappa
            let capacitiesParam = db.AddParameter("capacities", 1, "Kapazitäten")
            addParamEntries capacitiesParam "r" ps.Resources ps.Capacities
            let durationsParam = db.AddParameter("durations", 1, "Dauern")
            addParamEntries durationsParam "j" ps.Jobs ps.Durations
            let costsParam = db.AddParameter("costs", 1, "Kosten")
            addParamEntries costsParam "j" ps.ActualJobs ps.Costs
            let ustarParam = db.AddParameter("ustar", 1, "Erlös bei Makespan t")
            addParamEntries ustarParam "t" ps.TimeHorizon ps.UStar

            let demandsParam = db.AddParameter("demands", 2, "Bedarf")
            Utils.cartesianProduct ps.Jobs ps.Resources
            |> Seq.iter (fun (j,r) -> demandsParam.AddRecord("j"+string(j), "r"+string(r)).Value <- float(ps.Demands j r))

            let eftsParam = db.AddParameter("efts", 1, "Früheste Startzeitpunkte")
            addParamEntries eftsParam "j" ps.Jobs ps.EarliestFinishingTimes
            let lftsParam = db.AddParameter("lfts", 1, "Späteste Endzeitpunkte")
            addParamEntries lftsParam "j" ps.Jobs (fun j -> ps.LatestFinishingTimes.[j])

        addSets()
        addParams()
        db

    let processOutput (outdb:GAMSDatabase) =
        let parseKey (str:string) = System.Int32.Parse(str.Substring(1))

        let zvar = outdb.GetVariable("z")
        let z = new System.Collections.Generic.Dictionary<int*int, int>()
        for vrecsym in zvar do
            let vrec : GAMSVariableRecord = downcast vrecsym
            let r = parseKey vrec.Keys.[0]
            let t = parseKey vrec.Keys.[1]
            z.Add((r, t), int(vrec.Level))

        let xvar = outdb.GetVariable("x")
        let fts = new IntMap()        
        for vrecsym in xvar do
            let vrec : GAMSVariableRecord = downcast vrecsym
            if vrec.Level = 1.0 then
                let j = parseKey vrec.Keys.[0]
                let t = parseKey vrec.Keys.[1]
                fts.Add(j, t)

        (z, fts)

    let solve (ps:ProjectStructure) =
        let ws = new GAMSWorkspace(workingDirectory="../../", debug=DebugLevel.Off)
        let opt = ws.AddOptions()
        opt.License <- "C:\GAMS\gamslice_Kurs_Nov13.txt"
        opt.MIP <- "GUROBI"
        opt.OptCR <- 0.0001
        let job = ws.AddJobFromFile("model.gms")
        let db = createDatabase ws ps
        //let writer = new System.IO.StringWriter() // forward string writer to stdout
        let writer = System.Console.Out
        job.Run(opt, writer, db)
        // Parse elapsed time from string writer content
        opt.Dispose()
        let (z, fts) = processOutput job.OutDB
        ((fun r t -> z.[(r,t)]), ps.FinishingTimesToStartingTimes fts)
