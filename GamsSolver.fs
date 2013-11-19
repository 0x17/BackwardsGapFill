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
            addSetEntries tset "t" ps.TimeHorizon
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
            addParamEntries costsParam "j" ps.Jobs ps.Costs
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
        let z = outdb.GetVariable("z")
        let x = outdb.GetVariable("x")
        ()

    let solve (ps:ProjectStructure) =
        let ws = new GAMSWorkspace(workingDirectory="../../", debug=DebugLevel.KeepFiles)
        let opt = ws.AddOptions()
        opt.License <- "C:\GAMS\gamslice_Kurs_Nov13.txt"        
        let job = ws.AddJobFromFile("model.gms")
        let db = createDatabase ws ps
        //opt.Defines.Add("gdxincname", db.Name)
        //opt.AllModelTypes <- "xpress"
        job.Run(opt, db)
        opt.Dispose()
        processOutput job.OutDB