namespace RCPSP

open Runners
open Serialization

module VisRunners =
    let showRevenuePlot () =
        let ps = testProjectStructure ()
        PlotVisualisation.generatePlot ps.U ps.TimeHorizon "revenue"

    let scheduleVisTool (argv: string[]) =
        let ps = PSPLibParser.parse argv.[0]
        let sts = slurpMap argv.[1]
        GraphVisualisation.visualizePrecedenceGraph ps argv.[0]
        ScheduleVisualisation.showSchedules [("Schedule", ps, sts)]

    let solveAndVisualize () =
        let ps = testProjectStructure ()

        GraphVisualisation.visualizePrecedenceGraph (testProjectStructure ()) @"Modellendogen001"

        let optSchedFn = testFilename + ".OPTSCHED"

        let (sts1, solveTime, solveStat) = GamsSolver.solve ps        
        spitMap optSchedFn sts1

        ScheduleVisualisation.showSchedules [("MIP Modell", ps, sts1)]
        ()