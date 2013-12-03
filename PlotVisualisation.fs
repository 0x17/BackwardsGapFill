namespace RCPSP

open System.Text
open Utils

module PlotVisualisation =
    // TODO: Ustar visualisieren
    let writePlotData (f:float->float) (domain:Set<float>) filename =
        let sb = new StringBuilder ()
        for x in domain do
            sb.Append(string(x)+" "+string(f(x))+"\n") |> ignore
        spit filename (sb.ToString () + ".dat")

    let showPlot filename =
        let gplotPath = @"C:\Program Files (x86)\gnuplot\bin\gnuplot.exe"
        runCmd Blocking gplotPath ("-e \"filename='" + filename + ".dat'\" ustar.plg")