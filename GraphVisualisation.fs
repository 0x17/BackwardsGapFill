namespace RCPSP

open System.Text
open System.Collections.Generic
open System.Diagnostics

open Utils

module GraphVisualisation =
    let predsToEdgeRelation jobs preds =
        jobs |> Seq.collect (fun j -> preds(j) |> Seq.map (fun i -> (i,j)))

    let precedenceGraphToDot (ps:ProjectStructure) filename =
        let header = "digraph precedence {"
        let footer = "}"
        let edgeRelation = predsToEdgeRelation ps.Jobs ps.Preds
        let sb = new StringBuilder()
        for (i,j) in edgeRelation do
            sb.Append(string(i)+"->"+string(j)+"\n") |> ignore
        for j in ps.Jobs do
            let durStr = string(ps.Durations j)
            let resStr = String.concat "," (ps.Resources |> Seq.map (string << (ps.Demands j)))
            sb.Append(string(j)+"[label=\"(" + durStr + ") "+ string(j) + " (" + resStr + ")\"];") |> ignore
        spit (filename+".dot") (header + sb.ToString () + footer)

    let runDotOnFile filename =
        let dotPath = @"C:\Program Files (x86)\Graphviz2.34\bin\dot.exe"
        runCmd Blocking dotPath ("-Tpdf " + filename + ".dot -o " + filename + ".pdf")

    let visualizePrecedenceGraph (ps:ProjectStructure) filename = 
        let viewerPath = @"C:\Program Files (x86)\SumatraPDF\SumatraPDF.exe"
        precedenceGraphToDot ps filename
        runDotOnFile filename
        runCmd NonBlocking viewerPath (filename+".pdf")