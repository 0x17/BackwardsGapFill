namespace RCPSP

open System
open System.Diagnostics

module System =
    type RunBehavior =
        | Blocking
        | NonBlocking

    let runCmd behavior cmd args =
        let psi = ProcessStartInfo(cmd)
        psi.Arguments <- args
        let p = Process.Start psi
        match behavior with
        | Blocking -> p.WaitForExit ()
        | NonBlocking -> ()

    let onWindows = Environment.OSVersion.Platform = PlatformID.Win32NT

    let (stopwatchStart, stopwatchStop) =
        let sw = Stopwatch ()
        ((fun () -> sw.Reset (); sw.Start ()),
         (fun () -> sw.Stop(); sw.Elapsed))

    let bypassAndPrint x =
        printf "%O\n" x
        x