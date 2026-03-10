#!/usr/bin/env dotnet-fsi
/// Snapshot test runner for Mutton spec files.
/// Runs each *.mut file and compares stdout against the adjacent *.out file.
open System
open System.IO
open System.Diagnostics

let specDir = __SOURCE_DIRECTORY__
let projectDir = Path.GetFullPath(Path.Combine(specDir, ".."))

let normalise (s: string) = s.Replace("\r\n", "\n").TrimEnd()

let runSpec (specFile: string) =
    let psi = ProcessStartInfo("dotnet", "run")
    psi.WorkingDirectory <- projectDir
    psi.RedirectStandardInput <- true
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    use p = Process.Start(psi)
    p.StandardInput.Write(File.ReadAllText(specFile))
    p.StandardInput.Close()
    let output = p.StandardOutput.ReadToEnd()
    p.WaitForExit()
    output

let testSpec (specFile: string) =
    let name = Path.GetFileNameWithoutExtension(specFile)
    let outFile = Path.ChangeExtension(specFile, ".out")

    if not (File.Exists(outFile)) then
        printfn "SKIP: %s (no .out file)" name
        true
    else
        let expected = normalise (File.ReadAllText(outFile))
        let actual = normalise (runSpec specFile)

        if expected = actual then
            printfn "PASS: %s" name
            true
        else
            printfn "FAIL: %s" name
            let expectedLines = expected.Split('\n')
            let actualLines = actual.Split('\n')
            let maxLen = max expectedLines.Length actualLines.Length

            for i in 0 .. maxLen - 1 do
                let e = if i < expectedLines.Length then expectedLines.[i] else "<missing>"
                let a = if i < actualLines.Length then actualLines.[i] else "<missing>"

                if e <> a then
                    printfn "  line %d expected: %s" (i + 1) e
                    printfn "  line %d actual:   %s" (i + 1) a

            false

let specs = Directory.GetFiles(specDir, "*.mut") |> Array.sort
let results = specs |> Array.map testSpec
let passed = results |> Array.filter id |> Array.length
let failed = results |> Array.filter (fun x -> not x) |> Array.length

printfn ""
printfn "%d passed, %d failed" passed failed

if failed > 0 then
    Environment.Exit(1)
