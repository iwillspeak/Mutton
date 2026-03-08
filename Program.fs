namespace Mutton

module Main =
    open System
    open Firethorn.Red
    open Mutton.Syntax
    open Parse

    type DebugOutput =
        {
            showSyntax: bool
            showIlluminated: bool
            showBound: bool
        }

    let private defaultDebugOutput =
        {
            showSyntax = false
            showIlluminated = false
            showBound = true
        }

    let private parseDebugArgs (args: string[]) =
        let mutable output = defaultDebugOutput
        let mutable inputArgs = []

        for arg in args do
            match arg with
            | "--show-syntax" ->
                output <- { output with showSyntax = true }
            | "--show-illuminated" ->
                output <- { output with showIlluminated = true }
            | "--show-bound" ->
                output <- { output with showBound = true }
            | "--show-all" ->
                output <-
                    {
                        showSyntax = true
                        showIlluminated = true
                        showBound = true
                    }
            | arg when arg.StartsWith("--") ->
                eprintfn "Unknown option: %s" arg
            | _ -> inputArgs <- inputArgs @ [arg]

        output, inputArgs

    let private run (debugOutput: DebugOutput) (prog: Program) =

        if debugOutput.showSyntax then
            prog.Syntax
            |> Debug.debugDump (Debug.mappedFormatter SyntaxKinds.greenToAst)

        let stx = Illuminate.illum prog
        if debugOutput.showIlluminated then
            printfn "*~> %A" stx

        let bound = Binder.bind stx
        if debugOutput.showBound then
            printfn "%%~> %A" bound

        ()

    let private parse_and_run (debugOutput: DebugOutput) (input: string) =
        match Parse.parse input with
        | { Diagnostics = []; Tree = tr } ->
            run debugOutput tr
        | { Diagnostics = diags } ->
            for diag in diags do
                match diag with
                | Diagnostic message -> eprintfn "!! %s" message

    [<EntryPoint>]
    let main args =
        let debugOutput, _ = parseDebugArgs args

        if Console.IsInputRedirected then
            Console.In.ReadToEnd() |> parse_and_run debugOutput
        else
            let rec repl (prompt: string) (prog_so_far: string) =
                Console.Write(prompt)
                Console.Out.Flush()
                let line = Console.ReadLine()

                match line with
                | null -> () // EOF
                | line ->
                    let prog_so_far = prog_so_far + "\n" + line
                    match Parse.parse prog_so_far with
                    | { Diagnostics = []; Tree = tr } ->
                        run debugOutput tr
                        repl "> " ""
                    | { Diagnostics = diags } ->
                        if prog_so_far.EndsWith("\n\n") then
                            for diag in diags do
                                match diag with
                                | Diagnostic message -> eprintfn "!! %s" message
                            repl prompt ""
                        else
                            repl "| " prog_so_far

            repl "> " ""
        
        0
