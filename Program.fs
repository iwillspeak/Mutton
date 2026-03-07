namespace Mutton

module Main =
    open System
    open Firethorn.Red
    open Mutton.Syntax
    open Parse

    let private run (prog: Program) =

        prog.Syntax
        |> Debug.debugDump (Debug.mappedFormatter SyntaxKinds.greenToAst)

        let stx, scopes = Illuminate.illum prog
        printfn "*~> %A" stx

        let bound = Binder.bind scopes stx
        printfn "%%~> %A" bound

        ()

    let private parse_and_run (input: string) =
        match Parse.parse input with
        | { Diagnostics = []; Tree = tr } ->
            run tr
        | { Diagnostics = diags } ->
            for diag in diags do
                match diag with
                | Diagnostic message -> eprintfn "!! %s" message

    [<EntryPoint>]
    let main args =
        if Console.IsInputRedirected then
            Console.In.ReadToEnd() |> parse_and_run
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
                        run tr
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
