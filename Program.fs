namespace Mutton

module Main =
    open System
    open Firethorn.Red
    open Mutton.Syntax
    open Parse

    let step ch step input =
        let output = step input
        printfn "%s~> %A" ch output
        output

    let private run input =
        match Parse.parse input with
        | { Diagnostics = []; Tree = tr } ->
            Debug.debugDump (Debug.mappedFormatter SyntaxKinds.greenToAst) tr.Syntax

            let stx, scopes = Illuminate.illum tr
            printfn "*~> %A" stx

            let expanded = Expand.expand scopes stx
            printfn "$~> %A" expanded

            let bound = Binder.bind scopes expanded
            printfn "%%~> %A" bound

            ()
        | { Diagnostics = diags } ->
            for diag in diags do
                match diag with
                | Diagnostic message -> eprintfn "!! %s" message

    [<EntryPoint>]
    let main args =
        if Console.IsInputRedirected then
            Console.In.ReadToEnd() |> run
        else
            let rec repl (prompt: string) =
                Console.Write(prompt)
                Console.Out.Flush()
                let line = Console.ReadLine()

                match line with
                | null -> () // EOF
                | line ->
                    run line
                    repl prompt

            repl "> "

        0
