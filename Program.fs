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

    [<EntryPoint>]
    let main args =
        let rec repl (prompt: string) =
            Console.Write(prompt)
            Console.Out.Flush()
            let line = Console.ReadLine()

            match Parse.parse line with
            | { Diagnostics = []; Tree = tr } ->
                Debug.debugDump (Debug.mappedFormatter SyntaxKinds.greenToAst) tr.Syntax

                tr
                |> step "*" (Illuminate.illum)
                |> step "$" (Expand.expand)
                |> step "%" (Binder.bind)
                |> ignore
            | { Diagnostics = diags } ->
                for diag in diags do
                    match diag with
                    | Diagnostic message -> eprintfn "!! %s" message

            repl prompt

        repl "> "
