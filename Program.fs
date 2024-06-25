namespace Mutton

module Main =
    open System
    open Firethorn.Red
    open Mutton.Syntax
    open Parse

    [<EntryPoint>]
    let main args =
        let rec repl (prompt: string) =
            Console.Write(prompt)
            Console.Out.Flush()
            let line = Console.ReadLine()

            match Parse.parse line with
            | { Diagnostics = []; Tree = tr } ->
                Debug.debugDump (Debug.mappedFormatter SyntaxKinds.greenToAst) tr.Syntax
                let il = Illuminate.illum tr
                printfn "&& %A" il
                printfn "** %A" (Binder.bind il)
            | { Diagnostics = diags } ->
                for diag in diags do
                    match diag with
                    | Diagnostic message -> eprintfn "!! %s" message

            repl prompt

        repl "> "
