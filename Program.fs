namespace Mutton

module Main =
    open System
    open Firethorn.Red
    open Mutton.Syntax

    [<EntryPoint>]
    let main args =
        let rec repl (prompt: string) =
            Console.Write(prompt)
            Console.Out.Flush()
            let line = Console.ReadLine()
            let parsed = Parse.parse line
            printfn ">> %A" parsed
            Debug.debugDump (Debug.mappedFormatter SyntaxKinds.greenToAst) parsed.Tree

            repl prompt

        repl "> "
