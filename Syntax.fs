namespace Mutton.Syntax

open Firethorn.Red

type public SyntaxKind =
    | ERROR = -1

    // Token Kinds
    | SYM = 1
    | OPEN = 2
    | CLOSE = 3
    | NUM = 4
    | SPACE = 5
    | EOF = 6

    // Node kinds
    | FORM = 10
    | ATOM = 11

    | PROGRAM = 100

module SyntaxKinds =

    let astToGreen (kind: SyntaxKind) = Firethorn.SyntaxKind(int kind)

    let greenToAst =
        function
        | Firethorn.SyntaxKind n -> enum<SyntaxKind> n


[<AbstractClass>]
type public Expression internal () =

    static member FromRaw(node: SyntaxNode) =
        match node.Kind |> SyntaxKinds.greenToAst with
        | SyntaxKind.FORM -> Some(new Form(node) :> Expression)
        | SyntaxKind.ATOM -> Some(new Atom(node))
        | _ -> None

and Form internal (red: SyntaxNode) =
    inherit Expression()

    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

and Atom internal (red: SyntaxNode) =
    inherit Expression()
