namespace Mutton.Syntax

open Firethorn.Red
open System.Text

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
type public Node internal (red: SyntaxNode) =

    member _.Syntax = red

    override _.ToString() =
        Walk.walk red
        |> Seq.fold
            (fun (sb: StringBuilder) ev ->
                match ev with
                | OnToken t -> sb.Append(t.Green.Text)
                | _ -> sb)
            (new StringBuilder())
        |> (_.ToString())

[<AbstractClass>]
type public Expression internal (red: SyntaxNode) =
    inherit Node(red)

    static member FromRaw(node: SyntaxNode) =
        match node.Kind |> SyntaxKinds.greenToAst with
        | SyntaxKind.FORM -> Some(new Form(node) :> Expression)
        | SyntaxKind.ATOM -> Some(new Atom(node))
        | _ -> None

and Form internal (red: SyntaxNode) =
    inherit Expression(red)

    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

and Atom internal (red: SyntaxNode) =
    inherit Expression(red)

and Program internal (red: SyntaxNode) =
    inherit Node(red)

    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

[<AutoOpen>]
module Patterns =

    let (|Form|Atom|) (exp: Expression) =
        match exp with
        | :? Form as f -> Form f
        | :? Atom as a -> Atom a
        | _ -> failwithf "Unrecognised expression %A" exp
