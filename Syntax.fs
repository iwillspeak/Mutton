namespace Mutton.Syntax

open Firethorn.Red
open System.Text
open Firethorn

/// Kind Enumeration for Syntax Items
///
/// This represents the distinct kinds of nodes and tokens available in the
/// "green" tree.
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
    | CONST = 11
    | IDENT = 12

    | PROGRAM = 100

/// Utility Methods for working with `SyntaxKind`s
module SyntaxKinds =

    /// Convert a Mutton kind into a raw Firethorn kind
    let astToGreen (kind: SyntaxKind) = Firethorn.SyntaxKind(int kind)

    /// Convert a raw Firethorn kind into a Mutton kind
    let greenToAst =
        function
        | Firethorn.SyntaxKind n -> enum<SyntaxKind> n

/// Base class for all syntax noddes
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

/// Expression node. Either a `Form` or an `Atom`
[<AbstractClass>]
type public Expression internal (red: SyntaxNode) =
    inherit Node(red)

    static member FromRaw(node: SyntaxNode) =
        match node.Kind |> SyntaxKinds.greenToAst with
        | SyntaxKind.FORM -> Some(new Form(node) :> Expression)
        | SyntaxKind.IDENT -> Some(new Symbol(node))
        | SyntaxKind.CONST -> Some(new Literal(node))
        | _ -> None

/// A form value. Represents a parenthesied syntax form.
and Form internal (red: SyntaxNode) =
    inherit Expression(red)

    /// The body syntax for the form.
    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

/// A syntax atom. Represents a single value being used as an expression.
and Literal internal (red: SyntaxNode) =
    inherit Expression(red)

    member _.Value =
        red.ChildrenWithTokens()
        |> Seq.choose NodeOrToken.asToken
        |> Seq.tryPick (fun tok ->
            if tok.Kind |> SyntaxKinds.greenToAst = SyntaxKind.NUM then
                Some(int tok.Green.Text)
            else
                None)

/// A single symbol literal in the source text. We keep these separate from
/// other literal kinds to simplify expansion and binding later
and Symbol internal (red: SyntaxNode) =
    inherit Expression(red)

    member _.Value =
        red.ChildrenWithTokens()
        |> Seq.choose (NodeOrToken.asToken)
        |> Seq.tryPick (fun tok ->
            if tok.Kind |> SyntaxKinds.greenToAst = SyntaxKind.SYM then
                Some(tok.Green.Text)
            else
                None)

/// Program node. Represents a sequence of expressions. This is the root of
/// the syntax tree.
and Program internal (red: SyntaxNode) =
    inherit Node(red)

    /// The sequence of expressions within the program body.
    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

/// Utility patterns to make `match`ing over sytnax trees more ergonomic.
[<AutoOpen>]
module Patterns =

    let (|Form|Literal|Symbol|) (exp: Expression) =
        match exp with
        | :? Form as f -> Form f
        | :? Literal as a -> Literal a
        | :? Symbol as s -> Symbol s
        | _ -> failwithf "Unrecognised expression %A" exp
