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
    | ATOM = 11

    | PROGRAM = 100

/// Utility Methods for working with `SyntaxKind`s
module SyntaxKinds =

    /// Convert a Mutton kind into a raw Firethorn kind
    let astToGreen (kind: SyntaxKind) = Firethorn.SyntaxKind(int kind)

    /// Convert a raw Firethorn kind into a Mutton kind
    let greenToAst =
        function
        | Firethorn.SyntaxKind n -> enum<SyntaxKind> n

/// Base Class for atom values. These are are the syntax tokens that contain
/// the actual literals within a given `Atom` node.
[<AbstractClass>]
type public Value internal (red: SyntaxToken) =

    member _.Syntax = red

    override _.ToString() = red.Green.Text

    static member FromRaw(node: SyntaxToken) =
        match node.Kind |> SyntaxKinds.greenToAst with
        | SyntaxKind.NUM -> Some(new NumberValue(node) :> Value)
        | SyntaxKind.SYM -> Some(new SymValue(node))
        | _ -> None

/// An atom value representing a single numeric value
and NumberValue internal (red: SyntaxToken) =

    inherit Value(red)

    member _.Inner = red.Green.Text |> int

/// An atom value representing a single symbol reference
and SymValue internal (red: SyntaxToken) =

    inherit Value(red)

    member _.Identifier = red.Green.Text


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
        | SyntaxKind.ATOM -> Some(new Atom(node))
        | _ -> None

/// A form value. Represents a parenthesied syntax form.
and Form internal (red: SyntaxNode) =
    inherit Expression(red)

    /// The body syntax for the form.
    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

/// A syntax atom. Represents a single value being used as an expression.
and Atom internal (red: SyntaxNode) =
    inherit Expression(red)

    member _.Value =
        red.ChildrenWithTokens()
        |> Seq.choose NodeOrToken.asToken
        |> Seq.tryExactlyOne
        |> Option.bind Value.FromRaw

/// Program node. Represents a sequence of expressions. This is the root of
/// the syntax tree.
and Program internal (red: SyntaxNode) =
    inherit Node(red)

    /// The sequence of expressions within the program body.
    member _.Body = red.Children() |> Seq.choose (Expression.FromRaw)

/// Utility patterns to make `match`ing over sytnax trees more ergonomic.
[<AutoOpen>]
module Patterns =

    let (|Form|Atom|) (exp: Expression) =
        match exp with
        | :? Form as f -> Form f
        | :? Atom as a -> Atom a
        | _ -> failwithf "Unrecognised expression %A" exp

    let (|NumberValue|SymValue|) (v: Value) =
        match v with
        | :? NumberValue as n -> NumberValue n
        | :? SymValue as s -> SymValue s
        | _ -> failwithf "Unrecognised value %A" v
