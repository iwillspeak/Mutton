module Mutton.Parse

open Lex
open Firethorn.Green
open Firethorn.Red
open Mutton.Syntax

/// Parse Diagnostics
///
/// Holds a single parse related error message
type public Diagnostic = Diagnostic of string

/// Parse Response
///
/// Returned form the `parse` method. Holds the parsed syntax tree along
/// with any diagnostics.
type public ParseResponse<'a> =
    { Tree: 'a
      Diagnostics: Diagnostic list }

type private State =
    { Diags: Diagnostic list
      Tokens: (string * TokenKind) list }

[<AutoOpen>]
module private State =

    let public bufferDiag diag state =
        { state with
            Diags = diag :: state.Diags }

    let public peek state = List.tryHead state.Tokens

    let public bump state =
        (List.head state.Tokens,
         { state with
             Tokens = List.tail state.Tokens })

    let public currentKind state =
        peek state |> Option.map (fun (_, kind) -> kind) |> Option.defaultValue EOF

    let public eat (builder: GreenNodeBuilder) kind state =
        let (lexeme, _), state = bump state
        builder.Token(kind |> SyntaxKinds.astToGreen, lexeme)
        state

    let public expect (builder: GreenNodeBuilder) tokenKind syntaxKind state =
        if currentKind state = tokenKind then
            eat builder syntaxKind state
        else
            bufferDiag (sprintf "Expected %A" tokenKind |> Diagnostic) state

    let rec public skipWs (builder: GreenNodeBuilder) state =
        if currentKind state = TokenKind.Space then
            eat builder SyntaxKind.SPACE state |> skipWs builder
        else
            state

let private parseAtom (builder: GreenNodeBuilder) state =
    builder.StartNode(SyntaxKind.ATOM |> SyntaxKinds.astToGreen)

    let state =
        match currentKind state with
        | TokenKind.Sym -> state |> eat builder SyntaxKind.SYM
        | TokenKind.Number -> state |> eat builder SyntaxKind.NUM
        | kind ->
            state
            |> bufferDiag (Diagnostic(sprintf "Unexpected token %A, expecting atom" kind))
            |> eat builder SyntaxKind.ERROR

    builder.FinishNode()
    state

let rec private parseExpression (builder: GreenNodeBuilder) state =
    let state = skipWs builder state

    if currentKind state = TokenKind.OpenParen then
        parseForm builder state
    else
        parseAtom builder state
    |> skipWs builder

and private parseForm (builder: GreenNodeBuilder) state =

    builder.StartNode(SyntaxKind.FORM |> SyntaxKinds.astToGreen)
    let mutable state = eat builder SyntaxKind.OPEN state

    while not <| List.contains (currentKind state) [ TokenKind.EOF; TokenKind.CloseParen ] do
        state <- parseExpression builder state

    state <- expect builder TokenKind.CloseParen SyntaxKind.CLOSE state

    builder.FinishNode()
    state

let rec private parseProgram (builder: GreenNodeBuilder) state =
    if currentKind state <> TokenKind.EOF then
        parseExpression builder state |> parseProgram builder
    else
        state

let parse input =
    let state =
        { Diags = []
          Tokens = Lex.lex input |> List.ofSeq }

    let tree = new GreenNodeBuilder()

    let state = parseProgram tree state

    { Tree =
        tree.BuildRoot(SyntaxKind.PROGRAM |> SyntaxKinds.astToGreen)
        |> SyntaxNode.CreateRoot
      Diagnostics = state.Diags }
