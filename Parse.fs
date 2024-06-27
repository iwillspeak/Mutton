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

/// Internal parser state. Used to keep track of the current diagnostics we need
/// to emit, along with any unprocessed tokens.
type private State =
    { Diags: Diagnostic list
      Tokens: (string * TokenKind) list }

[<AutoOpen>]
module private State =

    /// Add a diagnostic to the given parser `state`
    let public bufferDiag diag state =
        { state with
            Diags = diag :: state.Diags }

    /// Inspect the next token, if any.

    let public peek state = List.tryHead state.Tokens

    /// Accept the next token. Returns both the token _and_ updated parse state
    let public bump state =
        (List.head state.Tokens,
         { state with
             Tokens = List.tail state.Tokens })

    /// Peek at just the kind of the current token.
    let public currentKind state =
        peek state |> Option.map (fun (_, kind) -> kind) |> Option.defaultValue EOF

    /// Consume the current token as a new node in `builder` of `kind`
    let public eat (builder: GreenNodeBuilder) kind state =
        let (lexeme, _), state = bump state
        builder.Token(kind |> SyntaxKinds.astToGreen, lexeme)
        state

    /// Consume the current token as `syntaxKind` iff it is of `syntaxKind`
    let public expect (builder: GreenNodeBuilder) tokenKind syntaxKind state =
        if currentKind state = tokenKind then
            eat builder syntaxKind state
        else
            bufferDiag (sprintf "Expected %A" tokenKind |> Diagnostic) state

    /// Loop and skip any tokens of kind `SPACE`.
    let rec public skipWs (builder: GreenNodeBuilder) state =
        if currentKind state = TokenKind.Space then
            eat builder SyntaxKind.SPACE state |> skipWs builder
        else
            state

/// Parse a single atomic value. Accepts either a symbol or number
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

/// Parse an expression that is either a form or an atom. Handles leading
/// and trailing whitespace.
let rec private parseExpression (builder: GreenNodeBuilder) state =
    let state = skipWs builder state

    if currentKind state = TokenKind.OpenParen then
        parseForm builder state
    else
        parseAtom builder state
    |> skipWs builder

/// Parse a parenthesised expression form. This parser expects the current token
/// to be an opening paren.
and private parseForm (builder: GreenNodeBuilder) state =

    builder.StartNode(SyntaxKind.FORM |> SyntaxKinds.astToGreen)
    let mutable state = eat builder SyntaxKind.OPEN state

    while not <| List.contains (currentKind state) [ TokenKind.EOF; TokenKind.CloseParen ] do
        state <- parseExpression builder state

    state <- expect builder TokenKind.CloseParen SyntaxKind.CLOSE state

    builder.FinishNode()
    state

/// Parse a sequence of expressions into a program node.
let rec private parseProgram (builder: GreenNodeBuilder) state =
    if currentKind state <> TokenKind.EOF then
        parseExpression builder state |> parseProgram builder
    else
        state

/// Parse the given `input` into a syntax tree.
let parse input =
    let state =
        { Diags = []
          Tokens = Lex.lex input |> List.ofSeq }

    let tree = new GreenNodeBuilder()
    let state = parseProgram tree state

    { Tree =
        tree.BuildRoot(SyntaxKind.PROGRAM |> SyntaxKinds.astToGreen)
        |> SyntaxNode.CreateRoot
        |> Program
      Diagnostics = state.Diags }
