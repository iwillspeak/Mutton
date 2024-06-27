/// Lexical Tokenisation for Mutton
///
/// This module is responsible for running a simple state machine over input
/// text and producing a seuqence of tokens for the `Parse` stage to consume.
module Mutton.Lex

open System
open System.Text

/// Token kind enumeration
type public TokenKind =
    | Error
    | Sym
    | OpenParen
    | CloseParen
    | Number
    | Space
    | EOF

/// Current lexer state
type private LexState =
    | Start
    | Ident
    | Number
    | Space
    // Used for single-character tokens. This avoids us needing many states for
    // each different single character lexeme.
    | Simple of TokenKind

/// Tokenise the given `input`
///
/// Runs the lexical state machine over the given `input` text and produces a
/// sequence of `string * TokenKind` pairs for each recognised lexeme.
let public lex input =

    /// Convert lexer final states to their corresponding tokens. This is
    /// a trivial mapping as all states are final other than the `Start`, which
    /// we map to the `Error` token.
    let kindForState =
        function
        | LexState.Ident -> TokenKind.Sym
        | LexState.Start -> TokenKind.Error
        | LexState.Space -> TokenKind.Space
        | LexState.Number -> TokenKind.Number
        | LexState.Simple s -> s

    /// Compute the next transiation for the state machine. Inspects the current
    /// state and input character to deduce a transition, if any. Returns `None`
    /// if no transition exists.
    let next ch =
        function
        | LexState.Start ->
            match ch with
            | c when Char.IsWhiteSpace(c) -> Some(Space)
            | c when Char.IsNumber(c) -> Some(Number)
            | '(' -> Some(Simple(TokenKind.OpenParen))
            | ')' -> Some(Simple(TokenKind.CloseParen))
            | _ -> Some(Ident)
        | LexState.Ident ->
            match ch with
            | ')'
            | '(' -> None
            | c when Char.IsWhiteSpace(c) -> None
            | _ -> Some(Ident)
        | LexState.Number -> if Char.IsNumber(ch) then Some(Number) else None
        | LexState.Space -> if Char.IsWhiteSpace(ch) then Some(Space) else None
        | _ -> None

    let mutable state = LexState.Start
    let mutable lexeme = StringBuilder()

    seq {
        for c in input do
            // For each char in the input we compute the next transition. If one
            // exists we take it. If no transition exists we emit a token, and
            // compute the next transition again from the `Start` state.
            match next c state with
            | Some next ->
                state <- next
                lexeme <- lexeme.Append(c)
            | None ->
                yield ((lexeme.ToString()), (kindForState state))
                lexeme <- lexeme.Clear()

                lexeme <- lexeme.Append(c)

                match next c Start with
                | Some next -> state <- next
                | None ->
                    state <- Start
                    yield ((lexeme.ToString()), TokenKind.Error)
                    lexeme <- lexeme.Clear()

        // If the state machine was left with a token partially buffered then
        // emit that. This handles the case taht multi-char tokens such as
        // identifiers are the last item in the source.
        if state <> Start then
            yield ((lexeme.ToString()), (kindForState state))
    }
