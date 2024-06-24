module Mutton.Lex

open System
open System.Text

type public TokenKind =
    | Error
    | Sym
    | OpenParen
    | CloseParen
    | Number
    | Space
    | EOF

type private LexState =
    | Start
    | Ident
    | Number
    | Space
    | Simple of TokenKind

let public lex input =

    let kindForState =
        function
        | LexState.Ident -> TokenKind.Sym
        | LexState.Start -> TokenKind.Error
        | LexState.Space -> TokenKind.Space
        | LexState.Number -> TokenKind.Number
        | LexState.Simple s -> s

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

        if state <> Start then
            yield ((lexeme.ToString()), (kindForState state))
    }
