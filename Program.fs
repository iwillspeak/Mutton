namespace Mutton

module Parse =
    open Firethorn
    open System
    open System.Text
    type TokenKind =
        | Error
        | Sym
        | OpenParen
        | CloseParen
        | Number
        | Space
        | EOF

    type AstKind =
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

    let private astToGreen (kind: AstKind) = SyntaxKind(int kind)

    let private greenToAst = function
        | SyntaxKind n -> enum<AstKind> n

    type private LexState =
        | Start
        | Ident
        | Number
        | Space
        | Simple of TokenKind

    let private lex input =
        let kindForState = function
            | LexState.Ident -> TokenKind.Sym
            | LexState.Start -> TokenKind.Error
            | LexState.Space -> TokenKind.Space
            | LexState.Number -> TokenKind.Number
            | LexState.Simple s -> s

        let next ch = function
            | LexState.Start ->
                match ch with
                | c when Char.IsWhiteSpace(c) -> Some(Space)
                | c when Char.IsNumber(c) -> Some(Number)
                | '(' -> Some(Simple(TokenKind.OpenParen))
                | ')' -> Some(Simple(TokenKind.CloseParen))
                | _ -> Some(Ident)
            | LexState.Ident -> 
                match ch with
                | ')' | '(' -> None
                | c when Char.IsWhiteSpace(c) -> None
                | _ -> Some(Ident)
            | LexState.Number ->
                if Char.IsNumber(ch) then
                    Some(Number)
                else
                    None
            | LexState.Space ->
                if Char.IsWhiteSpace(ch) then
                    Some(Space)
                else
                    None
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
                    | Some next ->
                        state <- next
                    | None ->
                        state <- Start
                        yield ((lexeme.ToString()), TokenKind.Error)
                        lexeme <- lexeme.Clear()

            if state <> Start then
                yield ((lexeme.ToString()), (kindForState state))
        }
    
    let parse input = lex input

module Main =
    open System

    [<EntryPoint>]
    let main args =
        let rec repl (prompt: string) =
            Console.Write(prompt)
            Console.Out.Flush()
            let line = Console.ReadLine()
            let parsed = Parse.parse line
            printfn ">> %A" parsed
            
            repl prompt

        repl "> "