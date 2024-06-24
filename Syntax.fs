namespace Mutton.Syntax

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
