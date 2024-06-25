module Mutton.Illuminate

open Mutton.Syntax

type public StxContext = StxContext of unit

module private StxContext =

    let public empty = StxContext()

type public Stx =
    | StxAtom of Atom * StxContext
    | StxForm of Stx list * Form * StxContext

let rec private illumExpr (exp: Expression) : Stx =
    match exp with
    | Atom a -> StxAtom(a, StxContext.empty)
    | Form f -> StxForm((Seq.map illumExpr f.Body |> List.ofSeq), f, StxContext.empty)


let public illum (tree: Program) =
    tree.Body |> Seq.map (illumExpr) |> List.ofSeq
