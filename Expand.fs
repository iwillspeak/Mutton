module Mutton.Expand

open Illuminate

/// Supported transfomr types. Identifiers are mapped to these in the expansion
/// context. When forms reference
type private Transform =
    | Fun
    | Quote
    | Var of string

// FIXME: Unify resolve
let resolve = id

/// Expand a single item in the given context.
///
/// This mimics the behaviour of the binder in recognising our special forms.
/// Rather than emitting a bound tree hwoever we are just transforming the input
/// syntax to re-write any macros, and rename any variables.
let private expandInCtx (ctx: Map<string, Transform>) (stx: Stx) : Stx * Map<string, Transform> =
    match stx with
    | StxForm _ -> (stx, ctx)
    | StxIdent(id, a, sctx) -> (StxIdent(resolve id, a, sctx), ctx)
    | StxLiteral(a, _) ->
        match a.Value with
        | _ -> (stx, ctx)

/// Expand any syntax mutations (macros) within the given tree. Returns the
/// input tree with all syntax lowered.
let public expand (syntax: Stx list) : Stx list =
    let ctx =
        Map.empty
        |> Map.add "lam" (Fun)
        |> Map.add "stx" (Quote)
        |> Map.add "quot" (Quote)

    syntax |> List.mapFold (expandInCtx) ctx |> fst
