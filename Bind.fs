module Mutton.Binder

open Mutton.Illuminate
open Mutton.Syntax

/// A simple s-expression datum. This is the runtime representation of
/// quoted syntax — compiler metadata (scope IDs, CST pointers) is stripped
/// away, leaving only the structural content.
type Datum =
    | DNum of int
    | DSym of string
    | DList of Datum list

/// Strip an illuminated syntax node down to a plain datum, discarding all
/// compiler metadata (scope IDs, CST node references).
let rec private strip (stx: Stx) : Datum =
    match stx with
    | StxLiteral(l, _) ->
        match l.Value with
        | Some n -> DNum n
        | None -> DSym "#err"
    | StxIdent(id, _, _) -> DSym id
    | StxForm(items, _, _) -> DList(List.map strip items)

/// Bound Expression Node
///
/// Each variant represents a different kind of bound node available in the
/// tree. Bound nodes represent the _semantic_ structure of the program rather
/// than the syntactic one. Any symbols in the program are resolved to their
/// appropriate storage locations.
type Bound =
    | Var of string
    | Def of string * Bound
    | App of Bound * Bound list
    | Fun of string * Bound
    | Stx of Stx
    | Quot of Datum
    | Error of string

// ── Binding ────────────────────────────────────────────────────────────────

/// Bind a single expression
let rec private bindOne =
    function
    | StxLiteral(l, _) ->
        match l.Value with
        | Some n -> Quot(DNum n)
        | None -> Error $"Invalid literal %A{l}"
    | StxIdent(id, _, sctx) -> Var(resolve id sctx)
    | StxForm(items, _, _) ->
        match items with
        | [] -> Error "No applicant in application form."
        | applicant :: args ->
            match applicant with
            | StxIdent(id, _, sctx) ->
                // FIXME: we should look at the _binding_ of the resolved ID here
                //        not just the plain symbol.
                match resolve id sctx with
                | "lam" -> bindLambda args
                | "def" -> bindDefinition args
                | "quot" -> bindQuotation args
                | "stx" -> bindSyntaxQuotation args
                | _ -> bindSimpleApp applicant args
            | _ -> bindSimpleApp applicant args

and private bindSimpleApp applicant args =
    let called = bindOne applicant
    let args = List.map bindOne args
    App(called, args)

and private bindLambda args =
    match args with
    | [ StxIdent(id, _, idctx); body ] -> Fun((resolve id idctx), (bindOne body))
    | _ -> Error $"Invalid lambda form %A{args}"

and private bindDefinition args =
    match args with
    | [ StxIdent(id, _, idCtx); body ] ->
        Def((resolve id idCtx), (bindOne body))
    | _ -> Error $"Invalid `def` form %A{args}"


and private bindQuotation args =
    match args with
    | [ form ] -> Quot(strip form)
    | _ -> Error $"Invalid `quot` form: %A{args}"

and private bindSyntaxQuotation args =
    match args with
    | [ form ] -> Stx form
    | _ -> Error $"Invalid `stx` form: %A{args}"

/// Main binder entry point. Runs the binder over each input item
let public bind = List.map bindOne
