module Mutton.Binder

open Mutton.Illuminate

/// Bound Expression Node
///
/// Each variant represents a different kind of bound node available in the
/// tree. Bound nodes represent the _semantic_ structure of the program rather
/// than the syntactic one. Any symbols in the program are resolved to their
/// appropriate storage locations.
type Bound =
    | Number of int
    | Var of string
    | Def of string * Bound
    | App of Bound * Bound List
    | Fun of string * Bound
    | Stx of Stx
    | Quot of Stx // FIXME: need to strip this ...
    | Error of string

/// Bind a single expression
let rec private bindOne =
    function
    | StxLiteral(l, _) ->
        match l.Value with
        | Some(n) -> Number n
        | _ ->
            // TODO: Implement binder erorrs. This is the case of a malformed
            //       or missing atom value. Most likely as the result of a
            //       parse error.
            Error $"Invalid literal %A{l}" 
    | StxIdent(id, _, sctx) -> Var(resolve id sctx)
    | StxForm(items, syntax, _) ->
        match items with
        | [] ->
            // TODO: Error to have no items in application
            Error "No applicant in application form."
        | applicant :: args ->
            let bindSipleApp () =
                let called = bindOne applicant
                let args = List.map (bindOne) args
                App(called, args)

            match applicant with
            | StxIdent(id, _, sctx) ->
                // FIXME: we should look at the _binding_ of the resolved ID here
                //        not just the plain symbol.
                match resolve id sctx with
                | "lam" -> bindLambda args
                | "def" -> bindDefinition args
                | "quot" -> bindQuotation args
                | "stx" -> bindSyntaxQuotation args
                | _ -> bindSipleApp ()
            | _ -> bindSipleApp ()

and private bindLambda args =
    match args with
    | [ StxIdent(id, _, idctx); body ] -> Fun((resolve id idctx), (bindOne body))
    | _ -> Error $"Invalid lambda form %A{args}"

and private bindDefinition args =
    match args with
    | [ StxIdent(id, _, idCtx); body ] ->
        Def((resolve id idCtx), (bindOne body))
    | _ -> $"Invalid `def` form %A{args}" |> Error

and private bindQuotation args =
    match args with
    | [ form ] -> Quot form // FIXME: Needs stripping
    | _ -> $"Invalid `quot` form: %A{args}" |> Error

and private bindSyntaxQuotation args =
    match args with
    | [ form ] -> Stx form
    | _ -> $"Invalid `stx` form: %A{args}" |> Error

/// Main binder entry point. Runs the binder over each input item
let public bind = List.map bindOne
