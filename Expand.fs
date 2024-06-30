module Mutton.Expand

open Illuminate
open Mutton.Syntax

/// Supported transfomr types. Identifiers are mapped to these in the expansion
/// context. When forms reference
type private Transform =
    | Fun
    | Quote
    | Def
    | Var of string * Symbol * StxContext

/// Expand a single item in the given context.
///
/// This mimics the behaviour of the binder in recognising our special forms.
/// Rather than emitting a bound tree hwoever we are just transforming the input
/// syntax to re-write any macros, and rename any variables.
let rec private expandInCtx (ctx: Map<string, Transform>) (stx: Stx) : Stx * Map<string, Transform> =
    match stx with
    | StxForm(inner, sForm, fSCtx) ->
        match inner with
        | [] -> (stx, ctx)
        | rator :: rands ->
            match rator with
            | StxIdent(id, sym, idctx) ->
                let resolved = resolve id idctx

                match Map.tryFind resolved ctx with
                | Some(Fun) ->
                    match rands with
                    | StxIdent(argId, argSym, argSctx) :: body ->
                        let freshName = freshId argSym
                        let fresh = Var(freshName, argSym, StxContext.Unit)
                        let bodyCtx = Map.add (resolve argId argSctx) fresh ctx
                        let body = List.mapFold (expandInCtx) bodyCtx body |> fst
                        (StxForm(rator :: StxIdent(freshName, argSym, StxContext.Unit) :: body, sForm, fSCtx), ctx)
                    | _ -> failwithf "Invalid lambda form %A" sForm
                | Some(Quote) -> (stx, ctx)
                | Some(Def) ->
                    match rands with
                    | [ StxIdent(defId, defSym, defSctx); expr ] ->
                        let freshName = freshId defSym
                        let fresh = Var(freshName, defSym, StxContext.Unit)
                        let body, ctx = expandInCtx ctx expr
                        let ctx = Map.add (resolve defId defSctx) fresh ctx
                        (StxForm([ rator; StxIdent(freshName, defSym, StxContext.Unit); body ], sForm, fSCtx), ctx)
                    | _ -> failwithf "ERR:%A Invalid def form %A" sForm.Syntax.Range sForm
                | Some(Var(idNew, symNew, ctxNew)) ->
                    (StxForm(StxIdent(idNew, symNew, ctxNew) :: List.map (expandInCtx ctx >> fst) rands, sForm, fSCtx),
                     ctx)
                | None ->
                    (StxForm(StxIdent(resolved, sym, idctx) :: List.map (expandInCtx ctx >> fst) rands, sForm, fSCtx),
                     ctx)
            | _ -> (StxForm(List.mapFold (expandInCtx) ctx inner |> fst, sForm, fSCtx), ctx)
    | StxIdent(id, sym, sctx) ->
        let resolved = resolve id sctx

        match Map.tryFind resolved ctx with
        | Some(Var(idNew, symNew, ctxNew)) -> (StxIdent(idNew, symNew, ctxNew), ctx)
        | _ -> (StxIdent(resolved, sym, sctx), ctx)
    | StxLiteral _ -> (stx, ctx)

/// Expand any syntax mutations (macros) within the given tree. Returns the
/// input tree with all syntax lowered.
let public expand (syntax: Stx list) : Stx list =
    let ctx =
        Map.empty
        |> Map.add "lam" (Fun)
        |> Map.add "stx" (Quote)
        |> Map.add "quot" (Quote)
        |> Map.add "def" (Def)

    syntax |> List.mapFold (expandInCtx) ctx |> fst
