module Mutton.Expand

open Illuminate
open Mutton.Syntax

/// Supported transform types. Identifiers are mapped to these in the expansion
/// context. When forms reference a known transform, expansion handles them
/// specially rather than treating them as plain applications.
type private Transform =
    | Fun
    | Quote
    | Def
    | Var of string * Symbol * ScopeId

/// Expand a single item in the given context.
///
/// This mimics the behaviour of the binder in recognising our special forms.
/// Rather than emitting a bound tree however we are just transforming the input
/// syntax to re-write any macros, and rename any variables.
let rec private expandInCtx
    (scopes: ScopeTable)
    (ctx: Map<string, Transform>)
    (stx: Stx)
    : Stx * Map<string, Transform> =
    match stx with
    | StxForm(inner, sForm, fSCtx) ->
        match inner with
        | [] -> (stx, ctx)
        | rator :: rands ->
            match rator with
            | StxIdent(id, sym, idScope) ->
                let resolved = scopes.Resolve(id, idScope)

                match Map.tryFind resolved ctx with
                | Some(Fun) ->
                    match rands with
                    | StxIdent(argId, argSym, argScope) :: body ->
                        let freshName = freshId argSym
                        let fresh = Var(freshName, argSym, scopes.Root)
                        let bodyCtx = Map.add (scopes.Resolve(argId, argScope)) fresh ctx
                        let body = List.mapFold (expandInCtx scopes) bodyCtx body |> fst

                        (StxForm(
                            rator :: StxIdent(freshName, argSym, scopes.Root) :: body,
                            sForm,
                            fSCtx
                         ),
                         ctx)
                    | _ -> failwithf "Invalid lambda form %A" sForm
                | Some(Quote) -> (stx, ctx)
                | Some(Def) ->
                    match rands with
                    | [ StxIdent(defId, defSym, defScope); expr ] ->
                        let freshName = freshId defSym
                        let fresh = Var(freshName, defSym, scopes.Root)
                        let body, ctx = expandInCtx scopes ctx expr
                        let ctx = Map.add (scopes.Resolve(defId, defScope)) fresh ctx

                        (StxForm(
                            [ rator; StxIdent(freshName, defSym, scopes.Root); body ],
                            sForm,
                            fSCtx
                         ),
                         ctx)
                    | _ -> failwithf "ERR:%A Invalid def form %A" sForm.Syntax.Range sForm
                | Some(Var(idNew, symNew, scopeNew)) ->
                    (StxForm(
                        StxIdent(idNew, symNew, scopeNew)
                        :: List.map (expandInCtx scopes ctx >> fst) rands,
                        sForm,
                        fSCtx
                     ),
                     ctx)
                | None ->
                    (StxForm(
                        StxIdent(resolved, sym, idScope)
                        :: List.map (expandInCtx scopes ctx >> fst) rands,
                        sForm,
                        fSCtx
                     ),
                     ctx)
            | _ -> (StxForm(List.mapFold (expandInCtx scopes) ctx inner |> fst, sForm, fSCtx), ctx)
    | StxIdent(id, sym, scope) ->
        let resolved = scopes.Resolve(id, scope)

        match Map.tryFind resolved ctx with
        | Some(Var(idNew, symNew, scopeNew)) -> (StxIdent(idNew, symNew, scopeNew), ctx)
        | _ -> (StxIdent(resolved, sym, scope), ctx)
    | StxLiteral _ -> (stx, ctx)

/// Expand any syntax mutations (macros) within the given tree. Returns the
/// input tree with all syntax lowered.
let public expand (scopes: ScopeTable) (syntax: Stx list) : Stx list =
    let ctx =
        Map.empty
        |> Map.add "lam" (Fun)
        |> Map.add "stx" (Quote)
        |> Map.add "quot" (Quote)
        |> Map.add "def" (Def)
        |> Map.add "def-syn" (Quote)

    syntax |> List.mapFold (expandInCtx scopes) ctx |> fst
