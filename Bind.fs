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

/// A single macro transformer rule. `Params` are the pattern-variable names
/// from the pattern form and `Template` is the syntax to expand into.
type private MacroRule =
    { Params: string list
      Template: Stx
      DefSiteRenames: Map<string, string>
      DefSiteMacros: Map<string, MacroRule list> }

/// Environment threaded through the binder. Carries scope information,
/// alpha-renamings, macro definitions, and any pre-bound pattern-variable
/// values during a macro expansion.
type private BindEnv =
    { Scopes: ScopeTable
      Renames: Map<string, string>
      Macros: Map<string, MacroRule list>
      PreBound: Map<string, Bound> }

// ── Binding ────────────────────────────────────────────────────────────────

/// Bind a single syntax node, returning the bound tree (or None for
/// compile-time-only forms like `def-syn`) and the potentially-updated
/// environment.
let rec private bindOne (env: BindEnv) (stx: Stx) : Bound option * BindEnv =
    match stx with
    | StxLiteral(l, _) ->
        match l.Value with
        | Some n -> (Some(Quot(DNum n)), env)
        | None -> (Some(Error $"Invalid literal %A{l}"), env)

    | StxIdent(id, _, scope) ->
        let resolved = env.Scopes.Resolve(id, scope)
        // Check pre-bound pattern variables first (before alpha-renames).
        match Map.tryFind resolved env.PreBound with
        | Some bound -> (Some bound, env)
        | None ->
            let renamed = Map.tryFind resolved env.Renames |> Option.defaultValue resolved
            (Some(Var renamed), env)

    | StxForm(items, _, _) ->
        match items with
        | [] -> (Some(Error "No applicant in application form."), env)
        | applicant :: args ->
            match applicant with
            | StxIdent(id, _, scope) ->
                let resolved = env.Scopes.Resolve(id, scope)
                // If the operator is a pre-bound pattern variable it is a
                // value, not a keyword — treat the form as a plain application.
                if Map.containsKey resolved env.PreBound then
                    (Some(bindSimpleApp env applicant args), env)
                else
                    let renamed =
                        Map.tryFind resolved env.Renames |> Option.defaultValue resolved

                    match renamed with
                    | "lam" -> (Some(bindLambda env args), env)
                    | "def" -> bindDefinition env args
                    | "def-syn" -> bindDefSyn env args
                    | "quot" -> (Some(bindQuotation args), env)
                    | "stx" -> (Some(bindSyntaxQuotation args), env)
                    | _ ->
                        match Map.tryFind renamed env.Macros with
                        | Some rules -> expandAndBind env rules args
                        | None -> (Some(bindSimpleApp env applicant args), env)
            | _ -> (Some(bindSimpleApp env applicant args), env)

// ── Special forms ──────────────────────────────────────────────────────────

and private bindLambda (env: BindEnv) (args: Stx list) =
    match args with
    | [ StxIdent(id, sym, scope); body ] ->
        let origName = env.Scopes.Resolve(id, scope)
        let freshName = freshId sym
        let bodyEnv =
            { env with
                Renames = Map.add origName freshName env.Renames
                // Shadow any pre-bound pattern variable with the same name so
                // template-introduced bindings correctly capture their own body.
                PreBound = Map.remove origName env.PreBound }

        let bound, _ = bindOne bodyEnv body

        Fun(freshName, bound |> Option.defaultValue (Error "void"))
    | _ -> Error $"Invalid lambda form %A{args}"

and private bindDefinition (env: BindEnv) (args: Stx list) =
    match args with
    | [ StxIdent(id, sym, scope); body ] ->
        let origName = env.Scopes.Resolve(id, scope)
        let freshName = freshId sym
        let bound, env = bindOne env body
        let env =
            { env with
                Renames = Map.add origName freshName env.Renames
                PreBound = Map.remove origName env.PreBound }

        (bound |> Option.map (fun b -> Def(freshName, b)), env)
    | _ -> (Some(Error $"Invalid `def` form %A{args}"), env)

and private bindQuotation args =
    match args with
    | [ form ] -> Quot(strip form)
    | _ -> Error $"Invalid `quot` form: %A{args}"

and private bindSyntaxQuotation args =
    match args with
    | [ form ] -> Stx form
    | _ -> Error $"Invalid `stx` form: %A{args}"

// ── Macro definition ───────────────────────────────────────────────────────

/// Parse a `def-syn` form. Extracts the macro name and transformer rules,
/// registers the macro in the environment, and produces no runtime output.
and private bindDefSyn (env: BindEnv) (args: Stx list) : Bound option * BindEnv =
    match args with
    | StxIdent(id, _, scope) :: ruleStxs ->
        let macroName =
            let resolved = env.Scopes.Resolve(id, scope)
            Map.tryFind resolved env.Renames |> Option.defaultValue resolved

        let rules = parseDefSynRules env ruleStxs
        let env = { env with Macros = Map.add macroName rules env.Macros }
        (None, env)
    | _ -> (Some(Error $"Invalid `def-syn` form %A{args}"), env)

/// Parse the rule forms inside a `def-syn`. Each rule is expected to be
/// `(<pattern> <template>)` where the pattern is a form whose first element
/// is the macro name and the remaining elements are parameter names.
and private parseDefSynRules (env: BindEnv) (ruleStxs: Stx list) : MacroRule list =
    ruleStxs
    |> List.choose (fun ruleStx ->
        match ruleStx with
        | StxForm([ pattern; template ], _, _) ->
            match pattern with
            | StxForm(StxIdent _ :: paramStxs, _, _) ->
                let parms =
                    paramStxs
                    |> List.choose (fun p ->
                        match p with
                        | StxIdent(pid, _, pscope) -> Some(env.Scopes.Resolve(pid, pscope))
                        | _ -> None)

                Some
                    { Params = parms
                      Template = template
                      DefSiteRenames = env.Renames
                      DefSiteMacros = env.Macros }
            | _ -> None
        | _ -> None)

// ── Macro expansion ────────────────────────────────────────────────────────

/// Expand a macro call by matching the arguments against the rule patterns,
/// pre-binding each argument in the *use-site* environment, then binding the
/// template with those pre-bound values. This naturally provides hygiene:
/// template-introduced bindings (lam/def) get fresh names via alpha-renaming,
/// while pattern variables resolve to their already-bound use-site values.
and private expandAndBind (env: BindEnv) (rules: MacroRule list) (args: Stx list) : Bound option * BindEnv =
    let matchedRule =
        rules
        |> List.tryPick (fun rule ->
            if List.length rule.Params = List.length args then
                Some rule
            else
                None)

    match matchedRule with
    | Some rule ->
        // Bind each argument at the use site.
        let boundArgs =
            List.zip rule.Params args
            |> List.map (fun (param, argStx) ->
                let bound, _ = bindOne env argStx
                (param, bound |> Option.defaultValue (Error "void")))
            |> Map.ofList

        // Bind the template in the definition-site environment, augmented
        // with the pre-bound pattern variables and the full set of macros
        // known at expansion time (so later-defined macros are still visible
        // inside templates — matches sequential top-level evaluation order).
        let templateEnv =
            { env with
                Renames = rule.DefSiteRenames
                Macros = Map.fold (fun acc k v -> Map.add k v acc) rule.DefSiteMacros env.Macros
                PreBound = boundArgs }

        let bound, _ = bindOne templateEnv rule.Template
        (bound, env)
    | None -> (Some(Error $"No matching macro rule for arguments %A{args}"), env)

// ── Simple application ─────────────────────────────────────────────────────

and private bindSimpleApp (env: BindEnv) applicant args =
    let called = bindOne env applicant |> fst |> Option.defaultValue (Error "void")
    let boundArgs = args |> List.map (fun a -> bindOne env a |> fst |> Option.defaultValue (Error "void"))
    App(called, boundArgs)

// ── Sequence binding ───────────────────────────────────────────────────────

/// Bind a sequence of top-level syntax nodes, threading the environment so
/// that `def` and `def-syn` forms are visible to later items. `def-syn`
/// produces no output in the bound tree.
and private bindSequence (env: BindEnv) (stxs: Stx list) : Bound list =
    match stxs with
    | [] -> []
    | stx :: rest ->
        let bound, env = bindOne env stx
        let restBound = bindSequence env rest

        match bound with
        | Some b -> b :: restBound
        | None -> restBound

/// Main binder entry point. Binds the given illuminated syntax, performing
/// alpha-renaming and macro expansion in a single pass. The expand phase
/// is no longer needed when using this entry point.
let public bind (scopes: ScopeTable) (syntax: Stx list) : Bound list =
    let env =
        { Scopes = scopes
          Renames = Map.empty
          Macros = Map.empty
          PreBound = Map.empty }

    bindSequence env syntax
