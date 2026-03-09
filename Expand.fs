module Mutton.Expand

open Illuminate
open Mutton.Syntax

// ----------- Syntax Environments -----------------

/// Close a piece of syntax over the given definition environment.
let close (stx: Stx) (defEnv: StxEnv) : Stx =
    StxClosure(stx, defEnv)

/// Resolve a given identifier in the given syntax environment
let resolve (id: Ident) (stxEnv: StxEnv) : StxBinding =
    // TODO: Does the syntax environment want to resolve names, or identifiers?
    match stxEnv.TryFind id.Name with
    | Some binding -> binding
    | None ->
        // If the identifier isn't bound, it takes on the 'default' meaning.
        // For keywords this is their marker symbol. For other identifiers this
        // is a variable in the scope 0 context (global).
        match id.Name with
        | "lam" -> Lam
        | "def" -> Def
        | "def-syn" -> DefSyn
        | "quot" -> Quot
        | "stx" -> Stx
        | _ -> Var id

/// A stamp marking a distinct syntax context.
let mutable private stamp = ref 1

/// Generate a fresh stamp for tracking syntax provenance
let newStamp () =
    let s = System.Threading.Interlocked.Increment(stamp)
    s

/// A rename adds a new entry to the environment binding the name of the given
/// identifier to a new variable.
let rename (env: StxEnv) (id: Ident) : StxEnv =
    Map.add id.Name (Var { Name = id.Name; Stamp = newStamp() }) env

// ----------- Pattern Matching and Template Application -----------------

/// A pre-parsed macro rule: a list of pattern-variable identifiers and the
/// template to substitute into when the rule matches.
type MacroRule = { PatArgs: Ident list; Template: Stx }

/// Try to match pattern arguments against call arguments.
/// Each pattern variable identifier binds to the corresponding call argument,
/// wrapped in a use-site closure for hygiene.
/// Returns Some bindings if successful, None if arity doesn't match.
let private matchPatternArgs (rule: MacroRule) (callArgs: Stx list) (useEnv: StxEnv) : Map<string, Stx> option =
    if List.length rule.PatArgs <> List.length callArgs then None
    else
        List.zip rule.PatArgs callArgs
        |> List.fold (fun acc (id, arg) ->
            match acc with
            | None -> None
            | Some bindings -> Some (Map.add id.Name (StxClosure(arg, useEnv)) bindings)
        ) (Some Map.empty)

/// Apply variable substitution bindings to a syntax template.
let rec private applyTemplate (template: Stx) (bindings: Map<string, Stx>) : Stx =
    match template with
    | StxIdent(id, _) ->
        match Map.tryFind id.Name bindings with
        | Some stx -> stx
        | None -> template
    | StxLiteral _ -> template
    | StxForm(items, f) ->
        StxForm(List.map (fun item -> applyTemplate item bindings) items, f)
    | StxClosure(stx, env) ->
        StxClosure(applyTemplate stx bindings, env)

// ----------- Macro Expansion -----------------

/// Expand any syntax mutations (macros) within the given tree. Returns an
/// optional expanded form (None for definitional forms like def-syn) and the
/// (possibly updated) syntax environment.
let rec public expand (stx: Stx) (stxEnv: StxEnv) : Stx option * StxEnv =
    match stx with
    | StxLiteral _ -> (Some stx, stxEnv)
    | StxIdent(id, sym) ->
        let resolved = resolve id stxEnv
        match resolved with
        | Var _ -> (Some stx, stxEnv)
        | x -> failwith $"Unexpected non-variable syntax binding for identifier {id.Name}: %A{resolved}"
    | StxForm ([], _) -> (Some stx, stxEnv)
    | StxForm (head :: args, f) ->
        match head with
        | StxIdent(id, sym) ->
            let resolved = resolve id stxEnv
            match resolved with
            | Macro transformer ->
                (Some (transformer stx stxEnv), stxEnv)
            | Quot | Stx -> (Some stx, stxEnv)
            | Lam -> (Some (expandLam head args f stxEnv), stxEnv)
            | Def -> failwith "def not implemented, needs to modify 'outer' syntax environment"
            | DefSyn -> expandDefSyn args stxEnv
            | Var _ ->
                // These are not macros, so we just recursively expand the subforms.
                let expanded = StxForm(List.map (expandOne stxEnv) (head :: args), f)
                (Some expanded, stxEnv)
        | _ ->
            let expanded = StxForm(List.map (expandOne stxEnv) (head :: args), f)
            (Some expanded, stxEnv)
    | StxClosure(stx, env) ->
        let (result, _) = expand stx env
        (result, stxEnv)

and private expandOne (stxEnv: StxEnv) (stx: Stx) : Stx =
    match expand stx stxEnv with
    | Some result, _ -> result
    | None, _ -> failwith "def-syn not valid in expression position"

and expandLam head args low stxEnv =
    match args with
    | StxIdent(id, s) :: body ->
        let innerEnv = rename stxEnv id
        let expandedBody = List.map (expandOne innerEnv) body
        StxForm(head :: StxIdent(id, s) :: expandedBody, low)
    | _ -> failwith "Invalid syntax for lam: expected (lam <id> <body>)"

/// Parse a `def-syn` form and return an updated syntax environment containing
/// the newly defined macro transformer.
and private expandDefSyn (args: Stx list) (stxEnv: StxEnv) : Stx option * StxEnv =
    match args with
    | StxIdent(macroName, _) :: ruleStxs ->
        let rules = List.map parseRule ruleStxs
        let transformer = makeSynTransformer rules stxEnv macroName.Name
        let newEnv = Map.add macroName.Name (Macro transformer) stxEnv
        (None, newEnv)
    | _ -> failwith "Invalid def-syn form: expected (def-syn <name> <rule>...)"

/// Parse a single syntax rule stx into a `MacroRule`, failing immediately if
/// the rule shape is invalid.
and private parseRule (ruleStx: Stx) : MacroRule =
    match ruleStx with
    | StxForm([StxForm(_ :: patArgs, _); template], _) ->
        // Extract the identifier from each pattern argument, failing if any is not an identifier
        let idents = patArgs |> List.mapi (fun i pat ->
            match pat with
            | StxIdent(id, _) -> id
            | _ -> failwith $"Invalid pattern in macro rule at position {i}: expected an identifier, got %A{pat}")
        { PatArgs = idents; Template = template }
    | _ -> failwith $"Invalid macro rule: expected ((name pat...) template), got %A{ruleStx}"

/// Build a macro transformer from a list of pre-parsed macro rules and the
/// definition-time environment. The transformer performs hygienic
/// pattern-matching and template substitution.
and private makeSynTransformer (rules: MacroRule list) (defEnv: StxEnv) (macroName: string) : Transformer =
    fun (callStx: Stx) (useEnv: StxEnv) ->
        match callStx with
        | StxForm(_ :: callArgs, _) ->
            let tryRule (rule: MacroRule) =
                match matchPatternArgs rule callArgs useEnv with
                | Some bindings ->
                    // Substitute pattern variables into template, then close
                    // over the definition-time environment for hygiene.
                    let substituted = applyTemplate rule.Template bindings
                    Some (StxClosure(substituted, defEnv))
                | None -> None
            match List.tryPick tryRule rules with
            | Some expanded ->
                match expand expanded useEnv with
                | Some result, _ -> result
                | None, _ -> failwith $"macro '{macroName}': def-syn in template is not valid"
            | None -> failwith $"No matching rule for macro '{macroName}'"
        | _ -> failwith $"Invalid macro call form for '{macroName}'"

