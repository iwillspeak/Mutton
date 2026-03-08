module Mutton.Expand

open Illuminate
open Mutton.Syntax

// ----------- Syntax Environments -----------------

/// Close a piece of syntax over the given definition environment.
let close (stx: Stx) (defEnv: StxEnv) : Stx =
    StxClosure(stx, defEnv)

/// Resolve a given identifer in the given syntax environment
let resolve (id: Ident) (stxEnv: StxEnv) : StxBinding =
    // TODO: Does the syntax environment want to resolve names, or identifiers?
    match stxEnv.TryFind id.Name with
    | Some binding -> binding
    | None ->
        // If the identifier isn't bound, it takes on the 'default' meaining.
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

/// Expand any syntax mutations (macros) within the given tree. Returns the
/// input tree with all syntax lowered.
let rec public expand (stx: Stx) (stxEnv: StxEnv) : Stx =
    match stx with
    | StxLiteral _ -> stx
    | StxIdent(id, sym) ->
        let resolved = resolve id stxEnv
        match resolved with
        | Var _ -> stx
        | x -> failwith $"Unexpected non-variable syntax binding for identifier {id.Name}: %A{resolved}"
    | StxForm ([], _) -> stx
    | StxForm (head :: args, f) ->
        match head with
        | StxIdent(id, sym) ->
            let resolved = resolve id stxEnv
            match resolved with
            | Macro transformer ->
                transformer stx stxEnv
            | Quot | Stx -> stx
            | Lam -> expandLam head args f stxEnv
            | Def -> failwith "def not implemented, needs to modify 'outer' syntax environment"
            | DefSyn -> failwith "def-syn not implemented. Needs to modify the environment to add the transfomer"
            | Var _ ->
                // These are not macros, so we just recursively expand the subforms.
                StxForm(List.map (fun arg -> expand arg stxEnv) (head :: args), f)
        | _ ->
            StxForm(List.map (fun arg -> expand arg stxEnv) (head :: args), f)
    | StxClosure(stx, env) ->
         expand stx env

and expandLam head args low stxEnv =
    match args with
    | StxIdent(id, s) :: body ->
        let innerEnv = rename stxEnv id
        // This might need to become some kind of fold if expand starts returning
        // a modified environemtn _as well_ as the expanded syntax (for defs).
        let expandedBody = List.map (fun arg -> expand arg innerEnv) body
        StxForm(head :: StxIdent(id, s) :: expandedBody, low)
    | _ -> failwith "Invalid syntax for lam: expected (lam <id> <body>)"

