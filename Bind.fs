module Mutton.Binder

open Mutton.Illuminate
open System.Collections.Generic

module Utils =

    /// Pull the `Ok` values out of a list of `Result`s, or return the first `Error` if any are present.
    let accumulateResults (results: Result<'a, 'e> list) : Result<'a list, 'e> =
        let rec loop acc = function
            | [] -> Ok (List.rev acc)
            | r :: rs ->
                match r with
                | Ok v -> loop (v :: acc) rs
                | Error e -> Error e

        loop [] results

/// A named storage location with a unique numeric suffix for scope tracking.
/// The integer disambiguates variables with the same base name in different scopes.
type Storage = string * int

/// A simple s-expression datum. This is the runtime representation of
/// quoted syntax — compiler metadata (scope IDs, CST pointers) is stripped
/// away, leaving only the structural content.
type Datum =
    | DNum of int
    | DSym of string
    | DList of Datum list

/// Bound Expression Node
///
/// Each variant represents a different kind of bound node available in the
/// tree. Bound nodes represent the _semantic_ structure of the program rather
/// than the syntactic one. Any symbols in the program are resolved to their
/// appropriate storage locations.
type Bound =
    | Var of Storage
    | Def of Storage * Bound
    | App of Bound * Bound list
    | Fun of Storage * Bound
    | Stx of Stx
    | Quot of Datum

/// Errors that can occur during binding. These are simple string messages with
/// attached source ranges. In a more complex implementation, these would likely
/// be richer structures with error codes, suggestions, etc.
type BinderError = string * Firethorn.TextRange

/// A simple binding context. This is used to resolve identifiers to their
/// bound names. In a more complex language, this would need to be extended to
/// support multiple namespaces, modules, etc.
type private BindingContext =
    {
        Parent: BindingContext option
        Bindings: Dictionary<Ident, Storage>
    }

module private BinderCtx =

    /// Create an empty binding context
    let empty = { Parent = None; Bindings = Dictionary<Ident, Storage>() }

    /// Extend a binding context with a new name mapping
    let extend ident bound ctx =
        ctx.Bindings.Add(ident, bound)

    /// Create a new child context with the given parent
    let withParent parent =
        { Parent = Some parent; Bindings = Dictionary<Ident, Storage>() }

    /// Resolve a name in the given context, searching parent contexts if
    /// necessary. Returns the resolved name or the original name if not found.
    let rec resolve ident ctx =
        match ctx.Bindings.TryGetValue ident with
        | true, bound -> bound
        | false, _->
            match ctx.Parent with
            | Some parent -> resolve ident parent
            | None -> ident.Name, 0

// ── Binding ────────────────────────────────────────────────────────────────

let mutable private varSuffix = ref 0

/// Generate a fresh storage location for the given variable name
let private freshStorage (name: string) : Storage =
    let suffix = System.Threading.Interlocked.Increment(varSuffix)
    (name, suffix)

/// Strip an illuminated syntax node down to a plain datum, discarding all
/// compiler metadata (scope IDs, CST node references).
let rec private strip (stx: Stx) : Datum =
    match stx with
    | StxLiteral l ->
        match l.Value with
        | Some n -> DNum n
        | None -> DSym "#err"
    | StxIdent(id, _) -> DSym id.Name
    | StxForm(items, _) -> DList(List.map strip items)
    | StxClosure(stx, _) -> strip stx

/// Bind a single expression
let rec private bindOne (ctx: BindingContext) =
    function
    | StxLiteral l ->
        match l.Value with
        | Some n -> Quot(DNum n) |> Some |> Ok
        | None -> ($"Invalid literal %A{l}", l.Syntax.Range) |> Error
    | StxIdent(id, _) ->
        Var(BinderCtx.resolve id ctx) |> Some |> Ok
    | StxForm(items, f) -> 
        match items with
        | [] -> Error ($"No applicant in application form.", f.Syntax.Range)
        | StxIdent(id, _) :: args when id.Name = "lam" ->
            bindLambda ctx f args
        | StxIdent(id, _) :: args when id.Name = "def" ->
            bindDefinition ctx f args
        | StxIdent(id, _) :: args when id.Name = "quot" ->
            bindQuotation f args
        | StxIdent(id, _) :: args when id.Name = "stx" ->
            bindSyntaxQuotation f args
        | applicant :: args ->
            bindSimpleApp ctx f applicant args
    | StxClosure(stx, env) -> bindOne ctx stx

and private bindSimpleApp ctx f applicant args =
    bindOne ctx applicant
    |> Result.bind (fun called ->
        args
        |> List.map (bindOne ctx)
        |> Utils.accumulateResults
        |> Result.bind (fun boundArgs ->
            match called with
            | Some called ->
                App(called, (List.choose id boundArgs)) |> Some |> Ok
            | None -> Error ($"Invalid function in application: has no value.", f.Syntax.Range)))

and private bindLambda ctx f args =
    match args with
    | [ StxIdent(id, _); body ] ->
        let argStorage = freshStorage id.Name
        let bodyCtx =
            BinderCtx.withParent ctx
        BinderCtx.extend id argStorage bodyCtx
        match bindOne bodyCtx body with 
        | Ok(Some boundBody) ->
            Fun(argStorage, boundBody) |> Some |> Ok
        | Ok None ->
            Error ($"Body has no value.", f.Syntax.Range)
        | e -> e
    | _ -> Error ($"Invalid lambda form %A{args}", f.Syntax.Range)

and private bindDefinition ctx f args =
    match args with
    | [ StxIdent(id, _); body ] ->
        match bindOne ctx body with
        | Ok(Some boundBody) ->
            let newStore = freshStorage id.Name
            BinderCtx.extend id newStore ctx
            Def(newStore, boundBody) |> Some |> Result.Ok
        | Ok None ->
            Error ($"Invalid `def` body: has no value.", f.Syntax.Range)
        | e -> e
    | _ -> Error ($"Invalid `def` form %A{args}", f.Syntax.Range)

and private bindQuotation f args =
    match args with
    | [ form ] -> Quot(strip form) |> Some |> Result.Ok
    | _ -> Error ($"Invalid `quot` form: %A{args}", f.Syntax.Range)

and private bindSyntaxQuotation f args =
    match args with
    | [ form ] -> Stx form |> Some |> Result.Ok
    | _ -> Error ($"Invalid `stx` form: %A{args}", f.Syntax.Range)

///
let rec private bindSequence (ctx: BindingContext) (exprs: Stx list) : Result<Bound list, string * Firethorn.TextRange> =
    match exprs with
    | [] -> Ok []
    | stx :: rest ->
        let bound = bindOne ctx stx
        let next = bindSequence ctx rest
        match bound with
        | Ok(Some b) ->
            next |> Result.map (fun bs -> b :: bs)
        | Ok None -> next
        | Error e -> Error e

/// Main binder entry point. Runs the binder over each input item
let public bind =
    let ctx = BinderCtx.empty
    bindSequence ctx
