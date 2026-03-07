/// Syntax Illumination
///
/// Converts a raw `Syntax` tree into an "illuminated" form where each syntax
/// item references a scope in a shared side table. Illuminated trees are used
/// by the later `Expand` and `Bind` phases.
///
/// Scope information (renamings and marks) is stored in a central `ScopeTable`
/// rather than inline on each node. This gives O(1) cached resolution, lets
/// many nodes share a single scope without duplicating context chains, and
/// provides a natural extension point for mark-based macro expansion.
module Mutton.Illuminate

open Mutton.Syntax

/// Identifier for a scope in the scope table. Each syntax node references
/// its enclosing scope by this ID rather than carrying an inline context chain.
type public ScopeId = int

/// An entry in the scope side table.
type internal ScopeEntry =
    | Rename of before: string * after: string * parent: ScopeId
    | Mark of mark: string * parent: ScopeId
    | Root

/// Side table storing all scope entries. Provides O(1) amortised identifier
/// resolution via caching, and lets many syntax nodes share a single scope
/// without duplicating context chains.
type public ScopeTable() =
    let entries = System.Collections.Generic.List<ScopeEntry>()
    let cache = System.Collections.Generic.Dictionary<struct (int * string), string>()

    do entries.Add(Root) // index 0 is the root scope

    /// The root scope (no renames, no marks).
    member _.Root: ScopeId = 0

    /// Create a new scope that renames `before` to `after`, parented to `parent`.
    member _.AddRename(before: string, after: string, parent: ScopeId) : ScopeId =
        let id = entries.Count
        entries.Add(Rename(before, after, parent))
        id

    /// Create a new scope that adds a mark, parented to `parent`.
    member _.AddMark(mark: string, parent: ScopeId) : ScopeId =
        let id = entries.Count
        entries.Add(Mark(mark, parent))
        id

    /// Resolve an identifier through the scope chain. Results are cached
    /// so repeated lookups are O(1) amortised.
    member this.Resolve(id: string, scope: ScopeId) : string =
        let key = struct (scope, id)

        match cache.TryGetValue(key) with
        | true, v -> v
        | false, _ ->
            let result = this.Walk(id, scope)
            cache.[key] <- result
            result

    member private this.Walk(id, scope) =
        match entries.[scope] with
        | Root -> id
        | Mark(_, parent) -> this.Resolve(id, parent)
        | Rename(before, after, parent) ->
            if before = id then
                this.Resolve(after, parent)
            else
                this.Resolve(id, parent)

/// A node in the illuminated syntax tree. Each node carries a `ScopeId`
/// referencing its scope in the shared `ScopeTable`.
type public Stx =
    | StxLiteral of Literal * ScopeId
    | StxIdent of string * Symbol * ScopeId
    | StxForm of Stx list * Form * ScopeId

/// Illuminate a single expression. This attaches the root scope to atoms, and
/// recurses to process the elements of a form.
let rec private illumExpr (scopes: ScopeTable) (exp: Expression) : Stx =
    match exp with
    | Symbol s ->
        // FIXME: We shouldn't have to call `.Value` here, or we should handle it.
        StxIdent(s.Value.Value, s, scopes.Root)
    | Literal l -> StxLiteral(l, scopes.Root)
    | Form f -> StxForm((Seq.map (illumExpr scopes) f.Body |> List.ofSeq), f, scopes.Root)

/// Illuminate the given program `tree`. Returns the illuminated syntax along
/// with the scope table used for resolution.
let public illum (tree: Program) : Stx list * ScopeTable =
    let scopes = ScopeTable()
    let stx = tree.Body |> Seq.map (illumExpr scopes) |> List.ofSeq
    (stx, scopes)


let mutable private idSuffix = ref 0

/// Generate a fresh symbol from the given ID hint
let public freshId (idHint: Symbol) =
    let suffix = System.Threading.Interlocked.Increment(idSuffix)
    let prefix = idHint.Value |> Option.defaultValue "tmp"
    $"{prefix}.{suffix}"
