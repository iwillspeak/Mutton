/// Syntax Illumination
///
/// Covnerts a raw `Syntax` tree into an "illuminated" form where each syntax
/// item has an attached `SyntaxContext`. Illuminated trees are used by the#
/// later `Expand` and `Bind` phases.
module Mutton.Illuminate

open Mutton.Syntax

/// The context metadata attached to a given syntax item.
type public StxContext =
    | Remapped of string * string * StxContext
    | Marked of string * StxContext
    | Unit

module public StxContext =

    /// An empty syntax context.
    let public empty = Unit

/// A node in the illuminated syntax tree
type public Stx =
    | StxLiteral of Literal * StxContext
    | StxIdent of string * Symbol * StxContext
    | StxForm of Stx list * Form * StxContext

/// Illuminate a single expression. This attaches syntax context to atoms, and
/// recurses to process the elements of a form.
let rec private illumExpr (exp: Expression) : Stx =
    match exp with
    | Symbol s ->
        // FIXME: We shouldn't have to call `.Value` here, or we should handle it.
        StxIdent(s.Value.Value, s, StxContext.empty)
    | Literal l -> StxLiteral(l, StxContext.empty)
    | Form f -> StxForm((Seq.map illumExpr f.Body |> List.ofSeq), f, StxContext.empty)

/// Illuminate the given program `tree`.
let public illum (tree: Program) =
    tree.Body |> Seq.map (illumExpr) |> List.ofSeq

/// Resolve the identifier in the given syntax context. Returns the canonical
/// form of this identifier at that soruce location.
let public resolve id ctx = id

let mutable private idSuffix = ref 0

/// Generate a fresh symbol from the given ID hint
let public freshId (idHint: Symbol) =
    let suffix = System.Threading.Interlocked.Increment(idSuffix)
    let prefix = idHint.Value |> Option.defaultValue "tmp"
    $"{prefix}.{suffix}"
