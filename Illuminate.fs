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

module private StxContext =

    /// An empty syntax context.
    let public empty = Unit

/// A node in the illuminated syntax tree
type public Stx =
    | StxAtom of Atom * StxContext
    | StxForm of Stx list * Form * StxContext

/// Illuminate a single expression. This attaches syntax context to atoms, and
/// recurses to process the elements of a form.
let rec private illumExpr (exp: Expression) : Stx =
    match exp with
    | Atom a -> StxAtom(a, StxContext.empty)
    | Form f -> StxForm((Seq.map illumExpr f.Body |> List.ofSeq), f, StxContext.empty)

/// Illuminate the given program `tree`.
let public illum (tree: Program) =
    tree.Body |> Seq.map (illumExpr) |> List.ofSeq
