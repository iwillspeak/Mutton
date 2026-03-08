/// Syntax Illumination
///
/// Converts a raw `Syntax` tree into an "illuminated" form where each syntax
/// item contains a marker used to indicate its "source provenance". This
/// information is used by the later `Expand` and `Bind` phases to perform
/// hygienic macro expansion and name resolution.
module Mutton.Illuminate

open Mutton.Syntax

/// An opaque marker. Added to items in the Stx treeto track their source
/// provenance. This is used for hygiene and name resolution.
type Stamp = int

type Ident = { Name: string; Stamp: Stamp }

/// A node in the illuminated syntax tree
type public Stx =
    | StxLiteral of Literal
    | StxIdent of Ident * Symbol
    | StxForm of Stx list * Form
    | StxClosure of Stx * StxEnv

// A syntax environment maps names to syntax bindings.
and StxEnv = Map<string, StxBinding>

// Syntax bindings are either to identifiers, or syntax items
and StxBinding =
    | Var of Ident
    | Macro of Transformer

// A tranformer takes a syntax item and its use-site environment, and produces a
// new syntax item. This is the basis of macro expansion
and Transformer = Stx -> StxEnv -> Stx

/// A stamp marking a distinct syntax context.
let mutable private stamp = ref 1

/// Generate a fresh stamp for tracking syntax provenance
let newStamp () =
    let s = System.Threading.Interlocked.Increment(stamp)
    s

/// Illuminate a single expression. This attaches syntax context to atoms, and
/// recurses to process the elements of a form.
let rec private illumExpr stamp (exp: Expression) : Stx =
    match exp with
    | Symbol s ->
        // FIXME: We shouldn't have to call `.Value` here, or we should handle it.
        let name = s.Value.Value
        StxIdent({ Name = name; Stamp = stamp }, s)
    | Literal l -> StxLiteral l
    | Form f -> StxForm(Seq.map (illumExpr stamp) f.Body |> List.ofSeq, f)

/// Illuminate the given program `tree`.
let public illum (tree: Program) =
    // Fresh source programs are given the stamp 0. This will not collide
    // with any synthetic stamps produced later by `newStamp`.
    tree.Body |> Seq.map (illumExpr 0) |> List.ofSeq
