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

/// The context metadata attached to a given syntax item.
type public StxContext =
    | Unit of Stamp

/// A node in the illuminated syntax tree
type public Stx =
    | StxLiteral of Literal * StxContext
    | StxIdent of string * Symbol * StxContext
    | StxForm of Stx list * Form * StxContext

let mutable private stamp = ref 0

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
        StxIdent(s.Value.Value, s, StxContext.Unit stamp)
    | Literal l -> StxLiteral(l, StxContext.Unit stamp)
    | Form f -> StxForm((Seq.map (illumExpr stamp) f.Body |> List.ofSeq), f, StxContext.Unit stamp)

/// Illuminate the given program `tree`.
let public illum (tree: Program) =
    let initialStamp = newStamp ()
    tree.Body |> Seq.map (illumExpr initialStamp) |> List.ofSeq

/// Resolve the identifier in the given syntax context. Returns the canonical
/// form of this identifier at that soruce location.
let rec public resolve id =
    function
    | _ -> id

