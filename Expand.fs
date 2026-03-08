module Mutton.Expand

open Illuminate
open Mutton.Syntax

let mutable private idSuffix = ref 0

/// Generate a fresh symbol from the given ID hint
let public freshId (idHint: Symbol) =
    let suffix = System.Threading.Interlocked.Increment(idSuffix)
    let prefix = idHint.Value |> Option.defaultValue "tmp"
    $"{prefix}.{suffix}"

/// Expand any syntax mutations (macros) within the given tree. Returns the
/// input tree with all syntax lowered.
let public expand = id

