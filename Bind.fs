module Mutton.Binder

open Illuminate
open Mutton.Syntax

type Bound =
    | Number of int
    | Error

let rec private bindOne =
    function
    | StxAtom(a, _) ->
        match a.Value with
        | Some(NumberValue n) -> Number n.Inner
        | Some(SymValue _) -> Error
        | _ -> Error
    | _ -> Error

let public bind = List.map bindOne
