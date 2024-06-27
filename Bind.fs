module Mutton.Binder

open Illuminate
open Mutton.Syntax

type Bound =
    | Number of int
    | Var of string
    | Error

let rec private bindOne =
    function
    | StxAtom(a, _) ->
        match a.Value with
        | Some(NumberValue n) -> Number n.Inner
        | Some(SymValue s) -> 
            // FIXME: This should map the identifier somehow?
            Var s.Identifier
        | _ ->
            // TODO: Implement binder erorrs. This is the case of a malformed
            //       or missing atom value. Most likely as the result of a
            //       parse error.
            Error
    | _ ->
        // TODO: Binding of special forms and applications
        Error

let public bind = List.map bindOne
