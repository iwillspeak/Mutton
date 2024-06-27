module Mutton.Binder

open Illuminate
open Mutton.Syntax

type Bound =
    | Number of int
    | Var of string
    | App of Bound * Bound List
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
    | StxForm(items, syntax, _) ->
        match items with
        | [] -> 
            // TODO: Erorr to have no items in application
            Error
        | applicant::args ->
            let called = bindOne applicant
            let args = List.map (bindOne) args
            App(called, args)

let public bind = List.map bindOne
