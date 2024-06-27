module Mutton.Binder

open Mutton.Illuminate
open Mutton.Syntax

/// Bound Expression Node
///
/// Each variant represents a different kind of bound node available in the
/// tree. Bound nodes represent the _semantic_ structure of the program rather
/// than the syntactic one. Any symbols in the program are resolved to their
/// appropriate storage locations.
type Bound =
    | Number of int
    | Var of string
    | App of Bound * Bound List
    | Fun of string * Bound
    | Error

// FIXME: This should map the identifier somehow?
let private resolve = id

/// Bind a single expression
let rec private bindOne =
    function
    | StxAtom(a, _) ->
        match a.Value with
        | Some(NumberValue n) -> Number n.Inner
        | Some(SymValue s) ->
            // FIXME: This should be an error. Symbols and atoms need to be
            //        distinct. Should we have a Literal + Symbol as different
            //        parse items?
            Var(resolve s.Identifier)
        | _ ->
            // TODO: Implement binder erorrs. This is the case of a malformed
            //       or missing atom value. Most likely as the result of a
            //       parse error.
            Error
    | StxIdent(id, _, _) -> Var(resolve id)
    | StxForm(items, syntax, _) ->
        match items with
        | [] ->
            // TODO: Error to have no items in application
            Error
        | applicant :: args ->
            let bindSipleApp () =
                let called = bindOne applicant
                let args = List.map (bindOne) args
                App(called, args)

            match applicant with
            | StxIdent(id, _, _) ->
                match resolve id with
                | "lam" -> bindLambda args
                // TODO: Recognise more special forms here.
                | _ -> bindSipleApp ()
            | _ -> bindSipleApp ()

and private bindLambda args =
    match args with
    | [ StxIdent(id, _, _); body ] -> Fun((resolve id), (bindOne body))
    | _ -> Error

/// Main binder entry point. Runs the binder over each input item
let public bind = List.map bindOne
