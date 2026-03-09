module Mutton.Utils

/// Pull the `Ok` values out of a list of `Result`s, or return the first `Error` if any are present.
let accumulateResults (results: Result<'a, 'e> list) : Result<'a list, 'e> =
    let rec loop acc =
        function
        | [] -> Ok(List.rev acc)
        | r :: rs ->
            match r with
            | Ok v -> loop (v :: acc) rs
            | Error e -> Error e

    loop [] results
