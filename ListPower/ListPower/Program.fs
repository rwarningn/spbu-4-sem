module ListPower

/// returns a list [2^n; 2^(n+1); ...; 2^(n+m)]
let powerList n m =
    match m with
    | m when m < 0 -> []
    | _ ->
        let rec buildList current count acc =
            match count with
            | 0 -> List.rev (current :: acc)
            | _ -> buildList (current * 2I) (count - 1) (current :: acc)
        buildList (1I <<< n) m []