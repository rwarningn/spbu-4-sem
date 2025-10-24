module Factorial


/// calculates factorial
let factorial n =
    let rec calculate n acc =
        match n with
        | _ when n < 0 -> None
        | 0 | 1 -> Some acc
        | _ -> calculate (n - 1) (acc * bigint n)
    
    calculate n 1I