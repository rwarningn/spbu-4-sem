module Fibonacci


/// Сomputes n-th fibonacci number
let fibonacci n =
    let rec calculate n a b =
        match n with
        | 0 -> Ok a
        | 1 -> Ok b
        | _ when n > 1 -> calculate (n - 1) b (a + b) 
        | _ -> Error "n must be non-negative integer"
    calculate n 0I 1I