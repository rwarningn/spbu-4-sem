module ListReversal

///  function that reverses a list takes a list of integers as a parameter
let reverse list =
    let rec reverseAcc acc list =
        match list with
        | [] -> acc
        | head :: tail -> reverseAcc (head :: acc) tail
    reverseAcc [] list