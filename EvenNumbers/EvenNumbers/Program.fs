module EvenNumbers

/// count even numbers using filter
let countWithFilter list =
    list 
    |> List.filter (fun x -> x % 2 = 0) 
    |> List.length

/// count even numbers using map
let countWithMap list =
    list 
    |> List.map (fun x -> if x % 2 = 0 then 1 else 0)
    |> List.sum

/// count even numbers using fold
let countWithFold list =
    list 
    |> List.fold (fun acc x -> if x % 2 = 0 then acc + 1 else acc) 0