module NumberSearch

/// returns the index of the first occurrence of the given element in the list
let findFirst list element =
    let rec find list element index =
        match list with
        | [] -> None
        | head :: tail when head = element -> Some index
        | _ :: tail -> find tail element (index + 1)
    find list element 0