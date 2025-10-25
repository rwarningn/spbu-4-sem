module TreeMap

/// binary tree type
type BinaryTree<'a> =
    | Node of 'a * BinaryTree<'a> * BinaryTree<'a>
    | Empty

/// map with linearization - returns new tree
let mapTree f tree =
    let rec map tree cont =
        match tree with
        | Empty -> cont Empty
        | Node(value, left, right) ->
            map left (fun mappedLeft ->
                map right (fun mappedRight ->
                    cont (Node(f value, mappedLeft, mappedRight))))
    map tree id