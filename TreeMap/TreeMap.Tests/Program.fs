module TreeMap.Tests

open NUnit.Framework
open FsUnit
open TreeMap

[<Test>]
let ``mapTree should map empty tree to empty tree`` () =
    let tree: BinaryTree<int> = Empty 
    let result = mapTree (fun x -> x * 2) tree
    let expected: BinaryTree<int> = Empty
    result |> should equal expected

[<Test>]
let ``mapTree should map single node`` () =
    let tree = Node(5, Empty, Empty)
    let result = mapTree (fun x -> x * 2) tree
    result |> should equal (Node(10, Empty, Empty))

[<Test>]
let ``mapTree should map simple tree`` () =
    let tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let result = mapTree (fun x -> x + 10) tree
    result |> should equal (Node(11, Node(12, Empty, Empty), Node(13, Empty, Empty)))

[<Test>]
let ``mapTree should map nested tree`` () =
    let tree = 
        Node(1,
            Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)),
            Node(3, Node(6, Empty, Empty), Node(7, Empty, Empty)))
    let result = mapTree (fun x -> x * 2) tree
    result |> should equal (
        Node(2,
            Node(4, Node(8, Empty, Empty), Node(10, Empty, Empty)),
            Node(6, Node(12, Empty, Empty), Node(14, Empty, Empty))))

[<Test>]
let ``mapTree should handle left-heavy tree`` () =
    let tree = 
        Node(1,
            Node(2, Node(3, Node(4, Empty, Empty), Empty), Empty),
            Empty)
    let result = mapTree (fun x -> x * 3) tree
    result |> should equal (
        Node(3,
            Node(6, Node(9, Node(12, Empty, Empty), Empty), Empty),
            Empty))

[<Test>]
let ``mapTree should handle right-heavy tree`` () =
    let tree = 
        Node(1,
            Empty,
            Node(2, Empty, Node(3, Empty, Node(4, Empty, Empty))))
    let result = mapTree (fun x -> x + 5) tree
    result |> should equal (
        Node(6,
            Empty,
            Node(7, Empty, Node(8, Empty, Node(9, Empty, Empty)))))

[<Test>]
let ``mapTree with strings`` () =
    let tree = Node("hello", Node("world", Empty, Empty), Node("test", Empty, Empty))
    let result = mapTree (fun (s: string) -> s.ToUpper()) tree
    let expected: BinaryTree<string> = 
        Node("HELLO", Node("WORLD", Empty, Empty), Node("TEST", Empty, Empty))
    result |> should equal expected

[<Test>]
let ``mapTree with type conversion`` () =
    let tree = Node(1, Node(2, Empty, Empty), Node(3, Empty, Empty))
    let result = mapTree (fun x -> string x) tree
    result |> should equal (Node("1", Node("2", Empty, Empty), Node("3", Empty, Empty)))

[<Test>]
let ``mapTree with complex function`` () =
    let tree = Node(5, Node(3, Empty, Empty), Node(7, Empty, Empty))
    let result = mapTree (fun x -> if x % 2 = 0 then x * 2 else x + 1) tree
    result |> should equal (Node(6, Node(4, Empty, Empty), Node(8, Empty, Empty)))

[<Test>]
let ``mapTree with balanced tree`` () =
    let tree = 
        Node(10,
            Node(5, Node(3, Empty, Empty), Node(7, Empty, Empty)),
            Node(15, Node(12, Empty, Empty), Node(20, Empty, Empty)))
    let result = mapTree (fun x -> x / 5) tree
    result |> should equal (
        Node(2,
            Node(1, Node(0, Empty, Empty), Node(1, Empty, Empty)),
            Node(3, Node(2, Empty, Empty), Node(4, Empty, Empty))))

[<EntryPoint>]
let main _ = 0
