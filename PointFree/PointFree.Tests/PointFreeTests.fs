module PointFree.Tests      

open NUnit.Framework
open FsUnit
open FsCheck
open PointFree

[<Test>]
let ``func1 simple example`` () =
    func1 2 [1; 2; 3] |> should equal [2; 4; 6]

[<Test>]
let ``func2 simple example`` () =
    func2 2 [1; 2; 3] |> should equal [2; 4; 6]

[<Test>]
let ``func3 simple example`` () =
    func3 2 [1; 2; 3] |> should equal [2; 4; 6]

[<Test>]
let ``func4 simple example`` () =
    func4 2 [1; 2; 3] |> should equal [2; 4; 6]

[<Test>]
let ``func5 simple example`` () =
    func5 2 [1; 2; 3] |> should equal [2; 4; 6]

[<Test>]
let ``FsCheck: all functions are equivalent`` () =
    let property (x: int) (list: int list) =
        let r1 = func1 x list
        let r2 = func2 x list
        let r3 = func3 x list
        let r4 = func4 x list
        let r5 = func5 x list
        r1 = r2 && r2 = r3 && r3 = r4 && r4 = r5
    Check.QuickThrowOnFailure property