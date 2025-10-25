module ListPower.Tests

open NUnit.Framework
open FsUnit
open ListPower

[<Test>]
let ``Power list with n=0, m=0 returns [1]`` () =
    let result = powerList 0 0
    result |> should equal [1I]

[<Test>]
let ``Power list with n=0, m=3`` () =
    let result = powerList 0 3
    result |> should equal [1I; 2I; 4I; 8I]

[<Test>]
let ``Power list with n=2, m=3`` () =
    let result = powerList 2 3
    result |> should equal [4I; 8I; 16I; 32I]

[<Test>]
let ``Power list with n=3, m=4`` () =
    let result = powerList 3 4
    result |> should equal [8I; 16I; 32I; 64I; 128I]

[<Test>]
let ``Power list with n=5, m=2`` () =
    let result = powerList 5 2
    result |> should equal [32I; 64I; 128I]

[<Test>]
let ``Power list with m=0 returns single element`` () =
    let result = powerList 10 0
    result |> should equal [1024I]

[<Test>]
let ``Power list with negative m returns empty list`` () =
    let result = powerList 5 -3
    result |> should be Empty

[<Test>]
let ``Power list with large n`` () =
    let result = powerList 10 3
    result |> should equal [1024I; 2048I; 4096I; 8192I]

[<Test>]
let ``Power list with n=1, m=5`` () =
    let result = powerList 1 5
    result |> should equal [2I; 4I; 8I; 16I; 32I; 64I]

[<Test>]
let ``Power list length is m+1`` () =
    let result = powerList 3 7
    result |> should haveLength 8

[<Test>]
let ``Power list elements are in ascending order`` () =
    let result = powerList 2 5
    result |> should equal (List.sort result)
