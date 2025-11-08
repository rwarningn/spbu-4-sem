module WorkflowTests

open Workflow
open NUnit.Framework
open FsCheck
open FsUnit


[<TestFixture>]
type ``String calculator workflow tests`` () =

    [<Test>]
    member _.``Parses valid numbers correctly`` () =
        let result =
            calculator {
                let! a = "10"
                let! b = "20"
                return (a + b)
            }
        result |> should equal (Some 30)

    [<Test>]
    member _.``Returns None for invalid input`` () =
        let result =
            calculator {
                let! a = "5"
                let! b = "xyz"
                return a + b
            }
        result |> should equal None

    [<Test>]
    member _.``Performs multi-step calculation`` () =
        let result =
            calculator {
                let! a = "2"
                let! b = "3"
                let! c = "4"
                return (a + b) * c
            }
        result |> should equal (Some 20)


[<TestFixture>]
type ``Rounding workflow tests`` () =

    [<Test>]
    member _.``Basic example from assignment`` () =
        let result =
            rounding 3 {
                let! a = 2.0 / 12.0
                let! b = 3.5
                return a / b
            }
        result |> should (equalWithin 1e-12) 0.048

    [<Test>]
    member _.``Zero precision rounds to integers`` () =
        let result =
            rounding 0 {
                let! a = 3.1415
                let! b = 2.71828
                return a + b
            }
        result |> should equal 6.0

    [<Test>]
    member _.``Intermediate rounding affects result`` () =
        let result =
            rounding 1 {
                let! a = 10.0 / 3.0
                let! b = a * 2.0
                let! c = b + 1.5
                return c
            }
        result |> should (equalWithin 1e-12) 8.1