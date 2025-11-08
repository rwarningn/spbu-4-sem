module Workflow

open System

// workflow for string-to-int calculations
type StringCalculatorBuilder() =
    member _.Bind(x: string, f: int -> int option) : int option =
        match Int32.TryParse x with
        | true, value -> f value
        | false, _ -> None

    member _.Return(x: int) : int option =
        Some x

let calculator = StringCalculatorBuilder()


// workflow that performs intermediate rounding to given accuracy
type RoundingBuilder(acc: int) =
    member _.Bind(x: float, f: float -> float) : float =
        let rounded = Math.Round(x, acc)
        f rounded

    member _.Return(x: float) : float =
        Math.Round(x, acc)

let rounding acc = RoundingBuilder(acc)