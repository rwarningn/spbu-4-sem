module KR.Tests

open NUnit.Framework
open FsUnit
open KR


// tests for repeatingNumbersSeq
[<Test>]
let ``repeatingNumbersSeq - first 15 items match the spec`` () =
    repeatingNumbersSeq
    |> Seq.take 15
    |> Seq.toList
    |> should equal [1; 2;2; 3;3;3; 4;4;4;4; 5;5;5;5;5]

[<Test>]
let ``The element is correct after changing the number`` () =
    let seventhElement = repeatingNumbersSeq |> Seq.item 6
    seventhElement |> should equal 4

// test for findLargestPalindrome
[<Test>]
let ``The largest palindrome from the product of two 3-digit numbers should be found correctly``() =
        let expected = 906609       
        let actual = findLargestPalindrome ()
        actual |> should equal expected
