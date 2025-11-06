module PrimeNumbers.Tests

open NUnit.Framework
open FsUnit
open PrimeNumbers

[<TestCase(2, true)>]
[<TestCase(3, true)>]
[<TestCase(4, false)>]
[<TestCase(5, true)>]
[<TestCase(10, false)>]
[<TestCase(11, true)>]
[<TestCase(1, false)>]
[<TestCase(0, false)>]
[<TestCase(-5, false)>]
[<TestCase(97, true)>]
[<TestCase(100, false)>]
let ``isPrime returns correct result`` (n: int) (expected: bool) =
    isPrime n |> should equal expected

[<Test>]
let ``First 10 primes are correct`` () =
    let expected = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
    let actual = primes |> Seq.take 10 |> Seq.toList
    actual |> should equal expected

[<Test>]
let ``First 20 primes are correct`` () =
    let expected = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29; 31; 37; 41; 43; 47; 53; 59; 61; 67; 71]
    let actual = primes |> Seq.take 20 |> Seq.toList
    actual |> should equal expected

[<Test>]
let ``Can get 100th prime`` () =
    let prime100 = primes |> Seq.item 99  
    prime100 |> should equal 541

[<Test>]
let ``Primes sequence is infinite`` () =
    let prime1000 = primes |> Seq.item 999
    prime1000 |> should equal 7919

[<Test>]
let ``All elements in sequence are prime`` () =
    let first50 = primes |> Seq.take 50 |> Seq.toList
    first50 |> List.forall isPrime |> should equal true

[<Test>]
let ``Primes sequence has no duplicates`` () =
    let first100 = primes |> Seq.take 100 |> Seq.toList
    let distinct = first100 |> List.distinct
    first100 |> should equal distinct

[<Test>]
let ``Primes are in ascending order`` () =
    let first50 = primes |> Seq.take 50 |> Seq.toList
    first50 |> should equal (List.sort first50)