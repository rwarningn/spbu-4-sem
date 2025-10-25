module PrimeNumbers.Tests

open NUnit.Framework
open FsUnit
open PrimeNumbers

[<Test>]
let ``2 is a prime number`` () =
    isPrime 2 |> should equal true

[<Test>]
let ``3 is a prime number`` () =
    isPrime 3 |> should equal true

[<Test>]
let ``4 is not a prime number`` () =
    isPrime 4 |> should equal false

[<Test>]
let ``5 is a prime number`` () =
    isPrime 5 |> should equal true

[<Test>]
let ``10 is not a prime number`` () =
    isPrime 10 |> should equal false

[<Test>]
let ``11 is a prime number`` () =
    isPrime 11 |> should equal true

[<Test>]
let ``1 is not a prime number`` () =
    isPrime 1 |> should equal false

[<Test>]
let ``0 is not a prime number`` () =
    isPrime 0 |> should equal false

[<Test>]
let ``Negative numbers are not prime`` () =
    isPrime -5 |> should equal false

[<Test>]
let ``Large prime 97 is prime`` () =
    isPrime 97 |> should equal true

[<Test>]
let ``Large non-prime 100 is not prime`` () =
    isPrime 100 |> should equal false

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