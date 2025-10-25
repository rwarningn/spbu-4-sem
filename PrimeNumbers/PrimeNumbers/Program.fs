module PrimeNumbers

/// checks if a number is prime
let isPrime n =
    match n with
    | n when n < 2 -> false
    | 2 -> true
    | n when n % 2 = 0 -> false
    | _ ->
        let rec checkDivisors divisor =
            if divisor * divisor > n then
                true
            elif n % divisor = 0 then
                false
            else
                checkDivisors (divisor + 2)
        checkDivisors 3

/// infinite sequence of prime numbers
let primes =
    Seq.initInfinite id
    |> Seq.skip 2
    |> Seq.filter isPrime