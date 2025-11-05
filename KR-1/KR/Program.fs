module KR

open System
open System.Threading

/// creates an infinitive sequence of numbers where each number is repeated n times
let repeatingNumbersSeq = 
    Seq.initInfinite (fun i -> i + 1)
    |> Seq.collect (fun n -> Seq.replicate n n)


/// checks if a number is palindrome
let private isPalindrome (n: int) =
    let s = string n
    s = (s |> Seq.toList |> List.rev |> System.String.Concat)

/// finds largest palindrome of product of two 3-digit numbers
let findLargestPalindrome () =
    seq { for i in 100..999 do
          for j in i..999 do
          yield i * j }
    |> Seq.filter isPalindrome
    |> Seq.max
