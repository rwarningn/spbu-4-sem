module LambdaInterpreter.Tests

open NUnit.Framework
open FsUnit
open LambdaInterpreter

let var v = Var v
let abs x body = Abs(x, body)
let app t1 t2 = App(t1, t2)

let I = abs "x" (var "x")                                    
let K = abs "x" (abs "y" (var "x"))                          
let S = abs "x" (abs "y" (abs "z" 
    (app (app (var "x") (var "z")) (app (var "y") (var "z")))))  

[<TestFixture>]
type FreeVarsTests() =
    
    [<Test>]
    member _.``Free vars of single variable`` () =
        freeVars (var "x") |> should equal (Set.singleton "x")
    
    [<Test>]
    member _.``Free vars of abstraction with free variable`` () =
        freeVars (abs "x" (var "y")) |> should equal (Set.singleton "y")
    
    [<Test>]
    member _.``Free vars of abstraction with bound variable`` () =
        let result: Set<string> = freeVars (abs "x" (var "x"))
        let expected: Set<string> = Set.empty
        result |> should equal expected
    
    [<Test>]
    member _.``Free vars of identity combinator`` () =
        let result: Set<string> = freeVars I
        let expected: Set<string> = Set.empty
        result |> should equal expected

[<TestFixture>]
type SubstituteTests() =
    
    [<Test>]
    member _.``Substitute variable with variable`` () =
        substitute (var "x") "x" (var "y")
        |> should equal (var "y")
    
    [<Test>]
    member _.``Don't substitute bound variable`` () =
        substitute (abs "x" (var "x")) "x" (var "y")
        |> should equal (abs "x" (var "x"))
    
    [<Test>]
    member _.``Substitute with no capture`` () =
        substitute (abs "y" (var "x")) "x" (var "z")
        |> should equal (abs "y" (var "z"))
    
    [<Test>]
    member _.``Alpha conversion to avoid capture`` () =
        let result = substitute (abs "y" (var "x")) "x" (var "y")
        match result with
        | Abs(v, Var v') -> 
            v' |> should equal "y"
            v |> should not' (equal "y")
        | _ -> 
            failwith "expected Abs with renamed variable"

[<TestFixture>]
type BetaReductionTests() =
    
    [<Test>]
    member _.``Identity application`` () =
        normalize (app I (var "x")) |> should equal (var "x")
    
    [<Test>]
    member _.``K combinator`` () =
        normalize (app (app K (var "x")) (var "y"))
        |> should equal (var "x")
    
    [<Test>]
    member _.``SKK = I`` () =
        let skk = app (app S K) K
        let result = normalize skk
        normalize (app result (var "z")) |> should equal (var "z")
    
    [<Test>]
    member _.``No reduction for normal form`` () =
        betaStep I |> should equal None
    
    [<Test>]
    member _.``Simple beta reduction`` () =
        betaStep (app (abs "x" (var "x")) (var "y"))
        |> should equal (Some (var "y"))
    
    [<Test>]
    member _.``Alpha conversion during reduction`` () =
        let term = app (abs "x" (abs "y" (var "x"))) (var "y")
        let result = normalize term
        match result with
        | Abs(v, Var "y") -> 
            v |> should not' (equal "y")
        | _ -> 
            failwith "expected Abs with alpha-converted variable"