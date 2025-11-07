namespace LocalNetwork.Tests

open NUnit.Framework
open FsUnit
open Moq
open LocalNetwork

module TestHelpers =
    let connect (a: Computer) (b: Computer) =
        a.ConnectTo(b) |> ignore


[<TestFixture>]
type ``Computer tests`` () =

    [<Test>]
    member _.``TryInfect infects when random value < probability`` () =
        let os = { Name = "OS"; InfectionP = 0.7 }
        let pc = Computer(1, os, false)

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(0.3) |> ignore

        pc.TryInfect(rnd.Object)
        pc.IsInfected |> should be True

    [<Test>]
    member _.``TryInfect does not infect when random value >= probability`` () =
        let os = { Name = "OS"; InfectionP = 0.5 }
        let pc = Computer(1, os, false)

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(0.6) |> ignore

        pc.TryInfect(rnd.Object)
        pc.IsInfected |> should be False

    [<Test>]
    member _.``Already infected computer stays infected`` () =
        let os = { Name = "OS"; InfectionP = 0.0 }
        let pc = Computer(1, os, true)

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(1.0) |> ignore

        pc.TryInfect(rnd.Object)
        pc.IsInfected |> should be True


[<TestFixture>]
type ``Network infection propagation tests`` () =

    [<Test>]
    member _.``When p=1 infection spreads layer by layer (BFS)`` () =
        let os = { Name = "Any"; InfectionP = 1.0 }
        let a = Computer(1, os, true)
        let b = Computer(2, os, false)
        let c = Computer(3, os, false)
        let d = Computer(4, os, false)
        TestHelpers.connect a b
        TestHelpers.connect b c
        TestHelpers.connect c d

        let net = Network()
        [a; b; c; d] |> List.iter net.Add

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(0.0) |> ignore

        // step 1: A infects B
        net.NextTurn(rnd.Object) |> ignore
        [a; b; c; d] |> List.map (fun x -> x.IsInfected)
        |> should equal [true; true; false; false]

        // step 2: B infects C
        net.NextTurn(rnd.Object) |> ignore
        [a; b; c; d] |> List.map (fun x -> x.IsInfected)
        |> should equal [true; true; true; false]

        // step 3: C infects D
        net.NextTurn(rnd.Object) |> ignore
        [a; b; c; d] |> List.map (fun x -> x.IsInfected)
        |> should equal [true; true; true; true]

    [<Test>]
    member _.``When p=0 no new infections occur`` () =
        let os = { Name = "Any"; InfectionP = 0.0 }
        let a = Computer(1, os, true)
        let b = Computer(2, os, false)
        let c = Computer(3, os, false)
        TestHelpers.connect a b
        TestHelpers.connect b c

        let net = Network()
        [a; b; c] |> List.iter net.Add

        let rnd = Mock<IRandom>()

        rnd.Setup(fun r -> r.NextDouble()).Returns(0.0) |> ignore

        for _ in 1..5 do net.NextTurn(rnd.Object) |> ignore

        [a.IsInfected; b.IsInfected; c.IsInfected]
        |> should equal [true; false; false]

    [<Test>]
    member _.``Infection cannot jump over two edges in one turn`` () =
        let os = { Name = "Any"; InfectionP = 1.0 }
        let a = Computer(1, os, true)
        let b = Computer(2, os, false)
        let c = Computer(3, os, false)
        TestHelpers.connect a b
        TestHelpers.connect b c

        let net = Network()
        [a; b; c] |> List.iter net.Add

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(0.0) |> ignore

        net.NextTurn(rnd.Object) |> ignore
        (b.IsInfected, c.IsInfected) |> should equal (true, false)

        net.NextTurn(rnd.Object) |> ignore
        (b.IsInfected, c.IsInfected) |> should equal (true, true)

    [<Test>]
    member _.``FromAdjacency builds network properly and respects OS probabilities`` () =
        let os1 = { Name = "Windows"; InfectionP = 1.0 }
        let os2 = { Name = "Linux";   InfectionP = 0.0 }
        let os3 = { Name = "Mac";     InfectionP = 1.0 }

        let osMap = Map.ofList [1,os1; 2,os2; 3,os3]
        let infectedIds = set [1]
        let m = Array2D.create 3 3 false
        m[0, 1] <- true; m[1,2] <- true

        let net = Network.FromAdjacency(osMap, infectedIds, m)

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(0.0) |> ignore

        for _ in 1..3 do net.NextTurn(rnd.Object) |> ignore

        net.Computers
        |> Seq.map (fun c -> c.IsInfected)
        |> Seq.toList
        |> should equal [true; false; false]

    [<Test>]
    member _.``Isolated node never gets infected even with p=1`` () =
        let os = { Name = "Any"; InfectionP = 1.0 }
        let a = Computer(1, os, true)
        let b = Computer(2, os, false)
        let c = Computer(3, os, false)  // isolated

        TestHelpers.connect a b

        let net = Network()
        [a; b; c] |> List.iter net.Add

        let rnd = Mock<IRandom>()
        rnd.Setup(fun r -> r.NextDouble()).Returns(0.0) |> ignore

        for _ in 1..5 do net.NextTurn(rnd.Object) |> ignore

        c.IsInfected |> should equal false
