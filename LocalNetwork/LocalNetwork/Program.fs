namespace LocalNetwork

open System
open System.Collections.Generic

/// interface to allow mocking randomness in tests
type IRandom =
    abstract member NextDouble: unit -> float

/// adapter to System.Random (for real runs)
type RandomAdapter (?seed: int) =
    let rnd =
        match seed with
        | Some s -> Random s
        | None -> Random.Shared
    interface IRandom with
        member _.NextDouble () = rnd.NextDouble ()

/// os descriptor: name and infection probability
type OS =
  { Name: string
    InfectionP: float }

/// a computer in local network
type Computer (id: int, os: OS, initInfected: bool) =
    // adjacency list
    let links = ResizeArray<Computer> ()
    // infection flag
    let mutable infected = initInfected

    member _.Id = id
    member _.OS = os
    member _.IsInfected
        with get () = infected
        and private set v = infected <- v

    /// neighbors
    member _.Links : IReadOnlyList<Computer> = upcast links

    /// connect to another computer (undirected by default)
    member this.ConnectTo (other: Computer, ?bidirectional: bool) =
        let bi = defaultArg bidirectional true
        if not (obj.ReferenceEquals (this, other)) then
            links.Add other
            if bi then other.ConnectTo (this, bidirectional = false)

    /// try to infect this computer once
    member this.TryInfect (rnd: IRandom) =
        if not this.IsInfected && rnd.NextDouble () < os.InfectionP then
            this.IsInfected <- true

    override _.ToString () =
        let state = if infected then "INFECTED" else "HEALTHY"
        $"PC {id}: {state} ({os.Name})"


/// local network that evolves in steps
type Network () =
    let computers = ResizeArray<Computer> ()

    /// add machine to network
    member _.Add (pc: Computer) = computers.Add pc
    member _.Computers : IReadOnlyList<Computer> = upcast computers

    /// count of infected comps now
    member _.InfectedCount () =
        computers |> Seq.filter (fun c -> c.IsInfected) |> Seq.length

    /// one discrete step
    member _.NextTurn (rnd: IRandom) : bool =
        let infectedNowIds =
            computers
            |> Seq.filter (fun c -> c.IsInfected)
            |> Seq.map (fun c -> c.Id)
            |> Set.ofSeq

        // healthy computers with >=1 infected neighbor
        let candidates =
            computers
            |> Seq.filter (fun c ->
                not c.IsInfected &&
                (c.Links |> Seq.exists (fun n -> infectedNowIds.Contains n.Id)))
            |> Seq.toArray

        // attempt infections
        let mutable changed = false
        for c in candidates do
            let was = c.IsInfected
            c.TryInfect rnd
            if not was && c.IsInfected then changed <- true
        changed

    member _.PrintState () =
        for c in computers do
            printfn "%O" c

    /// run simulation
    member this.Run (rnd: IRandom) =
        printfn "Start:"
        this.PrintState ()
        let mutable changed = true
        let mutable step = 0
        while changed && this.InfectedCount () > 0 do
            step <- step + 1
            changed <- this.NextTurn rnd
            printfn "Step %d:" step
            this.PrintState ()

    /// build network from adjacency matrix
    static member FromAdjacency (osById: Map<int, OS>, infectedIds: Set<int>, matrix: bool[,]) =
        let n = matrix.GetLength 0
        if n <> matrix.GetLength 1 then
            invalidArg "matrix" "Matrix must be square"

        // create computers with 1-based IDs
        let pcs =
            [|
                for i in 0 .. n - 1 do
                    let id = i + 1
                    let os =
                        defaultArg (osById.TryFind id) { Name = "Unknown"; InfectionP = 0.0 }
                    let inf = infectedIds.Contains id
                    Computer (id, os, inf)
            |]

        // add undirected edges
        for i in 0 .. n - 1 do
            for j in i + 1 .. n - 1 do
                if matrix[i, j] then
                    pcs[i].ConnectTo (pcs[j]) |> ignore

        let net = Network ()
        pcs |> Array.iter net.Add
        net