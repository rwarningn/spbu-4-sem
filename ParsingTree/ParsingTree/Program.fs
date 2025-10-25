module ParsingTree

/// expression type
type Expression =
    | Const of float
    | Plus of Expression * Expression
    | Minus of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | UMinus of Expression


/// returns None if division by zero occurs
let eval expression =
    let eps = 1e-10
    
    let rec evalCPS expr cont =
        match expr with
        | Const c -> cont (Some c)
        | UMinus e ->
            evalCPS e (fun result ->
                match result with
                | Some value -> cont (Some (-value))
                | None -> cont None)
        | Plus(left, right) ->
            evalCPS left (fun leftResult ->
                match leftResult with
                | None -> cont None
                | Some leftValue ->
                    evalCPS right (fun rightResult ->
                        match rightResult with
                        | None -> cont None
                        | Some rightValue -> cont (Some (leftValue + rightValue))))
        | Minus(left, right) ->
            evalCPS left (fun leftResult ->
                match leftResult with
                | None -> cont None
                | Some leftValue ->
                    evalCPS right (fun rightResult ->
                        match rightResult with
                        | None -> cont None
                        | Some rightValue -> cont (Some (leftValue - rightValue))))
        | Mul(left, right) ->
            evalCPS left (fun leftResult ->
                match leftResult with
                | None -> cont None
                | Some leftValue ->
                    evalCPS right (fun rightResult ->
                        match rightResult with
                        | None -> cont None
                        | Some rightValue -> cont (Some (leftValue * rightValue))))
        | Div(left, right) ->
            evalCPS left (fun leftResult ->
                match leftResult with
                | None -> cont None
                | Some leftValue ->
                    evalCPS right (fun rightResult ->
                        match rightResult with
                        | None -> cont None
                        | Some rightValue ->
                            if abs rightValue = 0.0 then 
                                cont None // division by zero
                            else
                                cont (Some (leftValue / rightValue))))
    
    evalCPS expression id