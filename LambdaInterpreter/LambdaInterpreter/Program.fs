module LambdaInterpreter

/// lambda term
type Term =
    | Var of string              
    | Abs of string * Term      
    | App of Term * Term        

/// returns set of free variables in term
let rec freeVars term =
    match term with
    | Var v -> Set.singleton v
    | App(t1, t2) -> Set.union (freeVars t1) (freeVars t2)
    | Abs(x, body) -> Set.remove x (freeVars body) 

/// generates new variable name that doesn't conflict with used variables
let freshVar baseName usedVars =
    let rec tryName n =
        let candidate = baseName + string n
        if Set.contains candidate usedVars then
            tryName (n + 1)
        else
            candidate
    
    if Set.contains baseName usedVars then
        tryName 0
    else
        baseName

/// replaces all free occurrences of x with subst in term
/// performs alpha-conversion to avoid variable capture
let rec substitute term x subst =
    match term with
    | Var v ->
        if v = x then subst else term
    
    | App(t1, t2) ->
        App(substitute t1 x subst, substitute t2 x subst)
    
    | Abs(v, body) ->
        if v = x then
            term
        elif not (Set.contains v (freeVars subst)) then
            Abs(v, substitute body x subst)
        else
            let usedVars = Set.union (freeVars body) (freeVars subst)
            let newV = freshVar v usedVars
            let renamedBody = substitute body v (Var newV)
            Abs(newV, substitute renamedBody x subst)

/// one step of beta-reduction
/// returns new term if reduction is possible
let rec betaStep term =
    match term with
    | Var _ -> None
    
    | App(Abs(x, body), arg) ->
        Some(substitute body x arg)
    
    | App(func, arg) ->
        match betaStep func with
        | Some func' -> Some(App(func', arg))
        | None ->
            match betaStep arg with
            | Some arg' -> Some(App(func, arg'))
            | None -> None
    
    | Abs(x, body) ->
        match betaStep body with
        | Some body' -> Some(Abs(x, body'))
        | None -> None

let rec normalize term =
    match betaStep term with
    | None -> term          
    | Some term' -> normalize term'  

let rec printTerm term =
    match term with
    | Var v -> v
    | Abs(x, body) -> sprintf "λ%s.%s" x (printTerm body)
    | App(func, arg) ->
        let funcStr =
            match func with
            | Abs _ -> sprintf "(%s)" (printTerm func)
            | _ -> printTerm func
        let argStr =
            match arg with
            | Abs _ | App _ -> sprintf "(%s)" (printTerm arg)
            | _ -> printTerm arg
        sprintf "%s %s" funcStr argStr