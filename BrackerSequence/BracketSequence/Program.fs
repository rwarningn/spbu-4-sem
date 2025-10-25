module BracketSequence

/// map of closing brackets and opening brackets
let private bracketPairs =
    Map.ofList [
        (')', '(')
        (']', '[')
        ('}', '{')
    ]

/// check if is an opening bracket
let private isOpening c =
    bracketPairs |> Map.exists (fun _ v -> v = c)

/// check if is a closing bracket
let private isClosing c =
    bracketPairs |> Map.containsKey c

/// matching opening bracket for a closing bracket
let private getOpening c =
    Map.tryFind c bracketPairs

/// checks if bracket sequence in string is valid
let checkBrackets (input: string) =
    let rec check index stack =
        if index >= input.Length then
            List.isEmpty stack
        else
            let c = input.[index]
            
            if isOpening c then
                // openning bracket onto stack
                check (index + 1) (c :: stack)
            elif isClosing c then
                // check if it matches top of stack
                match stack with
                | top :: rest -> 
                    match getOpening c with
                    | Some opening when top = opening ->
                        check (index + 1) rest 
                    | _ -> false 
                | [] -> false 
            else
                // skip non-bracket chars
                check (index + 1) stack
    
    check 0 []