module PhoneBook

open System
open System.IO

/// contact entry with name and phone
type Contact = 
    { 
        Name: string; 
        Phone: string 
    }

type PhoneBook = Contact list

let empty: PhoneBook = []

/// adds a contact to the phone book
let add name phone (book: PhoneBook) : PhoneBook =
    { Name = name; Phone = phone } :: book

/// finds all phone numbers by name
let findPhonesByName name (book: PhoneBook) : string list =
    book
    |> List.filter (fun c -> c.Name = name)
    |> List.map (fun c -> c.Phone)

/// finds all names by phone number
let findNamesByPhone phone (book: PhoneBook) : string list =
    book
    |> List.filter (fun c -> c.Phone = phone)
    |> List.map (fun c -> c.Name)

/// gets all contacts as strings
let getAllContacts (book: PhoneBook) : string list =
    book
    |> List.map (fun c -> $"{c.Name}:{c.Phone}")

/// saves phone book to file
let saveToFile filePath (book: PhoneBook) : Result<unit, string> =
    try
        let lines = getAllContacts book
        File.WriteAllLines(filePath, lines)
        Ok ()
    with
    | ex -> Error ex.Message

/// loads phone book from file
let loadFromFile filePath : Result<PhoneBook, string> =
    try
        if not (File.Exists filePath) then
            Ok []
        else
            let lines = File.ReadAllLines filePath
            let contacts =
                lines
                |> Array.choose (fun line ->
                    match line.Split(':', 2) with
                    | [| name; phone |] when not (System.String.IsNullOrWhiteSpace name) 
                                          && not (System.String.IsNullOrWhiteSpace phone) ->
                        Some { Name = name.Trim(); Phone = phone.Trim() }
                    | _ -> None)
                |> Array.toList
            Ok contacts
    with
    | ex -> Error ex.Message