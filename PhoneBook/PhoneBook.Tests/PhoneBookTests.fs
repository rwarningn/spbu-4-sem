module PhoneBook.Tests

open NUnit.Framework
open FsUnit
open PhoneBook
open System.IO

let testFolder = Path.Combine(Path.GetTempPath(), "PhoneBookTests")
let testFilePath = Path.Combine(testFolder, "contacts.txt")

[<SetUp>]
let Setup () =
    if Directory.Exists(testFolder) then
        Directory.Delete(testFolder, true)
    Directory.CreateDirectory(testFolder) |> ignore

[<TearDown>]
let TearDown () =
    if Directory.Exists(testFolder) then
        Directory.Delete(testFolder, true)

[<Test>]
let ``Empty phone book should be empty`` () =
    empty |> should be Empty

[<Test>]
let ``Add should add contact to phone book`` () =
    let book = empty |> add "Alice" "123-456"
    book |> should haveLength 1

[<Test>]
let ``Add multiple contacts`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "222-222"
        |> add "Charlie" "333-333"
    book |> should haveLength 3

[<Test>]
let ``Find phone by name returns correct phone`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "222-222"
    
    findPhonesByName "Alice" book |> should equal ["111-111"]

[<Test>]
let ``Find phone by name returns multiple phones`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Alice" "222-222"
        |> add "Bob" "333-333"
    
    findPhonesByName "Alice" book |> should equal ["222-222"; "111-111"]

[<Test>]
let ``Find phone by non-existent name returns empty list`` () =
    let book = empty |> add "Alice" "111-111"
    findPhonesByName "Bob" book |> should be Empty

[<Test>]
let ``Find name by phone returns correct name`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "222-222"
    
    findNamesByPhone "222-222" book |> should equal ["Bob"]

[<Test>]
let ``Find name by phone returns multiple names`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "111-111"
        |> add "Charlie" "222-222"
    
    findNamesByPhone "111-111" book |> should equal ["Bob"; "Alice"]

[<Test>]
let ``Find name by non-existent phone returns empty list`` () =
    let book = empty |> add "Alice" "111-111"
    findNamesByPhone "999-999" book |> should be Empty

[<Test>]
let ``Get all contacts returns formatted strings`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "222-222"
    
    let contacts = getAllContacts book
    contacts |> should equal ["Bob:222-222"; "Alice:111-111"]

[<Test>]
let ``Get all contacts from empty book returns empty list`` () =
    getAllContacts empty |> should be Empty

[<Test>]
let ``Save to file creates file with contacts`` () =
    let book = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "222-222"
    
    match saveToFile testFilePath book with
    | Ok () ->
        File.Exists(testFilePath) |> should be True
        let lines = File.ReadAllLines(testFilePath)
        lines |> should equal [| "Bob:222-222"; "Alice:111-111" |]
    | Error err -> Assert.Fail($"Expected Ok but got Error: {err}")

[<Test>]
let ``Save empty book creates empty file`` () =
    match saveToFile testFilePath empty with
    | Ok () ->
        File.Exists(testFilePath) |> should be True
        File.ReadAllLines(testFilePath) |> should be Empty
    | Error err -> Assert.Fail($"Expected Ok but got Error: {err}")

[<Test>]
let ``Load from file returns correct contacts`` () =
    File.WriteAllLines(testFilePath, [| "Alice:111-111"; "Bob:222-222" |])
    
    match loadFromFile testFilePath with
    | Ok book ->
        book |> should haveLength 2
        findPhonesByName "Alice" book |> should equal ["111-111"]
        findPhonesByName "Bob" book |> should equal ["222-222"]
    | Error err -> Assert.Fail($"Expected Ok but got Error: {err}")

[<Test>]
let ``Load from non-existent file returns empty book`` () =
    let nonExistentPath = Path.Combine(testFolder, "nonexistent.txt")
    
    match loadFromFile nonExistentPath with
    | Ok book -> book |> should be Empty
    | Error err -> Assert.Fail($"Expected Ok but got Error: {err}")

[<Test>]
let ``Load ignores invalid lines`` () =
    File.WriteAllLines(testFilePath, [| 
        "Alice:111-111"
        "InvalidLine"
        ":EmptyName"
        "EmptyPhone:"
        "Bob:222-222"
    |])
    
    match loadFromFile testFilePath with
    | Ok book ->
        book |> should haveLength 2
        findPhonesByName "Alice" book |> should equal ["111-111"]
        findPhonesByName "Bob" book |> should equal ["222-222"]
    | Error err -> Assert.Fail($"Expected Ok but got Error: {err}")

[<Test>]
let ``Save and load preserve data`` () =
    let originalBook = 
        empty 
        |> add "Alice" "111-111"
        |> add "Bob" "222-222"
        |> add "Charlie" "333-333"
    
    match saveToFile testFilePath originalBook with
    | Error err -> Assert.Fail($"Save failed: {err}")
    | Ok () ->
        match loadFromFile testFilePath with
        | Error err -> Assert.Fail($"Load failed: {err}")
        | Ok loadedBook ->
            getAllContacts loadedBook |> should equal (getAllContacts originalBook)

[<Test>]
let ``Save to invalid path returns error`` () =
    let invalidPath = "/invalid/path/contacts.txt"
    let book = empty |> add "Test" "123"
    
    match saveToFile invalidPath book with
    | Ok () -> Assert.Fail("Expected Error but got Ok")
    | Error _ -> Assert.Pass()