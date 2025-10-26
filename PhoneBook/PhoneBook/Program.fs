open System
open PhoneBook

let printMenu () =
    printfn """
            Телефонный справочник:
            1 - Выйти
            2 - Добавить запись
            3 - Найти телефон по имени
            4 - Найти имя по телефону
            5 - Показать все записи
            6 - Сохранить в файл
            7 - Загрузить из файла
            """

let rec mainLoop (book: PhoneBook) =
    printMenu()
    printf "выберите команду: "
    
    match Console.ReadLine() with
    | "1" ->
        printfn "Выход из программы"
        book
    
    | "2" ->
        printf "Введите имя: "
        let name = Console.ReadLine()
        printf "Введите телефон: "
        let phone = Console.ReadLine()
        let newBook = add name phone book
        printfn "Контакт добавлен!\n"
        mainLoop newBook
    
    | "3" ->
        printf "Введите имя: "
        let name = Console.ReadLine()
        let phones = findPhonesByName name book
        if List.isEmpty phones then
            printfn "Контакты не найдены\n"
        else
            printfn "Найденные телефоны:"
            phones |> List.iter (printfn "  %s")
            printfn ""
        mainLoop book
    
    | "4" ->
        printf "Введите телефон: "
        let phone = Console.ReadLine()
        let names = findNamesByPhone phone book
        if List.isEmpty names then
            printfn "Контакты не найдены\n"
        else
            printfn "Найденные имена:"
            names |> List.iter (printfn "  %s")
            printfn ""
        mainLoop book
    
    | "5" ->
        let contacts = getAllContacts book
        if List.isEmpty contacts then
            printfn "Справочник пуст\n"
        else
            printfn "Все контакты:"
            contacts |> List.iter (printfn "  %s")
            printfn ""
        mainLoop book
    
    | "6" ->
        printf "Введите путь к файлу: "
        let path = Console.ReadLine()
        match saveToFile path book with
        | Ok () -> printfn "Данные успешно сохранены!\n"
        | Error err -> printfn "Ошибка сохранения: %s\n" err
        mainLoop book
    
    | "7" ->
        printf "Введите путь к файлу: "
        let path = Console.ReadLine()
        match loadFromFile path with
        | Ok loadedBook ->
            printfn "Данные успешно загружены!\n"
            mainLoop loadedBook
        | Error err ->
            printfn "Ошибка загрузки: %s\n" err
            mainLoop book
    
    | _ ->
        printfn "Неверная команда!\n"
        mainLoop book

[<EntryPoint>]
let main argv =
    printfn "Добро пожаловать в телефонный справочник!"
    mainLoop empty |> ignore
    0