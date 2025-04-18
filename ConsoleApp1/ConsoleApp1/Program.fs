open System
open System.Text.RegularExpressions


type Passport =
    {
        Series: string
        Number: string
        FullName: string
        DateOfBirth: DateTime
        DateOfIssue: DateTime
    }

    
    static member private ValidateSeries series =
        let pattern = @"^\d{4}$"  
        Regex.IsMatch(series, pattern)

    static member private ValidateNumber number =
        let pattern = @"^\d{6}$" 
        Regex.IsMatch(number, pattern)

    static member private ValidateFullName fullName =
        let pattern = @"^[А-ЯЁ][а-яё]+\s[А-ЯЁ][а-яё]+\s[А-ЯЁ][а-яё]+$" 
        Regex.IsMatch(fullName, pattern)

    static member private ValidateDateOfBirth dateOfBirth =
        dateOfBirth < DateTime.Now && dateOfBirth > DateTime(1900, 1, 1)

    static member private ValidateDateOfIssue dateOfIssue =
        dateOfIssue <= DateTime.Now

    // Метод для создания паспорта с валидацией данных
    static member Create(series, number, fullName, dateOfBirth, dateOfIssue) =
        let isValid =
            Passport.ValidateSeries series
            && Passport.ValidateNumber number
            && Passport.ValidateFullName fullName
            && Passport.ValidateDateOfBirth dateOfBirth
            && Passport.ValidateDateOfIssue dateOfIssue
        if isValid then
            Some {
                Series = series
                Number = number
                FullName = fullName
                DateOfBirth = dateOfBirth
                DateOfIssue = dateOfIssue
            }
        else
            None


    member this.Print() =
        printfn "Паспорт гражданина РФ:"
        printfn "Серия: %s" this.Series
        printfn "Номер: %s" this.Number
        printfn "ФИО: %s" this.FullName
        printfn "Дата рождения: %s" (this.DateOfBirth.ToString("dd.MM.yyyy"))
        printfn "Дата выдачи: %s" (this.DateOfIssue.ToString("dd.MM.yyyy"))

    
    member this.Equal(other: Passport) =
        this.Series = other.Series && this.Number = other.Number


[<EntryPoint>]
let main argv =
  
    match Passport.Create("1234", "567890", "Иванов Иван Иванович", DateTime(1990, 5, 12), DateTime(2010, 3, 15)) with
    | Some passport ->
        passport.Print()        
        let passport2 = Passport.Create("1234", "567890", "Петров Петр Петрович", DateTime(1985, 7, 20), DateTime(2005, 6, 10)).Value
        printfn "\nСравнение паспортов: %b" (passport.Equal(passport2))
    | None -> printfn "Ошибка: неверные данные для паспорта"

    0 
