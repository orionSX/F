open System

open FParsec

// Алгебраический тип
type Expr =
    | Number of int
    | Add of Expr * Expr
    | Sub of Expr * Expr
    | Mul of Expr * Expr
    | Div of Expr * Expr

// Парсинг чисел
let ws = spaces
let str_ws s = pstring s .>> ws
let number: Parser<Expr, unit> =
    pint32 .>> ws |>> Number

// Операции с приоритетами
let opp = new OperatorPrecedenceParser<Expr, unit, unit>()
let expr = opp.ExpressionParser

let term = number <|> between (str_ws "(") (str_ws ")") expr

opp.TermParser <- term

opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun x y -> Add(x, y)))
opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun x y -> Sub(x, y)))
opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun x y -> Mul(x, y)))
opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun x y -> Div(x, y)))

// Главный парсер
let parseExpression input =
    match run expr input with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg


[<EntryPoint>]
let main argv =
    let input = "1 + 2 * (3 + 4)"
    let parsed = parseExpression input

    Console.WriteLine("Результат разбора: {0}", parsed)

    0
