open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers


(*
    expression: number | (expression) | expression op expression
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
*)

(*
let opp = new OperatorPrecedenceParser<float,unit,unit>()
let expression = opp.ExpressionParser
let term =
    (pfloat .>> spaces) <|>
    between (skipChar '(' >>. spaces)
            (skipChar ')' >>. spaces) expr
opp.TermParser <- term

opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> x + y))
opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> x - y))
opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> x * y))
opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> x / y))
*)




(*
    expression: number | number op expression
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
*)

let rec evaluate x y =
    match y with
    | [] -> x
    | (a, b)::t -> evaluate (a x b) t

let betweenSpaces p = between spaces spaces p

let number =
    betweenSpaces <| numberLiteral
        NumberLiteralOptions.AllowMinusSign
        "Invalid number"
        |>> fun num -> float num.String

let op =
    charReturn '+' (+) <|>
    charReturn '-' (-) <|>
    charReturn '*' (*) <|>
    charReturn '/' (/)

let opNum = pipe2 op number (fun x y -> (x, y))

let expression =
    attempt(pipe2 number (many1 opNum)
        (fun x y -> evaluate x y))
    <|> number

let test p str =
    match run (p .>> eof) str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv =
    test expression "-1"
    Console.Read() |> ignore
    0