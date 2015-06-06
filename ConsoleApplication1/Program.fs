open System
open FParsec
open FParsec.Primitives
open FParsec.CharParsers

let opp = new OperatorPrecedenceParser<float,unit,unit>()
let expr = opp.ExpressionParser
let term =
    (pfloat .>> spaces) <|>
    between (skipChar '(' >>. spaces)
            (skipChar ')' >>. spaces) expr
opp.TermParser <- term

opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> x + y))
opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> x - y))
opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> x * y))
opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> x / y))

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv =
    test expr "-1+2*(1+2    *(3))*(4\t\t\r\n*6  )"
    Console.Read() |> ignore
    0

(*
    Simplified:
    expression: number | number op expression
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    Complete:
    expression: number | (expression) | expression op expression
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
*)