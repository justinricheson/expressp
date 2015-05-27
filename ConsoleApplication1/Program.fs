(*
    expression: number | (expression) | expression op expresion
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
*)

open System
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let number : Parser<_, unit> =
    numberLiteral NumberLiteralOptions.None "missing number" 
    |>> fun num -> int num.String

let op : Parser<_, unit> =
    charReturn '+' (+) <|>
    charReturn '-' (-) <|>
    charReturn '*' (*) <|>
    charReturn '/' (/)
    

[<EntryPoint>]
let main argv = 

    test op "//"
    Console.Read()

    0