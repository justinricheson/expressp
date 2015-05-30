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

let number = many1 digit |>> fun ds -> int <| String.Concat(ds)

let op : Parser<int -> int -> int, unit> =
    charReturn '+' (+) <|>
    charReturn '-' (-) <|>
    charReturn '*' (*) <|>
    charReturn '/' (/)
    
let expression, expressionImpl = createParserForwardedToRef()
do expressionImpl :=
    choice[
        number;
        between (pchar '(') (pchar ')') expression;]
    

[<EntryPoint>]
let main argv = 

    test expression "((123123))"
    Console.Read()

    0

// paren expression paren
// number op
// single number