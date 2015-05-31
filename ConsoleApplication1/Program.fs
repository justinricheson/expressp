open System
open FParsec

let number = many1 digit |>> fun ds -> float <| String.Concat(ds)

let op : Parser<float -> float -> float, unit> =
    charReturn '+' (+) <|>
    charReturn '-' (-) <|>
    charReturn '*' (*) <|>
    charReturn '/' (/)

let expression, expressionImpl = createParserForwardedToRef()
do expressionImpl :=
    choice[
        attempt(pipe3 number op expression (fun x y z -> y x z));
        number]

let test p str =
    match run (p .>> eof) str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv = 

    test expression "1+36/3*4-2"
    Console.Read() |> ignore

    0

(*
    Simplified:
    expression: number | number op expresion
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

    Complete:
    expression: number | (expression) | expression op expresion
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
*)