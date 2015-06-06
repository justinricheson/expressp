open System
open FParsec

let rec evaluate x y =
    match y with
    | [] -> x
    | (a, b)::t -> evaluate (a x b) t

let number : Parser<_, unit> =
    spaces
    >>. numberLiteral NumberLiteralOptions.AllowMinusSign "Invalid number" .>>
    spaces
    |>> fun num -> float num.String

let op : Parser<_, unit> =
    charReturn '+' (+) <|>
    charReturn '-' (-) <|>
    charReturn '*' (*) <|>
    charReturn '/' (/)

let opNum : Parser<_, unit> =
    pipe2 op number (fun x y -> (x, y))

let expression : Parser<_, unit> =
    attempt(pipe2 number (many1 opNum) (fun x y -> evaluate x y))
    <|> number

let test p str =
    match run (p .>> eof) str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv = 
    test expression "    1\t\t"
    test expression "1 +
    2"
    test expression "1+2-3"
    test expression "-3+1"
    test expression "1+36/3*4-2"
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