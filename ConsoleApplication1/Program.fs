open System
open FParsec

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

let opString =
    attempt(pipe2 number (many1 opNum)
        (fun x y -> evaluate x y))
    <|> number

let lParen : Parser<_, unit> = betweenSpaces <| skipChar '('
let rParen : Parser<_, unit> = betweenSpaces <| skipChar ')'

let expression =
    attempt(opString .>> eof) <|>
    attempt(between lParen rParen opString)

let test p str =
    match run p str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv = 
    test expression "  (  1\t)\t"
    test expression "1 +
    2"
    test expression "1+2-3"
    test expression " -3+1"
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