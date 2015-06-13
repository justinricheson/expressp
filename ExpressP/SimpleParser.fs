module SimpleParser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers

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

let expression : Parser<_, unit> =
    attempt(pipe2 number (many1 opNum)
        (fun x y -> evaluate x y))
    <|> number