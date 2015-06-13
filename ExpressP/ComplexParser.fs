module ComplexParser

open FParsec
open FParsec.Primitives
open FParsec.CharParsers

(*
    expression: number | (expression) | expression op expression
    op: +, -, *, /
    number: digit*
    digit: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
*)

let opp = new OperatorPrecedenceParser<float,unit,unit>()
let expression = opp.ExpressionParser
let term =
    (pfloat .>> spaces) <|>
    between (skipChar '(' >>. spaces)
            (skipChar ')' >>. spaces) expression
opp.TermParser <- term

opp.AddOperator(InfixOperator("+", spaces, 1, Associativity.Left, fun x y -> x + y))
opp.AddOperator(InfixOperator("-", spaces, 1, Associativity.Left, fun x y -> x - y))
opp.AddOperator(InfixOperator("*", spaces, 2, Associativity.Left, fun x y -> x * y))
opp.AddOperator(InfixOperator("/", spaces, 2, Associativity.Left, fun x y -> x / y))