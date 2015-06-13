open System
open SimpleParser
open ComplexParser // Shadow SimpleParser
open FParsec

let test p str =
    match run (p .>> eof) str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv =
    test expression "-1 + (1)"
    Console.Read() |> ignore
    0