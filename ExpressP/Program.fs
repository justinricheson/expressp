open System
open CustomParser
open FParsec

let test p str =
    match run (p .>> eof) str with
    | Success(result, _, _) -> printfn "Success: %A" result
    | Failure(result, _, _) -> printfn "Failure: %A" result

[<EntryPoint>]
let main argv =
    //test expression "-1 + (1)"
    let result = Expression "(611-2^3^2+1)/10-5*2"
    printfn "%A" result.Result.Value

    Console.Read() |> ignore
    0