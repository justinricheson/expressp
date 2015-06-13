﻿module CustomParser

type ParseResult<'a> =
    {
        Result : 'a;
        Rest : string
    }

let Fail = fun input -> None
let Return a = fun input -> { Result = a; Rest = input}
let Then p f =
    fun input ->
        let r = p(input)
        match r with
        | None -> None
        | _ -> f(r.Value.Result)(r.Value.Rest)
let Then_ p1 p2 = fun () -> Then p1 (fun r -> p2)