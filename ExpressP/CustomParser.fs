﻿module CustomParser

type ParseResult<'a> =
    {
        Result : Option<'a>;
        Rest : string
    }

let Fail     = fun input -> { Result = None;   Rest = input }
let Return a = fun input -> { Result = Some a; Rest = input }

let ThenBind p f =
    fun input ->
        let r = p input
        match r.Result with
        | None -> { Result = None; Rest = input } // Have to recreate the result since p returns a Parser<'a>
        | _ -> (f r.Result) r.Rest
let Then p1 p2 = ThenBind p1 (fun r -> p2)

let Or p1 p2 =
    fun input ->
        let r = p1 input
        match r.Result with
        | None -> p2 input
        | _ -> r

let rec Chainl1Helper a p op =
    Or
        <| ThenBind  op (fun f ->
           ThenBind   p (fun y ->
           Chainl1Helper (f.Value a y.Value) p op))
        <| Return a
let Chainl1 p op = ThenBind p (fun x -> Chainl1Helper x.Value p op)

let rec Chainr1 p op =
    ThenBind p (fun x ->
        Or
            (ThenBind op (fun f ->
                ThenBind (Chainr1 p op) (fun y ->
                    Return (f.Value x.Value y.Value))))
            (Return x.Value))



let Nat = true
let Literal = true