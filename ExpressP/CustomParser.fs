module CustomParser

type ParseResult<'a> =
    {
        Result : Option<'a>;
        Rest : string
    }

type Parser<'a> = string -> ParseResult<'a>

let Fail     : Parser<_> = fun input -> { Result = None;   Rest = input }
let Return a : Parser<_> = fun input -> { Result = Some a; Rest = input }

let ThenBind p (f : Option<_> -> Parser<'b>) : Parser<'b> =
    fun input ->
        let r = p input
        match r.Result with
        | None -> { Result = None; Rest = input } // Have to recreate the result since p returns a Parser<'a>
        | _ -> (f r.Result) r.Rest
let Then p1 p2 : Parser<_> = ThenBind p1 (fun r -> p2)

let Or p1 p2 : Parser<_> =
    fun input ->
        let r = p1 input
        match r.Result with
        | None -> p2 input
        | _ -> r

// Then op (fun f -> Then p (fun y -> ChainlHelper (fun () -> f x y) p op))
let ChainlHelper a p op =
    Or
        <| (Then op op)
        <| Return a