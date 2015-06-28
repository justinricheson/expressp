module CustomParser

open System

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
        | None -> { Result = None; Rest = input } // Recreate the result since p returns a ParseResult<'a>
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

let Next = fun input ->
    match input with
    | null -> { Result = None; Rest = input }
    | ""   -> { Result = None; Rest = input }
    | _    -> { Result = Some <| char input.[0..0]; Rest = input.[1..] }

let Sat predicate = ThenBind Next (fun n -> if predicate n.Value then Return n.Value else Fail)

let Digit = ThenBind (Sat Char.IsDigit) (fun c -> Return (float (c.Value.ToString())))
let rec NatHelper i =
    Or
        (ThenBind Digit (fun x ->
            NatHelper (float 10 * i + x.Value) ))
        (Return i)
let Nat = ThenBind Digit (fun d -> NatHelper d.Value)

let LiteralChar c = Sat (fun x -> x = c)
let rec Literal input token =
    match input with
    | "" -> Return token
    | _  -> Then (LiteralChar <| char input.[0..0]) (Literal input.[1..] token)

let AddSub =
    Or
        <| ThenBind (LiteralChar '+') (fun c -> Return (+))
        <| ThenBind (LiteralChar '-') (fun c -> Return (-))

let MulDiv =
    Or
        <| ThenBind (LiteralChar '*') (fun c -> Return (*))
        <| ThenBind (LiteralChar '/') (fun c -> Return (/))

let Exp = ThenBind (LiteralChar '^') (fun c -> Return ( ** ))

let rec Expression = Chainl1 Term AddSub
and Term = Chainl1 Factor MulDiv
and Factor = Chainr1 Part Exp
and Part = Or Nat Paren
and Paren =
    ThenBind
      (LiteralChar '(')
      (fun _ -> ThenBind Expression (fun e ->
            Then (LiteralChar ')') (Return e.Value)))