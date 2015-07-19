module CustomParser

open System

type ParseResult<'a> =
    {
        Result : Option<'a>;
        Rest : string
    }

let Fail     = fun input -> { Result = None;   Rest = input }
let Return a = fun input -> { Result = Some a; Rest = input }

let (>>=) p f =
    fun input ->
        let r = p input
        match r.Result with
        | None -> { Result = None; Rest = input } // Recreate the result since p returns a ParseResult<'a>
        | _ -> (f r.Result.Value) r.Rest
let Then p1 p2 = p1 >>= (fun r -> p2)

let (<|>) p1 p2 =
    fun input ->
        let r = p1 input
        match r.Result with
        | None -> p2 input
        | _ -> r

let rec Chainl1Helper a p op =
    op >>= (fun f ->
     p >>= (fun y ->
       Chainl1Helper (f a y) p op))
    <|> Return a
let Chainl1 p op = p >>= (fun x -> Chainl1Helper x p op)

let rec Chainr1 p op =
    p >>= (fun x ->
        (op >>= (fun f -> (Chainr1 p op) >>= (fun y -> Return (f x y))))
        <|> (Return x))

let Next = fun input ->
    match input with
    | null -> { Result = None; Rest = input }
    | ""   -> { Result = None; Rest = input }
    | _    -> { Result = Some <| char input.[0..0]; Rest = input.[1..] }

let Sat predicate = Next >>= (fun n -> if predicate n then Return n else Fail)

let Digit = (Sat Char.IsDigit) >>= (fun c -> Return (float (c.ToString())))
let rec NatHelper i =
    (Digit >>= (fun x ->
        NatHelper (float 10 * i + x) ))
    <|> (Return i)
let Nat = Digit >>= (fun d -> NatHelper d)

let LiteralChar c = Sat (fun x -> x = c)
let rec Literal input token =
    match input with
    | "" -> Return token
    | _  -> Then (LiteralChar <| char input.[0..0]) (Literal input.[1..] token)

let AddSub =
    ((LiteralChar '+') >>= (fun c -> Return (+))) <|>
    ((LiteralChar '-') >>= (fun c -> Return (-)))

let MulDiv =
    ((LiteralChar '*') >>= (fun c -> Return (*))) <|>
    ((LiteralChar '/') >>= (fun c -> Return (/)))

let Exp = (LiteralChar '^') >>= (fun c -> Return ( ** ))

let rec Expression = Chainl1 Term AddSub
and Term = Chainl1 Factor MulDiv
and Factor = Chainr1 Part Exp
and Part = Nat <|> Paren
and Paren =
      (LiteralChar '(') >>=
      (fun _ -> Expression >>= (fun e ->
            Then (LiteralChar ')') (Return e)))