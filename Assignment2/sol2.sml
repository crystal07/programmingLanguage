(* 2016025532 Soojeong Shim *)
(* Assignment2 *)

datatype expr = NUM of int
                | PLUS of expr * expr
                | MINUS of expr * expr

datatype formula = TRUE
                    | FALSE
                    | NOT of formula
                    | ANDALSO of formula * formula
                    | ORELSE of formula * formula
                    | IMPLY of formula * formula
                    | LESS of expr * expr

(* evaluate formula result *)
(* eval: formula -> bool *)
(* implies: ~A or B *)
fun eval(formula) =
    case formula of
        TRUE => true
        | FALSE => false
        | NOT formula =>
            if eval(formula)
            then false
            else true
        | ANDALSO (formula1, formula2) => eval(formula1) andalso eval(formula2)
        | ORELSE (formula1, formula2) => eval(formula1) orelse eval(formula2)
        | IMPLY (formula1, formula2) =>
            if eval(formula1) = true andalso eval(formula2) = false
            then false
            else true
        | LESS (expr1, expr2) =>
            let
                fun express(e) =
                    case e of 
                        NUM n => n
                        | PLUS (n1, n2) => express(n1) + express(n2)
                        | MINUS (n1, n2) => express(n1) - express(n2)
            in
                if express(expr1) < express(expr2)
                then true
                else false
            end



type name = string
datatype metro = STATION of name
                | AREA of name * metro
                | CONNECT of metro * metro

(* return station name is correct *)
(* checkMetro: metro -> bool *)
fun checkMetro(metro) = 
    let
        fun inArea(met, areas) = 
            case met of
                STATION n =>
                    if null areas
                    then false
                    else
                        if hd(areas) = n
                        then true
                        else false orelse inArea(STATION(n), tl(areas))
                | AREA(n, m) =>
                    inArea(m, n::areas)
                | CONNECT(m1, m2) =>
                    inArea(m1, areas) andalso inArea(m2, areas)
    in
        inArea(metro, [])
    end

(* lazy list *)
datatype 'a lazyList = nullList
                    | cons of 'a * (unit -> 'a lazyList)

(* return lazyList starting with first, ending with last *)
(* seq: int * int -> lazyList *)
fun seq(first, last) = 
    if first = last
    then cons(first, fn() => nullList)
    else cons(first, fn() => seq(first+1, last))

(* return infinite lazy list starting with first *)
(* infSeq: int -> lazyList *)
fun infSeq(first) =
    cons(first, fn() => infSeq(first+1))

(* if list not contain N values, return all values *)
(* else return first N values *)
(* firstN: lazyListVal * int -> int list *)
fun firstN(lazyListVal, n) =
    case lazyListVal of
        nullList => []
        | cons(x, xlist) =>
            if n = 1
            then x::[]
            else x::firstN(xlist(), n-1)

(* return an option as representing the n-th value *)
(* Nth: lazyList * int -> option *)
fun Nth(lazyListVal, n) =
    case (lazyListVal, n) of
        (nullList, _) => NONE
        | (cons(x, xlist), 1) => SOME x
        | (cons(x, xlist), n) => Nth(xlist(), n-1)

(* return a new lazy list *)
(* filterMultiples: lazyList * int -> lazyList *)
fun filterMultiples(lazyListVal, n) =
    case lazyListVal of
        nullList => nullList
        | cons(x, xlist) =>
            if (x mod n) = 0 
            then filterMultiples(xlist(), n)
            else cons(x, fn() => filterMultiples(xlist(), n))

(* return n and n multiple filtered lazy list *)
(* seive: lazyList -> lazyList *)
fun seive(lazyListVal) = 
    case lazyListVal of
        nullList => nullList
        | cons(x, xlist) => cons(x, fn() => seive(filterMultiples(cons(x, xlist), x)))

(* return lazy list of prime numbers using Sieve of Eratosthnes algorithm *)
(* primes: -> lazyList *)
fun primes() =
    seive(infSeq(2))
