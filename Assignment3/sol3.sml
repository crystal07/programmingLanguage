(* 2016025532 Soojeong Shim *)
(* Assignment3 *)

(* sol1 and sol2 *)
datatype pattern = Wildcard
                    | Variable of string
                    | UnitP
                    | ConstP of int
                    | TupleP of pattern list
                    | ConstructorP of string * pattern

datatype valu = Const of int
                | Unit
                | Tuple of valu list
                | Constructor of string * valu

(* return true iff if all the variables appearing in the pattern are distinct from each other (i.e., use different strings) *)
(* val check_pat = fn : pattern -> bool *)

fun check_pat(pat) =
    let
        fun first(p, xs) =
            case p of
                Variable x => x::xs
                | TupleP ps => List.foldl first xs ps
                | ConstructorP(_, ps) => first(ps, xs)
                | _ => xs

        fun second(slist: string list) =
            case slist of
                [] => true
                | s::strs =>
                    if List.exists(fn a => a = s) strs
                    then false
                    else second(strs)
    in
        second(first(pat, []))
    end

(* val match = fn : valu * pattern -> (string * valu) list option *)
fun match (v, pat) =
    case (v, pat) of
        (_, Wildcard) => SOME []
        | (Unit, UnitP) => SOME []
        | (Const x, ConstP y) =>
            if x = y
            then SOME []
            else NONE
        | (vs, Variable vp) => SOME [(vp, vs)]
        | (Constructor (c1, x), ConstructorP (c2, y)) =>
            if c1 = c2
            then match(x, y)
            else NONE
        | (Tuple x, TupleP y) =>
            if List.length x = List.length y
            then
                (* all_answers match (ListPair.zip(x, y)) *)
                let
                    fun map (l, acc) = 
                        case l of
                            [] => SOME acc
                            | x::xs => 
                                case match x of
                                    NONE => NONE
                                    | SOME ls => map(xs, acc @ ls)
                in
                    let val fl = List.filter (fn a => if match(a) = NONE then false else true) (ListPair.zip(x,y))
                    in
                        if List.length fl = List.length x
                        then 
                            map((ListPair.zip(x, y)), [])
                        else NONE
                    end
                end
            else NONE
        | _ => NONE

(* sol3 *)
type name = string
datatype RSP =
        ROCK
        | SCISSORS
        | PAPER
datatype 'a strategy = Cons of 'a * (unit -> 'a strategy)
datatype tournament =
    PLAYER of name * (RSP strategy ref)
    | MATCH of tournament * tournament

fun onlyOne(one:RSP) =
    Cons(one, fn() => onlyOne(one)) 
fun alterTwo(one:RSP, two:RSP) =
    Cons(one, fn() => alterTwo(two, one))
fun alterThree(one:RSP, two:RSP, three:RSP) =
    Cons(one, fn() => alterThree(two, three, one))

val r = onlyOne(ROCK)
val s = onlyOne(SCISSORS)
val p = onlyOne(PAPER)
val rp = alterTwo(ROCK, PAPER)
val sr = alterTwo(SCISSORS, ROCK)
val ps = alterTwo(PAPER, SCISSORS)
val srp = alterThree(SCISSORS, ROCK, PAPER)

fun next(strategyRef) =
    let
        val Cons(rsp, func) = !strategyRef
    in
        strategyRef := func();
        rsp
    end

fun whosWinner(t) =
    let
        fun update(p) =
            case p of
                PLAYER(n, r) =>
                    let val Cons(rsp, func) = !r
                    in
                        r := func();
                        PLAYER(n, r)
                    end

        fun winner(p1, p2) =
            case (p1, p2) of
                (PLAYER(n1, r1), PLAYER(n2, r2)) =>
                    case (next(r1), next(r2)) of
                        (ROCK, PAPER) => update(PLAYER(n2, r2))
                        | (SCISSORS, ROCK) => update(PLAYER(n2, r2))
                        | (PAPER, SCISSORS) => update(PLAYER(n2, r2))
                        | _ => update(PLAYER(n1, r1))
    in
    case t of
        MATCH (t1, t2) =>
            case (t1, t2) of
                (PLAYER (n1, r1), MATCH (t1, t2)) =>
                    whosWinner(MATCH(PLAYER(n1, r1), whosWinner(MATCH(t1, t2))))
                | (MATCH (t1, t2), PLAYER (n1, r1)) =>
                    whosWinner(MATCH(PLAYER(n1, r1), whosWinner(MATCH(t1, t2))))
                | (MATCH (t1, t2), MATCH (t3, t4)) =>
                    whosWinner(MATCH(whosWinner(MATCH(t1, t2)), whosWinner(MATCH(t3, t4))))
                | (PLAYER (n1, r1), PLAYER (n2, r2)) =>
                    winner(PLAYER(n1, r1), PLAYER(n2, r2))
    end                    

val winner = whosWinner(MATCH(PLAYER("s", ref s), MATCH(PLAYER("rp", ref rp), PLAYER("r", ref r))));

fun test(p) =
    let fun update(p) =
        case p of
            PLAYER(n, r) =>
                let val Cons(rsp, func) = !r
                in
                    r := func();
                    PLAYER(n, r)
                end
    

fun test2(p) =
    case p of
        PLAYER (n1, r1) =>
            r1
val player = PLAYER("test", ref srp);
val t1 = test(player);
val t11 = test2(player);
val t2 = test(player);
val t22 = test2(player);
val t3 = test(player);
val t33 = test2(player);
