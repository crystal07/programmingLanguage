(* sigma: int * int * (int -> int) -> int *)

fun sigma(x: int, y: int, f: int -> int) =
    if x = y
    then
        f(x)
    else
        f(x) + sigma(x + 1, y, f)

fun tmp_p(x: int) =
    x + 1