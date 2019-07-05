
(* merge two sorted lists and return sorted list *)
(* merge: int list * int list -> int list *)

fun merge(x : int list, y : int list) =
    if null x
    then
        if null y
        then
            []
        else
            y
    else if null y
    then
        x
    else
        if hd(x) < hd(y)
        then
            hd(x) :: merge((tl x), y)
        else
            hd(y) :: merge(x, (tl y))
