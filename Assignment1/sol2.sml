(* takes a list and return reversed *)
(* reverse: int list -> int list *)

fun reverse(xs : int list) =
    let
        fun append(ys: int list, z: int) =
            if null ys
            then
                z::[]
            else
                hd ys::append(tl ys, z)
    in
        if null (tl xs)
        then
            hd xs::[]
        else
            append(reverse(tl xs), hd xs)
    end
