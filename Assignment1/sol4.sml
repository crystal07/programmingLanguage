(* digits: int -> int list *)

fun digits(x: int) =
    let
        fun append(ys: int list, z: int) =
            if null ys
            then
                z::[]
            else
                hd ys::append(tl ys, z)
    in
        let val digit_list = []
        in
            if x < 10
            then
                x::digit_list
            else
                append(digits(x div 10), x mod 10)
        end
    end