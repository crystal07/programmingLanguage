(* additivePersistence: int -> int *)
(* digitalRoot: int -> int *)

(* sol4 *)
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


fun additivePersistence(x: int) =
    let
        fun genetate_addtive_persistence_list(n: int) =
            let val digit_list = digits(n)
            in
                if null (tl digit_list)
                then
                    n::[]
                else
                    let
                        fun add_digit(nl: int list) =
                            if null (tl nl)
                            then hd nl
                            else hd nl + add_digit(tl nl)
                    in
                        n::genetate_addtive_persistence_list(add_digit(digit_list))
                    end   
            end
    in
        let
            fun count(nl: int list) =
                if null (tl nl)
                then
                    0
                else
                    count(tl nl) + 1
        in
            count(genetate_addtive_persistence_list(x))
        end
    end

fun digitalRoot(x: int) =
    let val digit_list = digits(x)
    in
        if null (tl digit_list)
        then
            hd digit_list
        else
            let
                fun add_digit(nl: int list) =
                    if null (tl nl)
                    then hd nl
                    else hd nl + add_digit(tl nl)
            in
                digitalRoot(add_digit(digit_list))  
            end
    end

