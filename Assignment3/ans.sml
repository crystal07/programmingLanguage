(* Coursera Programming Languages, Homework 3, Provided Code *)

(**** DATA DEFINITIONS ****)

exception NoAnswer

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

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** FUNCTIONS ****)

(* string list -> string list *)
(* produce a subset of a given list *)
(* containing only strings that begin with an uppercase letter *)
(* ASSUME: all strings at least one character long *)

val only_capitals = List.filter (fn s => Char.isUpper (String.sub (s, 0)))

(* string list -> string *)
(* produce the longest string on the list *)
(* if empty list then produce "" *)
(* resolving ties: produce the string closest to the beginning of the list *)

val longest_string1 = 
    List.foldl 
	(fn (s, acc) => if String.size s > String.size acc then s else acc) "" 

(* string list -> string *)
(* produce the longest string on the list *)
(* if empty list then produce "" *)
(* resolving ties: produce the string closest to the end of the list *)

val longest_string2 = 
    List.foldl 
	(fn (s, acc) => if String.size s >= String.size acc then s else acc) "" 

(* (int * int -> bool) -> string list -> string *)
(* produce a string after performing a folding of a string list *)
(* given function f and starting with an initial value "" *)

fun longest_string_helper f ss =
    List.foldl 
	(fn (s, acc) => if f (String.size s, String.size acc) then s else acc)
	"" ss

(* string list -> string *)
(* produce the longest string on the list *)
(* if empty list then produce "" *)
(* resolving ties: produce the string closest to the beginning of the list *)

val longest_string3 = longest_string_helper (fn (x, y) => x > y)

(* string list -> string *)
(* produce the longest string on the list *)
(* if empty list then produce "" *)
(* resolving ties: produce the string closest to the end of the list *)

val longest_string4 = longest_string_helper (fn (x, y) => x >= y)

(* string list -> string *)
(* produce the longest string that starts with an uppercase letter *)
(* return "" if there is no such string *)

val longest_capitalized = longest_string3 o only_capitals

(* string -> string *)
(* produce reversed string *)

val rev_string = String.implode o List.rev o String.explode

(* ('a -> 'b option) -> 'a list -> 'b *)
(* apply f to elements of the list and produce v if it returns SOME v *)
(* if for all elements f returns NONE raise an excpetion NoAnswer *)

fun first_answer f l = 
    case l of
	[]     => raise NoAnswer
      | x::xs' => case f x of
		      NONE   => first_answer f xs'
		    | SOME v => v

(* ('a -> 'b list option) -> 'a list -> 'b list option *)
(* apply f to the given list and *)
(* produce NONE if for any element f produces NONE *)
(* produce SOME lst, where lst is a merged list of all the lists returned by f *)

fun all_answers f l = 
    let 
	fun all_answers_helper (l', acc) = 
	    case l' of 
		[] => SOME acc
	      | x::xs => case f x of
			     NONE => NONE
			   | SOME lst => all_answers_helper (xs, acc @ lst)
    in
	all_answers_helper (l, [])
    end

(* pattern -> int *)
(* produce the number of Wildcards in the given pattern *)

val count_wildcards = g (fn () => 1) (fn _ => 0)

(* pattern -> int *)
(* produce the sum : number of Wildcards plus lengths of variable names *)

val count_wild_and_variable_lengths = g (fn () => 1) String.size

(* string * pattern -> int *)
(* produce the number of times the string appears as a variable in the pattern *)

fun count_some_var (s, p) = g (fn () => 0) (fn v => if v = s then 1 else 0) p

(* pattern -> bool *)
(* produce true if all variable names in the pattern are distinct *)

val check_pat = 
    let
	(* pattern -> string list *)
	(* produce the list of all variable names *)
	fun collect_variable_names p = 
	    case p of 
		Variable v => [v]
	      | ConstructorP (_, p') => collect_variable_names p'
	      | TupleP ps => 
		List.foldl (fn (p, acc) => (collect_variable_names p) @ acc)
			   [] ps
	      | _ => []

	(* string list -> bool *)
	(* produce true if list has a duplicate *)
	fun has_duplicate ss = 
	    case ss of
		[] => false
	      | s::ss' => 
		(List.exists (fn s' => s = s') ss') orelse has_duplicate ss'
    in
	not o has_duplicate o collect_variable_names
    end

(* valu * pattern -> (string * valu) list option *)
(* produce NONE if pattern does not match value *)
(* produce SOME lst if pattern matches, and *)
(* lst is a list of bindings (string * valu) *)

fun match (v, p) = 
    case (v, p) of
	(_, Wildcard) => SOME []
      | (Unit, UnitP) => SOME []
      | (Const c, ConstP c') => if c = c' then SOME [] else NONE
      | (v, Variable n) => SOME [(n, v)]
      | (Constructor (s1, v), ConstructorP (s2, p)) => 	
	if s1 = s2 then match (v, p) else NONE

      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then all_answers match (ListPair.zip (vs, ps))
				 else NONE
      | _ => NONE

(* valu * pattern list -> (string * valu) list option *)
(* produce NONE if no pattern matches the value *)
(* otherwise produce SOME lst and lst is a list of bindings *)

fun first_match v ps = 
    (SOME (first_answer (fn p => match (v, p)) ps)) handle NoAnswer => NONE


(* OPTIONAL - CHALLENGE PROBLEM *)

(* ((string * string * typ) list) * (pattern list) -> typ option *)
(* produce SOME t, where t is a typ, that all the patterns can have *)
(* produce NONE if there is no such typ *)
(* Algorithm: 1. Produce the list of all the types for all the patterns *)
(*            2. Fold the list, comparing types with each other *)

fun typecheck_patterns (cs, ps) = 
    let	
	(* ('a -> 'b) -> 'a list -> 'b list option *)
	(* produces the same result as all_answers; f is different *)
	fun all_answers2 f l = 
	    let 
		fun all_answers_helper (l', acc) = 
		    case l' of 
			[] => SOME acc
		      | x::xs => case f x of
				     NONE => NONE
				   | SOME x => all_answers_helper (xs, acc @ [x])
	    in
		all_answers_helper (l, [])
	    end 

	(* typ * typ -> typ option *)
	(* produce SOME t when two types are the same or *)
	(* one is more general than the other; t is less general type *)
	(* produce NONE otherwise *)
	fun compare_types (t1, t2) = 
	    case (t1, t2) of
		(Anything, t) => SOME t
	      | (t, Anything) => SOME t
	      | (UnitT, UnitT) => SOME UnitT
	      | (IntT, IntT) => SOME IntT
	      | (Datatype d1, Datatype d2) => if d1 = d2 then SOME (Datatype d1) 
					      else NONE
	      | (TupleT ts1, TupleT ts2)  => 
		if List.length ts1 = List.length ts2 
		then (case all_answers2 compare_types (ListPair.zip (ts1, ts2))
		       of NONE => NONE
			| SOME lst => SOME (TupleT lst))
		else NONE
	      | _ => NONE

	(* pattern -> typ option *)
	(* produce the type of data that matches given pattern *)
	fun to_type p = 
	    let
 		(* check if ConstructorP (s, p) is a valid constructor *)
		(* which means that this constructor was defined *)
		(* and the type of p is also a valid type for that constructor *)
		fun valid_construct (cs', s, t) = 
		    case cs' of
			[] => NONE
		      | (c, tn, t')::cs'' => if s = c 
					     then case compare_types (t, t') of
						      NONE => NONE
						    | SOME _ => SOME (Datatype tn)
					     else valid_construct (cs'', s, t)
	    in
		case p of
		    Wildcard             => SOME Anything
		  | Variable _           => SOME Anything
		  | UnitP                => SOME UnitT
		  | ConstP _             => SOME IntT
		  | ConstructorP (s, p') => (case to_type p' of
						 NONE => NONE
					       | SOME t => valid_construct (cs, s, t))
		  | TupleP ps            => case all_answers2 to_type ps of
						NONE => NONE
					      | SOME lst => SOME (TupleT lst)
	    end	      
    in
	case all_answers2 to_type ps of 
	    NONE => NONE
	  | SOME lst => (List.foldl 
			     (fn (t, acc) => 
				 case acc of
				     NONE => NONE
				   | SOME t' => compare_types(t, t'))
			     (SOME Anything) lst)
    end