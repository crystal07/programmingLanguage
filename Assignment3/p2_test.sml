use "sol3.sml";

match (Const 32, ConstP 1);
(* val it = NONE : (string * expression) list option *)

match (Const 32, ConstP 32);
(* val it = SOME [] : (string * expression) list option *)

match (Const 32, Variable "x");
(* val it = SOME [("x",Constant 32)] : (string * expression) list option *)

match (Tuple [Const 32, Const 3], TupleP [Variable "a", Variable "b"]);
(* val it = SOME [("a",Constant 32),("b",Variable "y")]
  : (string * expression) list option *)

match(Tuple [Unit, Unit], TupleP [Variable "a", ConstP 13] );