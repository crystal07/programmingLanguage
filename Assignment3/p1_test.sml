use "sol3.sml";

val tp0 = Wildcard;
val tp1 = TupleP ([Wildcard, UnitP, Variable ("Bhu"), Wildcard, ConstructorP ("Wuz", TupleP ([Variable ("Here"), Wildcard])), Variable ("")]);
val tp2 = TupleP ([ConstP(2), UnitP, Variable ("Bhu"), ConstP(7), ConstructorP ("Wuz", TupleP ([Variable ("Here"), UnitP])), Variable ("Bhu")]);
val tp3 = ConstructorP ("Combo", TupleP ([tp0, Wildcard, tp1, ConstP (84), tp2]));

val hw3Q10a_1 = check_pat (tp0) = true;
val hw3Q10a_2 = check_pat (tp1) = true;
val hw3Q10a_3 = check_pat (tp2) = false;