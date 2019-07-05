use "sol2.sml";

(* p1 test *)
val T = TRUE;
val F = FALSE;

eval(TRUE);
eval(FALSE);

eval(ANDALSO(T, T));
eval(ANDALSO(T, F));
eval(ANDALSO(F, T));
eval(ANDALSO(F, F));

eval(ORELSE(T, T));
eval(ORELSE(T, F));
eval(ORELSE(F, T));
eval(ORELSE(F, F));

eval(IMPLY(T, T));
eval(IMPLY(T, F));
eval(IMPLY(F, T));
eval(IMPLY(F, F));

eval(LESS(NUM 5, NUM 6));
eval(LESS(NUM 5, NUM 4));

eval(LESS(NUM 5, PLUS(NUM 3, NUM 3)));
eval(LESS(NUM 5, PLUS(NUM 2, NUM 2)));
eval(LESS(NUM 5, MINUS(NUM 6, NUM 1)));
eval(LESS(NUM 5, MINUS(NUM 7, NUM 1)));
