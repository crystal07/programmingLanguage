use "sol2.sml";


val result = true;

checkMetro (AREA("a", STATION "a"));
checkMetro (AREA("a", AREA("a", STATION "a")));
checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))));
checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))));

checkMetro (AREA("a", STATION "b"));
checkMetro (AREA("a", AREA("a", STATION "b")));
checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))));
checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))));

checkMetro (AREA("a", CONNECT(STATION "b", AREA("b", STATION "a"))));