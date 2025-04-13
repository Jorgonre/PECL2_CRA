oracion(eng, Oraciones) -->
    ( g_nominal_coord(eng, GNList),
      estructura_verbal(eng, VerbosObjs),
      { construir_oraciones(GNList, VerbosObjs, Oraciones) }
    )
    ; ( g_nombre_propio(eng, GN),
        verbos_y_posible_nuevo_sujeto(eng, GN, Oraciones)
      )
    ; ( (g_nombre_propio(eng, GN); g_nominal(eng, GN)),
        g_verbal(eng, GV),
        { Oraciones = [o(GN, GV)] }
      )
    ; ( (g_nombre_propio(eng, GN); g_nominal(eng, GN)),
        g_verbal(eng, GV),
        (g_adjetival(eng, OBJ); g_nominal(eng, OBJ)),
        { Oraciones = [o(GN, GV, OBJ)] }
      )
    ; ( (g_nombre_propio(eng, GN1); g_nominal(eng, GN1)),
        g_verbal(eng, GV1),
        (g_adjetival(eng, OBJ); g_nominal(eng, OBJ)),
        (g_conjuncion(eng, conj(_)); g_relativos(eng, rel(_))),
        oracion(eng, OracionRest),
        { Oraciones = [o(GN1, GV1, OBJ, OracionRest)] }
      )
    ; ( (g_nombre_propio(eng, GN1); g_nominal(eng, GN1)),
        g_verbal(eng, GV1),
        (g_conjuncion(eng, conj(_)); g_relativos(eng, rel(_))),
        oracion(eng, OracionRest),
        { Oraciones = [o(GN1, GV1, OracionRest)] }
      ).

% Fix the verbos_y_posible_nuevo_sujeto predicate
verbos_y_posible_nuevo_sujeto(eng, GN, [o(GN, GV) | RestOraciones]) -->
    g_verbal(eng, GV),
    g_conjuncion(eng, conj(_)),
    lookahead_nombre_propio,
    g_nombre_propio(eng, NewGN),
    verbos_y_posible_nuevo_sujeto(eng, NewGN, RestOraciones).

verbos_y_posible_nuevo_sujeto(eng, GN, [o(GN, GV)]) -->
    g_verbal(eng, GV).

lookahead_nombre_propio --> [X], { n_p(X) }, [X].

% GRUPO NOMINAL COORDINADO: Nombres propios unidos por conjunciones
g_nominal_coord(eng, [GN]) --> g_nombre_propio(eng, GN).
g_nominal_coord(eng, [GN | Rest]) -->
    g_nombre_propio(eng, GN),
    g_conjuncion(eng, _),
    g_nominal_coord(eng, Rest).

estructura_verbal(eng, [(GV)]) --> g_verbal(eng, GV).  % solo verbos

estructura_verbal(eng, [(GV, OBJ)]) -->
    g_verbal(eng, GV),
    g_nominal(eng, OBJ).

estructura_verbal(eng, [(GV, OBJ) | Rest]) -->
    g_verbal(eng, GV),
    g_nominal(eng, OBJ),
    g_conjuncion(eng, _),
    estructura_verbal(eng, Rest).

% CONSTRUCTOR DE MÚLTIPLES ORACIONES PARA VARIOS SUJETOS
construir_oraciones([], _, []).
construir_oraciones([GN | RestGNs], VerbosObjetos, [O | Ors]) :-
    construir_oracion_individual(GN, VerbosObjetos, O),
    construir_oraciones(RestGNs, VerbosObjetos, Ors).

% Genera una oración individual para un GN con multiples verbos y objetos
construir_oracion_individual(GN, VerbosObjetos, Oracion) :-
    construir_oracion_individual_aux(GN, VerbosObjetos, [], Oracion).

% Caso base recursivo: cuando no hay mas verbos y objetos
construir_oracion_individual_aux(GN, [], Acc, Oracion) :-
    reverse(Acc, Partes),
    Oracion =.. [o, GN | Partes].

% Caso recursivo: acumula GV y OBJ
construir_oracion_individual_aux(GN, [(GV, OBJ) | Rest], Acc, Oracion) :-
    construir_oracion_individual_aux(GN, Rest, [OBJ, GV | Acc], Oracion).

% CONJUNCIONES
g_conjuncion(eng, conj(and)) --> [and].
g_conjuncion(eng, conj(or)) --> [or].
g_conjuncion(eng, conj(but)) --> [but].

% RELATIVOS
g_relativos(eng, rel(while)) --> [while].
g_relativos(eng, rel(who)) --> [who].
g_relativos(eng, rel(that)) --> [that].
g_relativos(eng, rel(although)) --> [although].

% GRUPOS SINTÁCTICOS
g_nominal(eng, gn(N)) --> nombre(eng, N).
g_nominal(eng, gn(D,N)) --> determinante(eng, D), nombre(eng, N).



g_verbal(eng, gv(V1, V2)) --> 
    verbo(eng, V1), 
    g_conjuncion(eng, _), 
    verbo(eng, V2).

g_verbal(eng, gv(V, GN, V2)) -->
        verbo(eng, V),
        g_nominal(eng, GN),
        g_conjuncion(eng, _),
        verbo(eng, V2).
g_verbal(eng, gv(V, ADJ, N)) -->
        verbo(eng, V),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N).

g_verbal(eng, gv(V, N, ADJ)) -->
        verbo(eng, V),
        g_nominal(eng, N),
        g_adjetival(eng, ADJ).

g_verbal(eng, gv(V, ADV, ADJ, N)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N).

g_verbal(eng, gv(V, ADV, N, ADJ)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        g_nominal(eng, N),
        g_adjetival(eng, ADJ).


g_verbal(eng, gv(V1, V2, V3)) -->
    verbo(eng, V1), 
    g_conjuncion(eng, _), 
    verbo(eng, V2),
    g_conjuncion(eng, _), 
    verbo(eng, V3).

% fallback individual
g_verbal(eng, gv(V)) --> verbo(eng, V).


g_adjetival(eng, gadj(ADJ)) --> adjetivo(eng, ADJ).
g_adverbial(eng, gadv(ADV)) --> adverbio(eng, ADV).
g_preposicional(eng, gp(PREP)) --> preposicion(eng, PREP).

% Nombre propio simple
g_nombre_propio(eng, g_nom_prop(NP)) -->
    nombre_propio(eng, NP).

% Nombre propio compuesto con conjunción
g_nombre_propio(eng, g_nom_prop(NP1, NP2)) -->
    nombre_propio(eng, NP1),
    g_conjuncion(eng, _),
    nombre_propio(eng, NP2).



% DETERMINANTES
determinante(eng, det(X)) --> [X], {det(X)}.
det(the).
det(a).

% NOMBRES
nombre(eng, n(X)) --> [X], {n(X)}.
n(dog).
n(table).
n(coffee).
n(newspaper).
n(chips).
n(beer).
n(paella).
n(novel).
n(philosophy).
n(law).
n(juice).
n(afternoons).
n(climbing).
n(apples).
n(word).
n(processor).
n(tool).
n(documents).
n(mouse).
n(cat).
n(man).
n(yesterday).
n(neighbour).

% NOMBRES PROPIOS
nombre_propio(eng, n_p(X)) --> [X], {n_p(X)}.
n_p(JOSE).
n_p(MARY).
n_p(HECTOR).
n_p(IRENE).

% VERBOS
verbo(eng, v(Y)) --> [Y], {v(X,Y)}.
verbo(eng, v(X)) --> [X], {v(X)}.
v(is).
v(is, _).
v(clears).
v(drinks).
v(drink).
v(reads).
v(eat).
v(eats).
v(sings).
v(jumps).
v(studies).
v(skips).
v(climbs).
v(write).
v(writing).
v(caught).
v(saw).
v(was).
v(prefers).
v(dances).

% ADJETIVOS
adjetivo(eng, adj(X)) --> [X], {adj(X)}.
adj(dark-skinned).
adj(blue).
adj(tall).
adj(agile).
adj(delicate).
adj(red).
adj(reds).
adj(powerful).
adj(slow).
adj(grey).

% ADVERBIOS
adverbio(eng, adv(Y)) --> [Y], {adv(X,Y)}, !.
adverbio(eng, adv(X)) --> [X], {adv(X)}.
adv(little).
adv(quite).
adv(only).
adv(quite, _).

% PREPOSICIONES
preposicion(eng, prep(X)) --> [X], {prep(X)}.
prep(at).

% PUNTUACIÓN
puntuacion(_, punt(X)) --> [X], {punt(X)}.
puntuacion(-).