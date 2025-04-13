% CASO 2: Frase con sujeto omitido internamente (ya maneja sus propias conjunciones)
oracion(eng, Os) -->
    oracion_sujeto_omitido(eng, Os).

oracion(eng, [O | Os]) -->
    (oracion_simple(eng, O)),
    g_conjuncion(eng, _),
    oracion(eng, Os).



oracion(eng, [O]) -->
    oracion_simple(eng, O).

oracion_simple(eng, o(Suj, GV)) -->
    (g_nombre_propio(eng, Suj); g_nominal(eng, Suj)),
    g_verbal(eng, GV).


% Caso general recursivo: sujeto explícito + varios predicados unidos por conjunción
oracion_sujeto_omitido(eng, [o(Suj, GV1) | Resto]) -->
    (g_nombre_propio(eng, Suj); g_nominal(eng, Suj)),
    g_verbal(eng, GV1),
    (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
    g_verbal(eng, GV2),
    { Or = o(Suj, GV2) },
    (   % Si hay mas conjunciones y verbos, seguimos
        (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
        oracion_sujeto_omitido_resto(eng, Suj, Mas),
        { Resto = [Or | Mas] }
    ;
        % Si no hay mas conjunciones, terminamos
        { Resto = [Or] }
    ).

% Parte recursiva: sigue con mas verbos
oracion_sujeto_omitido_resto(eng, Suj, [o(Suj, GV) | Resto]) -->
    g_verbal(eng, GV),
    (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
    oracion_sujeto_omitido_resto(eng, Suj, Resto).

% Base de la recursión
oracion_sujeto_omitido_resto(eng, Suj, [o(Suj, GV)]) -->
    g_verbal(eng, GV).




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

g_verbal(eng, gv(V, OBJ))-->
    verbo(eng, V),
    (g_nominal(eng, OBJ); g_adjetival(eng, OBJ)).

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
verbo(eng, v(is, G)) --> [is, G], { gerundio(G) }.
verbo(eng, v(was, G)) --> [was, G], { gerundio(G) }.
verbo(eng, v(are, G)) --> [are, G], { gerundio(G) }.
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



gerundio(studying).
gerundio(writing).
gerundio(climbing).
gerundio(drinking).
gerundio(singing).
gerundio(eating).



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