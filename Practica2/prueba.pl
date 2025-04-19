



oracion(eng, [O | Os]) -->
    (oracion_con_subordinada(eng, O))
    ;
    (oracion_sujeto_omitido(eng, O))
    ;
    (oracion_simple(eng, O))
    ;
    (oracion_sujeto_omitido(eng, O),
    (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
    oracion(eng, Os))
    ;
    ((oracion_simple(eng, O)),
    (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
    oracion(eng, Os))
    ;
    oracion_con_subordinada_implicita(eng, O).



oracion_simple(eng, o(Suj, GV)) -->
    (g_nombre_propio(eng, Suj); g_nominal(eng, Suj)),
    g_verbal(eng, GV).


oracion_sujeto_omitido(eng, OracionesTotales) -->
    (g_nombre_propio(eng, Suj); g_nominal(eng, Suj)),
    g_verbal(eng, GV1),
    (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
    g_verbal(eng, GV2),
    {
        sujetos_de_compuesto(Suj, Sujetos),
        GVsIniciales = [GV1, GV2]
    },
    (
        (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
        oracion_sujeto_omitido_resto_verbs_collect(eng, GVsResto),
        { append(GVsIniciales, GVsResto, TodosGVs),
          generar_oraciones(Sujetos, TodosGVs, OracionesTotales)
        }
    ;
        { generar_oraciones(Sujetos, GVsIniciales, OracionesTotales) }
    ).





% Recolecta todos los GV restantes recursivamente
oracion_sujeto_omitido_resto_verbs_collect(eng, [GV | Resto]) -->
    g_verbal(eng, GV),
    (
        (g_conjuncion(eng, _); g_relativos(eng, rel(_))),
        oracion_sujeto_omitido_resto_verbs_collect(eng, Resto)
    ;
        { Resto = [] }
    ).


% Maneja oraciones con una subordinada relativa
oracion_con_subordinada(eng, [o(Suj, GVSub), o(Suj, GVMain)]) -->
    (g_nombre_propio(eng, Suj); g_nominal(eng, Suj)),
    ([','];coma),
    g_relativos(eng, rel(_)), % e.g. who
    g_verbal(eng, GVSub),
    ([',']; coma),
    g_verbal(eng, GVMain).


oracion_con_subordinada(eng, [o(SujMain, GVMain), o(SujSub, GVSub)]) -->
    (g_nombre_propio(eng, SujMain); g_nominal(eng, SujMain)),
    g_relativos(eng, rel(_)), 
    (g_nombre_propio(eng, SujSub); g_nominal(eng, SujSub)),
    g_verbal(eng, GVSub),
    g_verbal(eng, GVMain).


oracion_con_subordinada(eng, [o(SujMain, GVMain), o(SujSub, GVSub)]) -->
    (g_nombre_propio(eng, SujMain); g_nominal(eng, SujMain)),
    ([',']; coma),
    g_relativos(eng, rel(_)), 
    (g_nombre_propio(eng, SujSub); g_nominal(eng, SujSub)),
    g_verbal(eng, GVSub),
    ([',']; coma),
    g_verbal(eng, GVMain).

oracion_con_subordinada(eng, [o(Suj, GVSub), o(Suj, GVMain)]) -->
    (g_nombre_propio(eng, Suj); g_nominal(eng, Suj)),
    g_relativos(eng, rel(_)), 
    g_verbal(eng, GVSub),
    g_verbal(eng, GVMain).


oracion_con_subordinada_implicita(eng, [o(SujMain, GVMain), o(SujRel, GVSub)]) -->
    (g_nombre_propio(eng, SujMain); g_nominal(eng, SujMain)),
    (g_relativos(eng, rel(_)); []),  % Permite que se omita el relativo
    oracion_simple(eng, o(SujRel, GVSub)),
    g_verbal(eng, GVMain).

% Descompone un sujeto compuesto en una lista de sujetos individuales
sujetos_de_compuesto(g_nom_prop(NP1, NP2), [g_nom_prop(NP1), g_nom_prop(NP2)]) :- !.
sujetos_de_compuesto(Sujeto, [Sujeto]).


% Genera lista de oraciones con cada sujeto + cada GV
generar_oraciones([], _, []).
generar_oraciones([S | Sujetos], GVs, Resultado) :-
    findall(o(S, GV), member(GV, GVs), OracionesS),
    generar_oraciones(Sujetos, GVs, Resto),
    append(OracionesS, Resto, Resultado).


% CONJUNCIONES
g_conjuncion(eng, conj(and)) --> [and].
g_conjuncion(eng, conj(or)) --> [or].
g_conjuncion(eng, conj(but)) --> [but].
/*
No sé si este de but it está bien planteado
*/
g_conjuncion(eng, conj(but_it)) --> [but, it].

% RELATIVOS
g_relativos(eng, rel(while)) --> [while].
g_relativos(eng, rel(who)) --> [who].
g_relativos(eng, rel(who)) --> [which].
g_relativos(eng, rel(that)) --> [that].
g_relativos(eng, rel(although)) --> [although].

coma --> [','].
coma --> [coma].

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
        g_nominal(eng, OBJ).

g_verbal(eng, gv(V, ADV, OBJ)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        (g_nominal(eng, OBJ);g_adjetival(eng, OBJ)).

g_verbal(eng, gv(V, N, ADJ)) -->
        verbo(eng, V),
        g_nominal(eng, N),
        g_adjetival(eng, ADJ).

g_verbal(eng, gv(V, ADV, ADJ, N)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N).

g_verbal(eng, gv(V, ADV, PREP, ADJ, N)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        g_preposicional(eng, PREP),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N).


/*ESTA VA A SER MUY ESPECÍFICA, 
SE PUEDE DEJAR ASÍ O GENERALIZAR MÁS ADELANTE
*/
g_verbal(eng, gv(V, PREP1, N1, PREP2, N2)) -->
        verbo(eng, V),
        g_preposicional(eng, PREP1),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP2),
        g_nominal(eng, N2).

g_verbal(eng, gv(V1, PREP, V2, N)) -->
        verbo(eng, V1),
        g_preposicional(eng, PREP),
        verbo(eng, V2),
        g_nominal(eng, N).

g_verbal(eng, gv(V1, PREP1, ADV, ADJ, N1, PREP2, V2, N2)) -->
        verbo(eng, V1),
        g_preposicional(eng, PREP1),
        g_adverbial(eng, ADV),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP2),
        g_verbal(eng, V2),
        g_nominal(eng, N2).


g_verbal(eng, gv(V1, V2, V3)) -->
    verbo(eng, V1), 
    g_conjuncion(eng, _), 
    verbo(eng, V2),
    g_conjuncion(eng, _), 
    verbo(eng, V3).

g_verbal(eng, gv(V, ADV))-->
    verbo(eng, V),
    g_adverbial(eng, ADV).

% fallback individual
g_verbal(eng, gv(V)) --> verbo(eng, V).


g_adjetival(eng, gadj(ADJ)) --> adjetivo(eng, ADJ).
g_adverbial(eng, gadv(ADV)) --> adverbio(eng, ADV).
g_preposicional(eng, gp(PREP)) --> preposicion(eng, PREP).
 



% Nombre propio simple
g_nombre_propio(eng, g_nom_prop(NP)) -->
    nombre_propio(eng, NP).

/*
DE momento se deja para solo dos nombres pues no sucede de necesitarse 3 o más pero podría ser bueno añadir como mejora permitir más nombres. 
CHAT lo hace simple el cambio pero habría que cambiar tmb la función de sujeto omitido y hasta que no funcione todo no la quiero tocar
*/


% Nombre propio compuesto con conjunción
g_nombre_propio(eng, g_nom_prop(NP1, NP2)) -->
    nombre_propio(eng, NP1),
    g_conjuncion(eng, _),
    nombre_propio(eng, NP2).

    
    g_nombre_propio(eng, g_nom_prop(NP1, NP2, NPs)) -->
        nombre_propio(eng, NP1),
        g_conjuncion(eng, _),
        nombre_propio(eng, NP2),
        g_nombre_propio_resto(eng, NPs).

    % Recolecta los nombres propios adicionales recursivamente
    g_nombre_propio_resto(eng, [NP | NPs]) -->
        g_conjuncion(eng, _),
        nombre_propio(eng, NP),
        g_nombre_propio_resto(eng, NPs).
    g_nombre_propio_resto(eng, []) --> [].


% DETERMINANTES
determinante(eng, det(X)) --> [X], {det(X)}.
det(the).
det(a).
det(my).
det(his).
det(her).
det(first).
det(several).

% NOMBRES
nombre(eng, n(Nombre)) -->
    [X, Y],
    { nombre_compuesto(X, Y),
      atomic_list_concat([X, Y], '_', Nombre) }.
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
n(neighbour).
n(football).
n(basketball).
n(goalkeepers).
n(team).
n(Roland-Garros).
n(ball).
n(player).
n(swimmer).
n(tennis).
n(medal).
n(swimming).
n(expert).
n(arts).
n(tournaments).
n(marathons).
n(dance).
n(couple).
n(region).
n(rugby).
n(boxing).
n(tournament).
n(captains).
n(volleyball).
n(record).
n(category).
n(climbing).
n(mountains).
n(triathlon).
n(competition).
n(cycling).
n(race).
n(archery).
n(hockey).
n(chess).
n(school).
n(championship).
n(athletics).
n(weightlifting).
n(kayaking).
n(swimming).
n(tennis).
n(handball_team).
n(table_tennis).
n(long_jump).
n(weekends).
n(skiing).
n(snowboarding).
n(title).
n(parkour).
n(city).
n(shooting).
n(badminton).
n(wrestling).
n(judo).
n(belt).
n(dexterity).
n(podium).
n(technique).


nombre_compuesto(climbing, wall).
nombre_compuesto(word, processor).


% NOMBRES PROPIOS
nombre_propio(eng, n_p(X)) --> [X], {n_p(X)}.
//MUY IMPORTANTE, SI SE USAN LOS NOMBRES EN MAYUSCULAS 
PONERLOS ENTRE COMILLASEN LA TERMINAL//
n_p(JOSE).
n_p(MARY).
n_p(HECTOR).
n_p(IRENE).
n_p(JUAN).
n_p(PEDRO).
n_p(JAVIER).
n_p(RAFA).
n_p(AITANA).
n_p(JULIÁN).
n_p(MIGUEL).
n_p(LEO).
n_p(MARTA).
n_p(ÁLEX).
n_p(MARCOS).
n_p(PAULA).
n_p(SARA).
n_p(CLAUDIA).
n_p(DANIEL).
n_p(BEATRIZ).
n_p(MARIA).
n_p(CARLOS).
n_p(ANDRÉS).
n_p(DIEGO).
n_p(LUCAS).
n_p(ELENA).
n_p(SOFIA).
n_p(TOMÁS).
n_p(LAURA).
n_p(ANA).
n_p(LUIS).
n_p(JAIME).
n_p(NATALIA).
n_p(RUBÉN).
n_p(INÉS).
n_p(CLARA).
n_p(ÁLVARO).
n_p(CRISTINA).
n_p(DAVID).
n_p(SANTIAGO).
n_p(JORGE).
n_p(ROCÍO).
n_p(ANGELA).
n_p(MARTINA).
n_p(LUCÍA).
n_p(ALBERTO).
n_p(TERESA).
n_p(SERGIO).
n_p(ALEXIS).
n_p(ELISA).
n_p(PABLO).
n_p(EDUARDO).
n_p(ARIANA).
n_p(ANDREA).
n_p(ÓSCAR).

% VERBOS
verbo(eng, v(Y)) --> [Y], {v(X,Y)}.
verbo(eng, v(X)) --> [X], {v(X)}.
verbo(eng, v(is, G)) --> [is, G], { gerundio(G) }.
verbo(eng, v(was, G)) --> [was, G], { gerundio(G) }.
verbo(eng, v(are, G)) --> [are, G], { gerundio(G) }.
verbo(eng, v(is, P)) --> [is, P], { pasado(P) }.

v(is).
v(is, _).
v(clears).
v(drinks).
v(drink).
v(reads).
v(climbs).
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
v(plays).
v(practices).
v(has).
v(is).
v(competes).
v(runs).
v(trains).
v(climbed).
v(broken).
v(elected).
v(selected).
v(train).
v(named).
v(beaten).
v(achieved).
v(goes).
v(enjoys).
v(practises).
v(participated).
v(managed).
v(won).
v(clears).
v(drinks).
v(reads).
v(skips).
v(eats).
v(sings).
v(studies).
v(prepares).
v(obtained).
v(improves).




gerundio(studying).
gerundio(writing).
gerundio(climbing).
gerundio(drinking).
gerundio(singing).
gerundio(eating).

pasado(used).



% ADJETIVOS
adjetivo(eng, adj(X)) --> [X], {adj(X)}.
adj(dark-skinned).
adj(agile).
adj(blue).
adj(tall).
adj(agile).
adj(delicate).
adj(red).
adj(reds).
adj(powerful).
adj(slow).
adj(grey).
adj(martial).
adj(great).
adj(best).
adj(impressive).
adj(national).
adj(international).
adj(agile).
adj(strong).
adj(regional).
adj(first).
adj(personal).
adj(freestyle).
adj(daily).
adj(gold).
adj(artistic).
adj(whitewater).
adj(rhythmic).
adj(paddle).
adj(delicate).
adj(red).
adj(powerful).
adj(slow).
adj(black).

% ADVERBIOS
adverbio(eng, adv(X)) --> [X], {adv(X)}.
adv(little).
adv(quite).
adv(very).
adv(only).
adv(yesterday).
adv(together).
adv(every).
adv(daily).
adv(monthly).
adv(around).
adv(to_improve).
adv(successfully).
adv(after).
adv(before).
adv(only).
adv(very).

% PREPOSICIONES
preposicion(eng, prep(X)) --> [X], {prep(X)}.
prep(at).
prep(on).
prep(in).
prep(a).
prep(to).
prep(for).
prep(of).
prep(in).
prep(on).
prep(with).
prep(around).
prep(to).
prep(at).
prep(after).
prep(during).
prep(on_the_podium).
prep(on_the_beach).
prep(on_weekends).
prep(before_school).
