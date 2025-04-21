



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


oracion_con_subordinada(eng, [o(SujMain, GVMain), o(SujSub, GVSubAdjusted)]) -->
    (g_nombre_propio(eng, SujMain); g_nominal(eng, SujMain)),
    g_relativos(eng, rel(_)), 
    (g_nombre_propio(eng, SujSub); g_nominal(eng, SujSub)),
    g_verbal(eng, GVSub),
    { GVSubAdjusted = gv(GVSub, SujMain) },
    g_verbal(eng, GVMain).


//Esta opción se está usando??//

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
g_nominal(eng, gn(D, ADJ, N)) --> determinante(eng, D), adjetivo(eng, ADJ),nombre(eng, N).
g_nominal(eng, gn(ADJ, N)) --> adjetivo(eng, ADJ), nombre(eng, N).



g_verbal(eng, gv(V, ADV, PREP, ADJ, N)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        g_preposicional(eng, PREP),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N).
    
g_verbal(eng, gv(V, ADV, PREP, N)) -->
        verbo(eng, V),
        g_adverbial(eng, ADV),
        g_preposicional(eng, PREP),
        g_nominal(eng, N).

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




/*ESTA VA A SER MUY ESPECÍFICA, 
SE PUEDE DEJAR ASÍ O GENERALIZAR MÁS ADELANTE
*/
g_verbal(eng, gv(V, PREP1, N1, PREP2, N2)) -->
        verbo(eng, V),
        g_preposicional(eng, PREP1),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP2),
        g_nominal(eng, N2).

g_verbal(eng, gv(V, PREP, N)) -->
        verbo(eng, V),
        g_preposicional(eng, PREP),
        g_nominal(eng, N).

g_verbal(eng, gv(V1, PREP, V2, N)) -->
        verbo(eng, V1),
        g_preposicional(eng, PREP),
        verbo(eng, V2),
        g_nominal(eng, N).

g_verbal(eng, gv(V, N1,PREP, N2)) -->
        verbo(eng, V),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP),
        g_nominal(eng, N2).

g_verbal(eng, gv(V, N1,PREP1, N2, PREP2, N3)) -->
        verbo(eng, V),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP1),
        g_nominal(eng, N2),
        g_preposicional(eng, PREP2),
        g_nominal(eng, N3).


g_verbal(eng, gv(V1, PREP1, ADV, ADJ, N1, PREP2, V2, N2)) -->
        verbo(eng, V1),
        g_preposicional(eng, PREP1),
        g_adverbial(eng, ADV),
        g_adjetival(eng, ADJ),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP2),
        g_verbal(eng, V2),
        g_nominal(eng, N2).

g_verbal(eng, gv(V1,N1, PREP, ADJ, V2)) -->
        verbo(eng, V1),
        g_nominal(eng, N1),
        g_preposicional(eng, PREP),
        g_adjetival(eng, ADJ),
        verbo(eng, V2).


g_verbal(eng, gv(V, N1, PREP, N2))-->
    verbo(eng, V),
    g_nominal(eng, N1),
    g_preposicional(eng, PREP),
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
det(an).
det(his).
det(her).
det(first).
det(several).

% NOMBRES
% Caso triple:
nombre(eng, n(Nombre)) -->
    [X, Y, Z],
    { nombre_compuesto(X, Y),
      nombre_compuesto(Y, Z),
      atomic_list_concat([X, Y, Z], '_', Nombre) }.

% Caso doble:
nombre(eng, n(Nombre)) -->
    [X, Y],
    { nombre_compuesto(X, Y),
      atomic_list_concat([X, Y], '_', Nombre) }.

% Caso simple:    
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
n(tennis).
n(table_tennis).
n(long_jump).
n(weekends).
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
n(club).
n(gymnastics).
n(day).
n(sports).
n(jump).
n(handball).
n(part).
n(champion).
n(beach).
n(intelligence).
n(world).
n(handbag).
n(chinese).
n(week).
n(girl).
n(exgirlfriend).


nombre_compuesto(climbing, wall).
nombre_compuesto(word, processor).
nombre_compuesto(paddle, tennis).
nombre_compuesto(golden, ball).
nombre_compuesto(basketball, player).
nombre_compuesto(football, player).
nombre_compuesto(sports, dance).
nombre_compuesto(dance, couple).
nombre_compuesto(volleyball, team).
nombre_compuesto(jump, record).
nombre_compuesto(handball, team).
nombre_compuesto(triathlon, competition).
nombre_compuesto(cycling, race).
nombre_compuesto(chess, player).
nombre_compuesto(tennis, championship).
nombre_compuesto(athletics, competition).
nombre_compuesto(whitewater, kayaking).
nombre_compuesto(table, tennis).
nombre_compuesto(wrestling, championship).


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
verbo(eng, v(practices, G)) --> [practices, G], {gerundio(G) }.
verbo(eng, v(enjoys, G)) --> [enjoys, G], {gerundio(G) }.
verbo(eng, v(goes, G)) --> [goes, G], {gerundio(G) }.
verbo(eng, v(is, P)) --> [is, P], { pasado(P) }.
verbo(eng, v(has, P)) --> [has, P], { pasado(P) }.
verbo(eng, v(has, been ,P)) --> [has, been,P], { pasado(P) }.
verbo(eng, v(was, P)) --> [was, P], { pasado(P) }.


v(is).
v(are).
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
v(compete).
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
v(swimming).
v(play).
v(greeted).
v(likes).




gerundio(studying).
gerundio(writing).
gerundio(climbing).
gerundio(drinking).
gerundio(singing).
gerundio(eating).
gerundio(swimming).
gerundio(taking).
gerundio(skiing).
gerundio(snowboarding).
gerundio(playing).
gerundio(changing).
gerundio(reading).

pasado(used).
pasado(won).
pasado(elected).
pasado(broken).
pasado(climbed).
pasado(selected).
pasado(named).
pasado(beaten).
pasado(achieved).
pasado(imported).



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
adj(synchronised).
adj(delicate).
adj(red).
adj(powerful).
adj(slow).
adj(black).
adj(long).
adj(next).
adj(junior).
adj(new).
adj(artificial).
adj(advanced).
adj(last).
adj(attractive).


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





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ORACIONES JAPONESAS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
oracion_japones(jpn, [O | Os]) -->
    (oracion_sujeto_omitido_jpn(jpn, O))
    ;
    (oracion_simple_jpn(jpn, O))
    ;
    (oracion_sujeto_omitido_jpn(jpn, O),
    (g_soushite_jpn(jpn, P)),
    oracion_japones(eng, Os))
    ;
    ((oracion_simple_jpn(jpn, O)),
    (g_soushite_jpn(jpn, P)),
    oracion_japones(jpn, Os))
    .


oracion_sujeto_omitido_jpn(jpn, OracionesTotales) -->
    (g_nombre_propio_jpn(jpn, Suj); g_nominal_jpn(jpn, Suj)),
    g_particula_jpn(jpn,P1),
    g_verbal_jpn(jpn, GV1),
    g_soushite_jpn(jpn,S),
    g_verbal_jpn(jpn, GV2),
    {
        sujetos_de_compuesto_jpn(Suj, Sujetos),
        GVsIniciales = [GV1, GV2]
    },
    (
        (g_soushite_jpn(jpn, S)),
        oracion_sujeto_omitido_resto_verbs_collect_jpn(jpn, GVsResto),
        { append(GVsIniciales, GVsResto, TodosGVs),
          generar_oraciones(Sujetos, TodosGVs, OracionesTotales)
        }
    ;
        { generar_oraciones(Sujetos, GVsIniciales, OracionesTotales) }
    ).

% Recolecta todos los GV restantes recursivamente
oracion_sujeto_omitido_resto_verbs_collect_jpn(jpn, [GV | Resto]) -->
    g_verbal_jpn(jpn, GV),
    (
        (g_particula_jpn(jpn, P)),
        oracion_sujeto_omitido_resto_verbs_collect_jpn(jpn, Resto)
    ;
        { Resto = [] }
    ).


oracion_simple_jpn(jpn, o(Suj, P, GV)) -->
    (g_nombre_propio_jpn(jpn, Suj); g_nominal_jpn(jpn, Suj)),
    g_particula_jpn(jpn, P),
    g_verbal_jpn(jpn, GV).



% Descompone un sujeto compuesto en una lista de sujetos individuales
sujetos_de_compuesto_jpn(g_nom_prop_jpn(NP1, NP2), [g_nom_prop_jpn(NP1), g_nom_prop_jpn(NP2)]) :- !. 
sujetos_de_compuesto_jpn(Sujeto, [Sujeto]).

g_verbal_jpn(jpn, gv(N, P, OBJ)) -->
        g_nominal_jpn(jpn, N),
        g_particula_jpn(jpn, P),
        (adjetivo_jpn(jpn, OBJ) ;verbo_jpn(jpn, OBJ)).

g_verbal_jpn(jpn, gv(N1, P1, OBJ1, P2,N2, P3, OBJ2)) -->
        g_nominal_jpn(jpn, N1),
        g_particula_jpn(jpn, P1),
        (adjetivo_jpn(jpn, OBJ1) ;verbo_jpn(jpn, OBJ1)),
        g_particula_jpn(jpn, P2),
        g_nominal_jpn(jpn, N2),
        g_particula_jpn(jpn, P3),
        (adjetivo_jpn(jpn, OBJ2) ;verbo_jpn(jpn, OBJ2)).

g_verbal_jpn(jpn, gv(N1, P1, OBJ1, N2, P2, OBJ2)) -->
        g_nominal_jpn(jpn, N1),
        g_particula_jpn(jpn, P1),
        (adjetivo_jpn(jpn, OBJ1) ;verbo_jpn(jpn, OBJ1)),
        g_nominal_jpn(jpn, N2),
        g_particula_jpn(jpn, P2),
        (adjetivo_jpn(jpn, OBJ2) ;verbo_jpn(jpn, OBJ2)).

g_verbal_jpn(jpn, gv(N1, P1, N2, P2, OBJ)) -->
        g_nominal_jpn(jpn, N1),
        g_particula_jpn(jpn, P1),
        g_nominal_jpn(jpn, N2),
        g_particula_jpn(jpn, P2),
        (adjetivo_jpn(jpn, OBJ) ;verbo_jpn(jpn, OBJ)).

g_adjetival_jpn(jpn, gadj(ADJ)) --> adjetivo_jpn(jpn, ADJ).
g_adverbial_jpn(jpn, gadv(ADV)) --> adverbio_jpn(jpn, ADV).

% CONJUNCIONES
g_particula_jpn(jpn, part(wa)) --> [wa].
g_particula_jpn(jpn, part(ni)) --> [ni].
g_particula_jpn(jpn, part(demo)) --> [demo].
g_particula_jpn(jpn, part(kedo)) --> [kedo].
g_particula_jpn(jpn, part(to)) --> [to].
g_particula_jpn(jpn, part(ga)) --> [ga].
g_particula_jpn(jpn, part(wo)) --> [wo].

g_soushite_jpn(jpn,part(soushite))-->[soushite].

% GRUPOS SINTÁCTICOS
g_nominal_jpn(jpn, gn(N)) --> nombre_jpn(jpn, N).
g_nominal_jpn(jpn, gn(D,N)) --> determinante_jpn(jpn, D), nombre_jpn(jpn, N).
g_nominal_jpn(jpn, gn(D, ADJ, N)) --> determinante_jpn(jpn, D), adjetivo_jpn(jpn, ADJ),nombre_jpn(jpn, N).
g_nominal_jpn(jpn, gn(ADJ, N)) --> adjetivo_jpn(jpn, ADJ), nombre_jpn(jpn, N).


% Nombre propio simple
g_nombre_propio_jpn(jpn, g_nom_prop(NP)) -->
    nombre_propio_jpn(jpn, NP).

% Nombre propio compuesto con conjunción
g_nombre_propio_jpn(jpn, g_nom_prop(NP1, NP2)) -->
    nombre_propio_jpn(jpn, NP1),
    g_particula_jpn(jpn, _),
    nombre_propio_jpn(jpn, NP2).

    
g_nombre_propio_jpn(jpn, g_nom_prop(NP1, NP2, NPs)) -->
    nombre_propio_jpn(jpn, NP1),
    g_particula_jpn(jpn, _),
    nombre_propio_jpn(jpn, NP2),
    g_nombre_propio_resto_jpn(jpn, NPs).

% Recolecta los nombres propios adicionales recursivamente
g_nombre_propio_resto_jpn(jpn, [NP | NPs]) -->
    g_particula_jpn(jpn, _),
    nombre_propio_jpn(jpn, NP),
    g_nombre_propio_resto_jpn(jpn, NPs).
g_nombre_propio_resto_jpn(jpn, []) --> [].






nombre_propio_jpn(jpn, n_p(X)) --> [X], {n_p(X)}.



adjetivo_jpn(jpn, adj(X)) --> [X], {adj(X)}.

adverbio_jpn(jpn, adv(X)) --> [X], {adv(X)}.

verbo_jpn(jpn, v(Y)) --> [Y], {v(X,Y)}.
verbo_jpn(jpn, v(X)) --> [X], {v(X)}.
%verbo_jpn(jpn, v(is, G)) --> [is, G], { gerundio(G) }.
%verbo_jpn(jpn, v(was, G)) --> [was, G], { gerundio(G) }.
%verbo_jpn(jpn, v(are, G)) --> [are, G], { gerundio(G) }.
%verbo_jpn(jpn, v(practices, G)) --> [practices, G], {gerundio(G) }.
%verbo_jpn(jpn, v(enjoys, G)) --> [enjoys, G], {gerundio(G) }.
%verbo_jpn(jpn, v(goes, G)) --> [goes, G], {gerundio(G) }.
%verbo_jpn(jpn, v(is, P)) --> [is, P], { pasado(P) }.
%verbo_jpn(jpn, v(has, P)) --> [has, P], { pasado(P) }.
%verbo_jpn(jpn, v(has, been ,P)) --> [has, been,P], { pasado(P) }.
%verbo_jpn(jpn, v(was, P)) --> [was, P], { pasado(P) }.


% NOMBRES
% Caso triple:
nombre_jpn(jpn, n(Nombre)) -->
    [X, Y, Z],
    { nombre_compuesto(X, Y),
      nombre_compuesto(Y, Z),
      atomic_list_concat([X, Y, Z], '_', Nombre) }.

% Caso doble:
nombre_jpn(jpn, n(Nombre)) -->
    [X, Y],
    { nombre_compuesto(X, Y),
      atomic_list_concat([X, Y], '_', Nombre) }.

% Caso simple:    
nombre_jpn(jpn, n(X)) --> [X], {n(X)}.


determinante_jpn(jpn, det(X)) --> [X], {det(X)}.


n_p(HARUKO).
n_p(YUI).
n_p(KANA).

n(hana).
n(koohii).
n(saakaa).
n(manga).
n(eigakan).
n(bangohan).
n(resutoran).


adj(suki).
adj(kirai).

v(itte).
v(tabemasu).
v(ikimasu).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEJORAS DE DETECTAR COMPLEMENTOS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Detecta el complemento directo recorriendo todos los hijos del gv
detectar_complemento_directo(o(_, GV), CD) :-
    write('Procesando GV: '), writeln(GV), % Depuración
    buscar_gn_en_termino(GV, CD), !.

% Caso en el que no hay complemento directo
detectar_complemento_directo(_, none).

% Busca un gn en un término compuesto
buscar_gn_en_termino(Termino, CD) :-
    write('Procesando término: '), writeln(Termino), % Depuración
    functor(Termino, _, Arity), % Obtiene el número de argumentos del término
    buscar_en_argumentos_CD(1, Arity, Termino, CD).

% Recorre los argumentos de un término compuesto y busca todos los gn
buscar_en_argumentos_CD(Pos, Arity, Termino, CD) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, gn, _) -> write('Encontrado CD en argumento: '), writeln(Arg), CD = Arg, ! % Busca recursivamente en el argumento
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CD(NextPos, Arity, Termino, CD)).

%Detectar el complemento indirecto recorriendo todos los hijos del gv
detectar_complemento_indirecto(o(_, GV), CI) :-
    write('Procesando GV: '), writeln(GV), % Depuración
    buscar_gn_prep_en_termino(GV, CI), !.

% Caso en el que no hay complemento indirecto
detectar_complemento_indirecto(_, none).

% Busca un gn en un término compuesto
buscar_gn_prep_en_termino(Termino, CI) :-
    write('Procesando término: '), writeln(Termino), % Depuración
    functor(Termino, _, Arity), % Obtiene el número de argumentos del término
    buscar_en_argumentos_CI(1, Arity, Termino, CI).

% Recorre los argumentos de un término compuesto y busca todos los gn
buscar_en_argumentos_CI(Pos, Arity, Termino, CI) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, gp, _) -> PosNew is Pos + 1, arg(PosNew, Termino, Arg1), 
    ArgF = (Arg, Arg1), % Concatenar el argumento y el complemento
    write('Encontrado CI en argumento: '), writeln(ArgF), 
    CI = ArgF, ! % Busca recursivamente en el argumento
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CI(NextPos, Arity, Termino, CI)).