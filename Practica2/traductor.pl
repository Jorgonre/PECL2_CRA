% ORACIÓN con SUJETOS COORDINADOS (la que ya tienes)
oracion(eng, Oraciones) -->
    g_nominal_coord(eng, GNList),
    estructura_verbal(eng, VerbosObjs),
    { construir_oraciones(GNList, VerbosObjs, Oraciones) }.

% GRUPO NOMINAL COORDINADO (recursivo con nombres propios y/o nominales)
g_nominal_coord(eng, [GN]) --> g_nombre_propio(eng, GN).
g_nominal_coord(eng, [GN | Rest]) -->
    g_nombre_propio(eng, GN),
    g_conjuncion(eng, _),
    g_nominal_coord(eng, Rest).

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

% Caso base recursivo: cuando ya no hay mas verbos y objetos
construir_oracion_individual_aux(GN, [], Acc, Oracion) :-
    reverse(Acc, Partes),
    Oracion =.. [o, GN | Partes].

% Caso recursivo: acumula GV y OBJ
construir_oracion_individual_aux(GN, [(GV, OBJ) | Rest], Acc, Oracion) :-
    construir_oracion_individual_aux(GN, Rest, [OBJ, GV | Acc], Oracion).






% ORACIÓN SIMPLE (grupo nominal o nombre propio + verbo)

% ORACIÓN SIMPLE (grupo nominal o nombre propio + verbo + complemento)
oracion(eng, o(GN, GV, OBJ)) --> 
    (g_nombre_propio(eng, GN); g_nominal(eng, GN)), % Sujeto (nombre o nombre propio)
    g_verbal(eng, GV),                             % Verbo
    (g_adjetival(eng, OBJ); g_nominal(eng, OBJ)). % Adjetivo o complemento nominal

oracion(eng, o(GN, GV)) --> 
    (g_nombre_propio(eng, GN); g_nominal(eng, GN)), % Sujeto (nombre o nombre propio)
    g_verbal(eng, GV).                             % Verbo

% ORACIÓN COMPUESTA CON ADJETIVO O COMPLEMENTO
oracion(eng, o(GN1, GV1, OBJ, OracionRest)) --> 
    (((g_nombre_propio(eng, GN1); g_nominal(eng, GN1)), % Sujeto
    g_verbal(eng, GV1),                              % Verbo
    (g_adjetival(eng, OBJ); g_nominal(eng, OBJ)))
    ;
    ((g_nombre_propio(eng, GN1); g_nominal(eng, GN1)), % Sujeto
    g_verbal(eng, GV1),                              % Verbo
    (g_adjetival(eng, OBJ); g_nominal(eng, OBJ)),
    sentencia_recursiva(eng, GV1),
    )), 
    % Adjetivo o complemento nominal
    (g_conjuncion(eng, conj(and)); g_conjuncion(eng, conj(or)); g_conjuncion(eng, conj(but));
     g_relativos(eng, rel(while)); g_relativos(eng, rel(who)); g_relativos(eng, rel(that)); g_relativos(eng, rel(although))),
    oracion(eng, OracionRest).  % Llamada recursiva para la oración coordinada


% ORACIÓN COMPUESTA (coordinada con otra oración o relativa)
oracion(eng, o(GN1, GV1, OracionRest)) --> 
    (g_nombre_propio(eng, GN1); g_nominal(eng, GN1)), % Sujeto
    g_verbal(eng, GV1),                              % Verbo
    (g_conjuncion(eng, conj(and)); g_conjuncion(eng, conj(or)); g_conjuncion(eng, conj(but));
     g_relativos(eng, rel(while)); g_relativos(eng, rel(who)); g_relativos(eng, rel(that)); g_relativos(eng, rel(although))),

    oracion(eng, OracionRest).                        % Llamada recursiva para la oración coordinada


                      
% Regla recursiva para gestionar multiples verbos unidos por 'and'
sentencia_recursiva(eng, v(V1)) --> 
    g_conjuncion(eng, conj(and)),        % Conjunción 'and'
    g_verbal(eng, v(V1)).                % Solo un verbo mas

sentencia_recursiva(eng, v(V1, V2)) --> 
    g_conjuncion(eng, conj(and)),        % Conjunción 'and'
    g_verbal(eng, v(V2)),                % Segundo verbo
    sentencia_recursiva(eng, v(V1, V2)).  % Recursión para continuar con mas verbos (si los hay)
    




% Regla para la conjunción 'and'
g_conjuncion(eng, conj(and)) --> [and].
g_conjuncion(eng, conj(or)) --> [or].
g_conjuncion(eng, conj(but)) --> [but].
g_relativos(eng, rel(while)) --> [while].
g_relativos(eng, rel(who)) --> [who].
g_relativos(eng, rel(that)) --> [that].
g_relativos(eng, rel(although)) --> [although].







g_nominal(eng, gn(N)) --> nombre(eng, N).
g_nominal(eng, gn(D,N)) --> determinante(eng, D), nombre(eng, N).
g_verbal(eng, gv(V1,V2)) --> verbo(eng,V1),verbo(eng, V2).
g_verbal(eng, gv(V)) --> verbo(eng, V).
g_adjetival(eng, gadj(ADJ)) --> adjetivo(eng, ADJ).
g_adverbial(eng, gadv(ADJ)) --> adverbio(eng, ADJ).
g_preposicional (eng, gp(PREP)) --> preposicion(eng, PREP).
%g_conjuncion (eng, gc(CONJ)) --> conjuncion(eng, CONJ).
%g_relativos (eng, gr(REL)) --> relativos(eng, REL).
g_nombre_propio(eng, g_nom_prop(NOM_PROP)) --> nombre_propio(eng, NOM_PROP).


%determinante(esp, det(X)) --> [X],{det(X,_)}.
determinante(eng, det(X)) --> [X],{det(X)}.
%det(el,the).
%det(la,the, a). 
det(the).
det(a).

%nombre(esp, n(X)) --> [X],{n(X,_)}.
nombre(eng, n(X)) --> [X],{n(X)}.
%n(hombre,man).
%n(mujer,woman).
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
n(climbing, _).
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




nombre_propio(eng, n_p(X)) --> [X], {n_p(X)}.
n_p(JOSE).
n_p(MARY).
n_p(HECTOR).
n_p(IRENE).




verbo(eng, v(Z)) --> [Z],{v(X,Y,Z)}.
verbo(eng, v(Y)) --> [Y],{v(X,Y)}.
verbo(eng, v(X)) --> [X],{v(X)}.


v(is).
v(is, _).
v(is, _, _). //Para la estructura is used to
v(clears).
//Hay alguna forma de que se tenga solo uno para drinks?//
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
v(_, and, _).

%adjetivo(esp, adj(X)) --> [X],{adj(X,_)}.
adjetivo(eng, adj(X)) --> [X],{adj(X)}.
%adj(bonito, alto).
adj(dark-skinned).
adj(blue).
adj(tall).
adj(agile).
adj(delicate).
adj(red).
adj(powerful).
adj(slow).
adj(grey).


%adverbio(esp, adv(X)) --> [X],{adv(X,_)}.
adverbio(eng, adv(Y)) --> [Y],{adv(X,Y)}.
adverbio(eng, adv(X)) --> [X],{adv(X)}.
%adv(mucho, poco).
adv(little).
adv(quite).
adv(quite, _). %Para el "quite a"


%preposicion(esp, prep(X)) --> [X],{prep(X,_)}.
preposicion(eng, prep(X)) --> [X],{prep(X)}.
%prep(en, de).
prep(at).

%conjuncion(esp, conj(X)) --> [X],{conj(X,_)}.
%conjuncion(eng, conj(X)) --> [X],{conj(X)}.
%conj(o, y, pero).
%conj(or).

%relativos(esp, rel(X)) --> [X],{rel(X,_)}.
%relativos(eng, rel(X)) --> [X],{rel(X)}.
%rel(que, quien).

puntuacion(_, punt(X))-->[X],{punt(X)}.
%puntuacion(_, punt(X))--> [X],{punt(X,Y)}.
puntuacion(-).