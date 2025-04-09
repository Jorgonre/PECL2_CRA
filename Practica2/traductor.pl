/*

Traductor b�sico

Ejemplo de funcionamiento:

?- oracion(eng,X,[the,man,eats],[]), oracion(esp,X,Y,[]).
X = o(gn(det(el), n(hombre)), gv()),
Y = [el, hombre, come]

*/

%oracion(esp, o(GN,GV)) --> g_nominal(esp, GN)|g_nombre_propio(esp, GN), g_verbal(esp, GV).
%oracion(esp, o_s(GN, GV)) --> oracion(esp, o_s(GN, GV)), g_conjuncion(esp, Conj), oracion(esp, o_s(GN, GV)).
%oracion(esp, o_coord(GN, GV))--> g_nominal(esp, GN), g_verbal(esp, GV).
%oracion(esp, o_comp(GN, GV))--> g_nominal(esp, GN), g_verbal(esp, GV).




%oracion(eng, o_coord(GN, GV)) --> oracion(eng, o_s(GN, GV)), g_conjuncion(eng, Conj), oracion(eng, o_s(GN, GV)).
%oracion(eng, o_s(GN, GV))--> oracion(eng, o_s(GN, GV)), g_relativos(eng, Rel), oracion(eng, o_s(GN, GV)).
%oracion(eng, o_comp(GN, GV))--> oracion(eng, o_s(GN, GV)), oracion(eng, o_comp(GN, GV)).
%oracion(eng, o_comp(GN, GV))--> oracion(eng, o_s(GN, GV)).





%g_nominal(esp, gn(N)) --> nombre(esp, N).
%g_nominal(esp, gn(D,N)) --> determinante(esp, D), nombre(esp, N).
%g_verbal(esp, gv(V)) --> verbo(esp, V).
%g_adjetival(esp, gadj(Adj)) --> adjetivo(esp, Adj).
%g_adverbial (esp, gadv(Adv)) --> adverbio(esp, Adv).
%g_preposicional (esp, gp(Prep)) --> preposicion(esp, Prep).
%g_conjuncion (esp, gc(Conj)) --> conjuncion(esp, Conj).
%g_relativos (esp, gr(Rel)) --> relativos(esp, Rel).
%g_nombre_propio(esp, g_nom_prop(Nom_Prop)) --> nombre_propio(esp, Nom_Prop).






% ReglaS base: Una oración simple
oracion(eng, GN_P, o(GN, GV, GADJ)) --> 
    (g_nombre_propio(eng, GN);g_nominal(eng, GN)), g_verbal(eng, GV), (g_adjetival(eng, GADJ); g_nominal(eng, GADJ)).

oracion(eng, GN_P, o(GV2, GADJ2)) --> 
    g_nombre_propio(eng, GN_P),g_verbal(eng, GV2), (g_adjetival(eng, GADJ2); g_nominal(eng, GADJ2)).


% Regla recursiva: Conjunción que une oraciones
oracion(eng, o(GN1, GV1, GADJ1, OracionRest)) --> 
    (g_nombre_propio(eng, GN1);g_nominal(eng, GN1)), g_verbal(eng, GV1), (g_adjetival(eng, GADJ1); g_nominal(eng, GADJ1)),
    (g_conjuncion(eng, conj(and)); g_conjuncion(eng, conj(or)) ; g_conjuncion(eng, conj(but))
    ; g_relativos(eng, rel(while)); g_relativos(eng,rel(who)); g_relativos(eng, rel(that)); 
    g_relativos(eng, rel(although))),
    oracion(eng, GN1 ,OracionRest).







% Regla para la conjunción 'and'
g_conjuncion(eng, conj(and)) --> [and].
g_conjuncion(eng, conj(or)) --> [or].
g_conjuncion(eng, conj(but)) --> [but].
g_relativos(eng, rel(while)) --> [while].
g_relativos(eng, rel(who)) --> [who].
g_relativos(eng, rel(that)) --> [that].
g_relativos(eng, rel(althoug)) --> [although].







g_nominal(eng, gn(N)) --> nombre(eng, N).
g_nominal(eng, gn(D,N)) --> determinante(eng, D), nombre(eng, N).
g_verbal(eng, gv(V1,V2)) --> verbo(eng,V1), verbo(eng, V2).
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