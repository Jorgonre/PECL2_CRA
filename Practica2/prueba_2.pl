
:- module(prueba_2, [oracion/4]). 


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

% ADVERBIOS
adverbio(eng, adv(X)) --> [X], {adv(X)}.
adv(little).
adv(quite).
adv(very).
adv(only).
adv(yesterday).
% PREPOSICIONES
preposicion(eng, prep(X)) --> [X], {prep(X)}.
prep(at).
prep(on).
prep(in).
prep(a).
prep(to).
prep(for).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MEJORAS DE DETECTAR COMPLEMENTOS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lugar((gp(prep(on)),gn(det(the),n(climbing_wall)))).
tiempo((gadv(adv(yesterday)))).
tiempo((gp(prep(in)),gn(det(the),n(afternoons)))).

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
    (functor(Arg, gp, _) -> CD = none, ! % Si el argumento es un gp, no es un complemento directo
    ;functor(Arg, gn, _) -> write('Encontrado CD en argumento: '), writeln(Arg), CD = Arg, ! % Busca recursivamente en el argumento
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
    (tiempo(ArgF)
    -> CI = none, !
    ;( lugar(ArgF)
    -> CI = none, !
    ;CI = ArgF, !)) % Busca recursivamente en el argumento
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CI(NextPos, Arity, Termino, CI)).

%Detecta el complemento circunstancial de modo recorriendo todos los hijos del gv
detectar_ccm(o(_, GV), CCM) :-
    write('Procesando GV: '), writeln(GV), % Depuración
    buscar_gadj_en_termino(GV, CCM), !.

% Caso en el que no hay complemento indirecto
detectar_ccm(_, none).

% Busca un gn en un término compuesto
buscar_gadj_en_termino(Termino, CCM) :-
    write('Procesando término: '), writeln(Termino), % Depuración
    functor(Termino, _, Arity), % Obtiene el número de argumentos del término
    buscar_en_argumentos_CCM(1, Arity, Termino, CCM).

% Recorre los argumentos de un término compuesto y busca todos los gn
buscar_en_argumentos_CCM(Pos, Arity, Termino, CCM) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, gadv, _) -> PosNew is Pos + 1, arg(PosNew, Termino, Arg1),
    (functor(Arg1, gadj, _) 
    -> ArgF = (Arg, Arg1), % Concatenar el argumento y el complemento
    write('Encontrado CCM en argumento: '), writeln(ArgF),
    CCM = ArgF, !  % Busca recursivamente en el argumento
    ; NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCM(NextPos, Arity, Termino, CCM))
    ; NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCM(NextPos, Arity, Termino, CCM)
    ;(functor(Arg, gadj, _) 
    -> write('Encontrado CCM en argumento: '), writeln(Arg),
    CCM = Arg, ! % Busca recursivamente en el argumento
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCM(NextPos, Arity, Termino, CCM))).

%Detecta el complemento circunstancial de lugar recorriendo todos los hijos del gv
detectar_ccl(o(_, GV), CCL) :-
    write('Procesando GV: '), writeln(GV), % Depuración
    buscar_gp_en_termino(GV, CCL), !.

% Caso en el que no hay complemento circunstancial de lugar
detectar_ccl(_, none).

% Busca un gn en un término compuesto
buscar_gp_en_termino(Termino, CCL) :-
    write('Procesando término: '), writeln(Termino), % Depuración
    functor(Termino, _, Arity), % Obtiene el número de argumentos del término
    buscar_en_argumentos_CCL(1, Arity, Termino, CCL).

% Recorre los argumentos de un término compuesto y busca todos los gn
buscar_en_argumentos_CCL(Pos, Arity, Termino, CCL) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, gp, _) -> PosNew is Pos + 1, arg(PosNew, Termino, Arg1), 
    ArgF = (Arg, Arg1), % Concatenar el argumento y el complemento
    write('Encontrado CCL en argumento: '), writeln(ArgF), 
    CCLTemp = ArgF,  % Busca recursivamente en el argumento
    (lugar(CCLTemp)
    -> CCL = ArgF), ! % Asignar el resultado a CCL
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCL(NextPos, Arity, Termino, CCL)).

%Detecta el complemento circunstancial de tiempo recorriendo todos los hijos del gv
detectar_cct(o(_, GV), CCT) :-
    write('Procesando GV: '), writeln(GV), % Depuración
    buscar_gp_temp_en_termino(GV, CCT), !.

% Caso en el que no hay complemento circunstancial de tiempo
detectar_cct(_, none).

% Busca un gn en un término compuesto
buscar_gp_temp_en_termino(Termino, CCT) :-
    write('Procesando término: '), writeln(Termino), % Depuración
    functor(Termino, _, Arity), % Obtiene el número de argumentos del término
    buscar_en_argumentos_CCT(1, Arity, Termino, CCT).

% Recorre los argumentos de un término compuesto y busca todos los gn
buscar_en_argumentos_CCT(Pos, Arity, Termino, CCT) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, gp, _) -> PosNew is Pos + 1, arg(PosNew, Termino, Arg1), 
    ArgF = (Arg, Arg1), % Concatenar el argumento y el complemento
    write('Encontrado CCT en argumento: '), writeln(ArgF), 
    CCTTemp = ArgF,  % Busca recursivamente en el argumento
    (tiempo(CCTTemp)
    -> CCT = ArgF, !
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCT(NextPos, Arity, Termino, CCT)) % Asignar el resultado a CCT
    ;(functor(Arg, gadv, _) ->
    write('Encontrado CCT en argumento: '), writeln(Arg),
    CCTTemp = Arg,
    (tiempo(CCTTemp)
    -> CCT = Arg, !
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCT(NextPos, Arity, Termino, CCT)) % Busca recursivamente en el argumento
    ;NextPos is Pos + 1, % Pasa al siguiente argumento
    buscar_en_argumentos_CCT(NextPos, Arity, Termino, CCT))).

%Transformación de frase con complementos 
transformar_frase_con_complementos(FraseOriginal, FraseTransformada) :-
    detectar_complemento_directo(FraseOriginal, CD),
    detectar_complemento_indirecto(FraseOriginal, CI),
    detectar_ccm(FraseOriginal, CCM),
    detectar_ccl(FraseOriginal, CCL),
    detectar_cct(FraseOriginal, CCT),
    write('Complemento directo: '), writeln(CD),
    write('Complemento indirecto: '), writeln(CI),
    write('Complemento circunstancial de modo: '), writeln(CCM),
    write('Complemento circunstancial de lugar: '), writeln(CCL),
    write('Complemento circunstancial de tiempo: '), writeln(CCT),
    % Realiza la transformación de la frase original
    transformar_frase_con_cd(FraseOriginal, FraseTransformadaCD),
    transformar_frase_con_ci(FraseTransformadaCD, FraseTransformadaCI),
    transformar_frase_con_ccm(FraseTransformadaCI, FraseTransformadaCCM),
    transformar_frase_con_ccl(FraseTransformadaCCM, FraseTransformadaCCL),
    transformar_frase_con_cct(FraseTransformadaCCL, FraseTransformada).

% Transformar con complemento directo (sustituyendo el CD por cd(CD))
transformar_frase_con_cd(FraseOriginal, FraseTransformada) :-
    detectar_complemento_directo(FraseOriginal, CD),
    (CD \= none
    -> sustituir_cd_en_termino(FraseOriginal, CD, FraseTransformada) % Realiza la sustitución directamente
    ; FraseTransformada = FraseOriginal). % Si no hay CD, deja la frase igual


% Sustituye el complemento directo (gn(...)) por cd(gn(...)) recorriendo los argumentos
sustituir_cd_en_termino(Termino, CD, TerminoTransformado) :-
    functor(Termino, Functor, Arity), % Obtiene el functor y la aridad del término
    sustituir_cd_en_argumentos(1, Arity, Termino, CD, TerminoTransformado, Functor).

% Recorre los argumentos de un término compuesto y realiza la sustitución
sustituir_cd_en_argumentos(Pos, Arity, Termino, CD, TerminoTransformado, Functor) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, Functor2, NewArity),
     Functor2 == gv -> % Verifica si el argumento es un término compuesto
        write('Encontrado término compuesto en argumento: '), writeln(Arg), % Depuración
        sustituir_cd_en_argumentos(1, NewArity, Arg, CD, NuevoArg, Functor2)
    ; true), % Llama recursivamente para los argumentos del término
    (Arg = CD -> % Si el argumento actual es el complemento directo
        write('Sustituyendo CD en argumento: '), writeln(Arg), % Depuración
        cd(CD) = NuevoArg, % Crea la nueva estructura etiquetada
        setarg(Pos, Termino, NuevoArg), % Sustituye el argumento en la posición Pos
        TerminoTransformado = Termino, 
        write('El término transformado es: '), writeln(TerminoTransformado), ! % El término transformado es el mismo término modificado
    ;   NextPos is Pos + 1, % Pasa al siguiente argumento
        sustituir_cd_en_argumentos(NextPos, Arity, Termino, CD, TerminoTransformado, Functor)).
sustituir_cd_en_argumentos(Pos, Arity, Termino, _, Termino, _) :-
    Pos > Arity. % Caso base: cuando Pos supera la aridad, termina

% Transformar con complemento indirecto (sustituyendo el CI por ci(CI))
transformar_frase_con_ci(FraseOriginal, FraseTransformada) :-
    detectar_complemento_indirecto(FraseOriginal, CI),
    (CI \= none
    -> sustituir_ci_en_termino(FraseOriginal, CI, FraseTransformada) % Realiza la sustitución directamente
    ; FraseTransformada = FraseOriginal). % Si no hay CI, deja la frase igual


% Sustituye el complemento indirecto (gp(prep(...)), gn(...)) por ci(gp(prep(...)), gn(...)) recorriendo los argumentos
sustituir_ci_en_termino(Termino, CI, TerminoTransformado) :-
    functor(Termino, Functor, Arity), % Obtiene el functor y la aridad del término
    sustituir_ci_en_argumentos(1, Arity, Termino, CI, TerminoTransformado, Functor).

% Recorre los argumentos de un término compuesto y realiza la sustitución
sustituir_ci_en_argumentos(Pos, Arity, Termino, CI, TerminoTransformado, Functor) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, Functor2, NewArity),
     Functor2 == gv -> % Verifica si el argumento es un término compuesto
        write('Encontrado término compuesto en argumento: '), writeln(Arg), % Depuración
        sustituir_ci_en_argumentos(1, NewArity, Arg, CI, NuevoArg, Functor2), !
    ; true), % Llama recursivamente para los argumentos del término
    arg(1, CI, Arg1), % Obtiene el primer argumento del complemento indirecto
    arg(2, CI, Arg2), % Obtiene el segundo argumento del complemento indirecto
    (NextPos is Pos + 1, % Pasa al siguiente argumento
    arg(NextPos, Termino, NextArg), % Obtiene el argumento en la posición Pos
    Arg = Arg1,
    NextArg = Arg2 -> % Si el argumento actual es el complemento directo
        write('Sustituyendo CI en argumento: '), writeln(Arg), % Depuración
        ci(Arg1) = NuevoArg1, % Crea la nueva estructura etiquetada
        ci(Arg2) = NuevoArg2, % Crea la nueva estructura etiquetada
        setarg(Pos, Termino, NuevoArg1), % Sustituye el argumento en la posición Pos
        setarg(NextPos, Termino, NuevoArg2), % Sustituye el argumento en la posición Pos
        TerminoTransformado = Termino,
        write('El término transformado es: '), writeln(TerminoTransformado), ! % El término transformado es el mismo término modificado
    ;   NextPos is Pos + 1, % Pasa al siguiente argumento
        sustituir_ci_en_argumentos(NextPos, Arity, Termino, CI, TerminoTransformado, Functor)).
sustituir_ci_en_argumentos(Pos, Arity, Termino, _, Termino, _) :-
    Pos > Arity. % Caso base: cuando Pos supera la aridad, termina

% Transformar con ccm (sustituyendo el CCM por ccm(CCM))
transformar_frase_con_ccm(FraseOriginal, FraseTransformada) :-
    detectar_ccm(FraseOriginal, CCM),
    (CCM \= none
    -> sustituir_ccm_en_termino(FraseOriginal, CCM, FraseTransformada) % Realiza la sustitución directamente
    ; FraseTransformada = FraseOriginal). % Si no hay CCM, deja la frase igual


% Sustituye el ccm recorriendo los argumentos
sustituir_ccm_en_termino(Termino, CCM, TerminoTransformado) :-
    functor(Termino, Functor, Arity), % Obtiene el functor y la aridad del término
    sustituir_ccm_en_argumentos(1, Arity, Termino, CCM, TerminoTransformado, Functor).

% Recorre los argumentos de un término compuesto y realiza la sustitución
sustituir_ccm_en_argumentos(Pos, Arity, Termino, CCM, TerminoTransformado, Functor) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, Functor2, NewArity),
     Functor2 == gv -> % Verifica si el argumento es un término compuesto
        write('Encontrado término compuesto en argumento: '), writeln(Arg), % Depuración
        sustituir_ccm_en_argumentos(1, NewArity, Arg, CCM, NuevoArg, Functor2), !
    ; true), % Llama recursivamente para los argumentos del término
    ( functor(CCM, _, ArityCCM), % Obtiene la aridad del complemento circunstancial de modo
    ArityCCM =< 1 % Verifica si la aridad es 1
    -> (Arg = CCM -> % Si el argumento actual es el ccm
        write('Sustituyendo CCM en argumento: '), writeln(Arg), % Depuración
        ccm(CCM) = NuevoArg, % Crea la nueva estructura etiquetada
        setarg(Pos, Termino, NuevoArg), % Sustituye el argumento en la posición Pos
        TerminoTransformado = Termino, 
        write('El término transformado es: '), writeln(TerminoTransformado), ! % El término transformado es el mismo término modificado
    ;   NextPos is Pos + 1, % Pasa al siguiente argumento
        sustituir_ccm_en_argumentos(NextPos, Arity, Termino, CCM, TerminoTransformado, Functor))
    ;
    arg(1, CCM, Arg1), % Obtiene el primer argumento del complemento circunstancial de modo
    arg(2, CCM, Arg2), % Obtiene el segundo argumento del complemento circunstancial de modo
    (NextPos is Pos + 1, % Pasa al siguiente argumento
    arg(NextPos, Termino, NextArg), % Obtiene el argumento en la posición Pos
    Arg = Arg1,
    NextArg = Arg2 -> % Si el argumento actual es el ccm
        write('Sustituyendo CCM en argumento: '), writeln(Arg), % Depuración
        ccm(Arg1) = NuevoArg1, % Crea la nueva estructura etiquetada
        ccm(Arg2) = NuevoArg2, % Crea la nueva estructura etiquetada
        setarg(Pos, Termino, NuevoArg1), % Sustituye el argumento en la posición Pos
        setarg(NextPos, Termino, NuevoArg2), % Sustituye el argumento en la posición Pos
        TerminoTransformado = Termino,
        write('El término transformado es: '), writeln(TerminoTransformado), ! % El término transformado es el mismo término modificado
    ;   NextPos is Pos + 1, % Pasa al siguiente argumento
        sustituir_ccm_en_argumentos(NextPos, Arity, Termino, CCM, TerminoTransformado, Functor))).
sustituir_ccm_en_argumentos(Pos, Arity, Termino, _, Termino, _) :-
    Pos > Arity. % Caso base: cuando Pos supera la aridad, termina

% Transformar con ccl (sustituyendo el CCL por ccl(CCL))
transformar_frase_con_ccl(FraseOriginal, FraseTransformada) :-
    detectar_ccl(FraseOriginal, CCL),
    (CCL \= none
    -> sustituir_ccl_en_termino(FraseOriginal, CCL, FraseTransformada) % Realiza la sustitución directamente
    ; FraseTransformada = FraseOriginal). % Si no hay CCL, deja la frase igual


% Sustituye el ccl recorriendo los argumentos
sustituir_ccl_en_termino(Termino, CCL, TerminoTransformado) :-
    functor(Termino, Functor, Arity), % Obtiene el functor y la aridad del término
    sustituir_ccl_en_argumentos(1, Arity, Termino, CCL, TerminoTransformado, Functor).

% Recorre los argumentos de un término compuesto y realiza la sustitución
sustituir_ccl_en_argumentos(Pos, Arity, Termino, CCL, TerminoTransformado, Functor) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, Functor2, NewArity),
     Functor2 == gv -> % Verifica si el argumento es un término compuesto
        write('Encontrado término compuesto en argumento: '), writeln(Arg), % Depuración
        sustituir_ccl_en_argumentos(1, NewArity, Arg, CCL, NuevoArg, Functor2), !
    ; true), % Llama recursivamente para los argumentos del término
    arg(1, CCL, Arg1), % Obtiene el primer argumento del complemento indirecto
    arg(2, CCL, Arg2), % Obtiene el segundo argumento del complemento indirecto
    (NextPos is Pos + 1, % Pasa al siguiente argumento
    arg(NextPos, Termino, NextArg), % Obtiene el argumento en la posición Pos
    Arg = Arg1,
    NextArg = Arg2 -> % Si el argumento actual es el complemento circunstancial del lugar
        write('Sustituyendo CCL en argumento: '), writeln(Arg), % Depuración
        ccl(Arg1) = NuevoArg1, % Crea la nueva estructura etiquetada
        ccl(Arg2) = NuevoArg2, % Crea la nueva estructura etiquetada
        setarg(Pos, Termino, NuevoArg1), % Sustituye el argumento en la posición Pos
        setarg(NextPos, Termino, NuevoArg2), % Sustituye el argumento en la posición Pos
        TerminoTransformado = Termino,
        write('El término transformado es: '), writeln(TerminoTransformado), ! % El término transformado es el mismo término modificado
    ;   NextPos is Pos + 1, % Pasa al siguiente argumento
        sustituir_ccl_en_argumentos(NextPos, Arity, Termino, CCL, TerminoTransformado, Functor)).
sustituir_ccl_en_argumentos(Pos, Arity, Termino, _, Termino, _) :-
    Pos > Arity. % Caso base: cuando Pos supera la aridad, termina

% Transformar con cct (sustituyendo el CCT por cct(CCT))
transformar_frase_con_cct(FraseOriginal, FraseTransformada) :-
    detectar_cct(FraseOriginal, CCT),
    (CCT \= none
    -> sustituir_cct_en_termino(FraseOriginal, CCT, FraseTransformada) % Realiza la sustitución directamente
    ; FraseTransformada = FraseOriginal). % Si no hay CCT, deja la frase igual


% Sustituye el cct recorriendo los argumentos
sustituir_cct_en_termino(Termino, CCT, TerminoTransformado) :-
    functor(Termino, Functor, Arity), % Obtiene el functor y la aridad del término
    sustituir_cct_en_argumentos(1, Arity, Termino, CCT, TerminoTransformado, Functor).

% Recorre los argumentos de un término compuesto y realiza la sustitución
sustituir_cct_en_argumentos(Pos, Arity, Termino, CCT, TerminoTransformado, Functor) :-
    Pos =< Arity, % Asegúrate de que no hemos superado el número de argumentos
    arg(Pos, Termino, Arg), % Obtiene el argumento en la posición Pos
    write('Procesando argumento: '), writeln(Arg), % Depuración
    (functor(Arg, Functor2, NewArity),
     Functor2 == gv -> % Verifica si el argumento es un término compuesto
        write('Encontrado término compuesto en argumento: '), writeln(Arg), % Depuración
        sustituir_cct_en_argumentos(1, NewArity, Arg, CCT, NuevoArg, Functor2), !
    ; true), % Llama recursivamente para los argumentos del término
    arg(1, CCT, Arg1), % Obtiene el primer argumento del complemento indirecto
    arg(2, CCT, Arg2), % Obtiene el segundo argumento del complemento indirecto
    (NextPos is Pos + 1, % Pasa al siguiente argumento
    arg(NextPos, Termino, NextArg), % Obtiene el argumento en la posición Pos
    Arg = Arg1,
    NextArg = Arg2 -> % Si el argumento actual es el complemento circunstancial del lugar
        write('Sustituyendo CCT en argumento: '), writeln(Arg), % Depuración
        cct(Arg1) = NuevoArg1, % Crea la nueva estructura etiquetada
        cct(Arg2) = NuevoArg2, % Crea la nueva estructura etiquetada
        setarg(Pos, Termino, NuevoArg1), % Sustituye el argumento en la posición Pos
        setarg(NextPos, Termino, NuevoArg2), % Sustituye el argumento en la posición Pos
        TerminoTransformado = Termino,
        write('El término transformado es: '), writeln(TerminoTransformado), ! % El término transformado es el mismo término modificado
    ;   NextPos is Pos + 1, % Pasa al siguiente argumento
        sustituir_cct_en_argumentos(NextPos, Arity, Termino, CCT, TerminoTransformado, Functor)).
sustituir_cct_en_argumentos(Pos, Arity, Termino, _, Termino, _) :-
    Pos > Arity. % Caso base: cuando Pos supera la aridad, termina