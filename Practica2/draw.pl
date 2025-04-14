/*-----------------------------------------------------------------------------*/
/*  This program was written by Mark Holcomb and is hearby released into the   */
/*  public domain provided this header remains intact.  As usual, no guarantee */
/*  is made as to the correctness of the code, the suitability of this code    */
/*  for any particular application, or that the code will be maintained.       */
/*  USAGE:                                                                     */
/*      The code expects (as input), the tree structure of a sentence in a     */
/*      form similar to:   s(np(d(the),n(dog)),vp(v(ran),np(d(the),n(house)))) */
/*      and prints an ascii diagram of the structure.                          */
/*                                                                             */
/*        eg.  draw(s(np(d(the),n(dog)),vp(v(ran),np(d(the),n(house))))).      */
/*                                                                             */
/*                       s                                                     */
/*                       |                                                     */
/*          +-----------------+                                                */
/*          np                vp                                               */
/*          |                 |                                                */
/*       +------+      +----------+                                            */
/*       d      n      v          np                                           */
/*       |      |      |          |                                            */
/*       |      |      |      +-------+                                        */
/*       |      |      |      d       n                                        */
/*       |      |      |      |       |                                        */
/*       |      |      |      |       |                                        */
/*      the    dog    ran    the    house                                      */
/*                                                                             */
/*-----------------------------------------------------------------------------*/


:- module(draw, [draw/1]).


/* ---- begin draw routine ----------------------------------------------------*/

% :- ensure_loaded(library(lists)).
/* the 5 in add is the constant offset from the left edge of the screen */


draw(Struct) :-
        once(draw0(Struct)). % BL: make it deterministic

draw0(Struct) :-
        md(1,Struct,1,Depth),
        add(Depth,Struct,New_struct,5,_),
        Mod is Depth + 1,
        breadth(1,Mod,New_struct).

trunc(X,Y) :-
        name(X,NX),
        until_dot(NX,NY),
        reverse(NY,RNY),
        name(Y,RNY).

until_dot(X,Res) :- until_dot_aux(X,[],Res).
until_dot_aux([],Res,Res) :- !.
until_dot_aux([46|_],Res,Res) :- !.
until_dot_aux([H|T],Sofar,Res) :- until_dot_aux(T,[H|Sofar],Res).

%append([],X,X).
%append([X1|X2],Y,[X1|Z]) :- append(X2,Y,Z).

wspaces(0).
wspaces(Num) :- New is Num - 1, write(' '), wspaces(New).

spaces(0,Tout,Tout).
spaces(Num,Tin,Tout) :- New is Num - 1,
                        append(Tin,[32],Tmid),
                        spaces(New,Tmid,Tout).

aux_lines(_,_,Max,Max) :- !. /* if this is the last level, don't print |'s */
aux_lines(Out,Begin,_,_) :- lines(Out,Begin).

lines([],_).
lines([H|T],Cur) :- New is H - Cur, wspaces(New),
                        Next is H + 1,  
                        write('|'), lines(T,Next). 

size(Term,Size) :- name(Term,List),len(List,Size). 

len([],0).
len([_|T],Length) :- len(T,J), Length is J + 1.

dummy(~,Tin,Tout) :- !,
                       append(Tin,[124],Tout). 

dummy(Name,Tin,Tout) :- name(Name,List), append(Tin,List,Tout).

prt([]).
prt(List) :- name(Text,List),write(Text).

post(0,_) :- !.
post(1,_) :- !. 
post(_,Dout) :- prt(Dout).

end([],[],[]):- !. /* empty list sent to end */
end([H],[],H) :- !,atomic(H). /* one element list */
end([H|T],[H|Out],End) :- end(T,Out,End).

cross(2,Din,Dout,Chars) :- !, single(Din,Dout,Chars).
cross(_,Din,Dout,Chars) :- aux_cross(Din,Dout,Chars).

ifs([],Dout,Dout) :- !.
ifs(43,Dout,Dout) :- !,nl,write('Error: blank end in ifs '). %He puesto 43 para que no de error
ifs(End,Din,Dout) :- append(Din,[End],Dout).


single(Din,Dout,Chars) :- end(Din,Dmid,End),
                                          /* if [] add char - 1 spaces
                                             if + append + and char - 1 spaces
                                             if | append | and char - 1 spaces 
                                             if anything else, I messed up */
                        ifs(End,Dmid,Dmid2),
                        New is Chars - 1,
                        spaces(New,Dmid2,Dmid3),
                        append(Dmid3,[124],Dout). /* add a '|' to Dout */

dashes(0,Dout,Dout) :- !.
dashes(Num,Din,Dout) :- New is Num - 1,
                        append(Din,[45],Dmid),
                        dashes(New,Dmid,Dout). 

cont(2,Chars,Din,Dout) :- !,dashes(Chars,Din,Dout).
                        /* if ' ' append char dashes */
                                
                        
cont(End,Chars,Din,Dout) :-     New is Chars - 1,
                                ifs(End,Din,Dmid),
                                spaces(New,Dmid,Dmid1),
                                append(Dmid1,[43],Dout).

                                /* if [] add char - 1 spaces and +
                                if + append + and char - 1 spaces and + 
                                if | append | and char - 1 spaces and +
                                if anything else, I messed up */

aux_cross(Din, Dout, Chars) :-
        end(Din, Dmid1, End),
        cont(End, Chars, Dmid1, Dmid2),
        end(Dmid2, Mid, Final), % Verifica el carácter final después del primer append
        (   Final \= 43, % Si el carácter final no es '+'
            Din \= [], % Asegúrate de que no sea el principio
            Chars \= 1 % Asegúrate de que no sea el final
        ->  append(Mid, [45,43,32], Dout) % Añade un '-', un '+' y un espacio
        ;   append(Dmid2, [2], Dout) % Solo añade un espacio
        ).

choose(32,Din,Dout) :- append(Din,[32],Dout).
choose(End,Din,Dout) :-  ifs(End,Din,Dout).

check(In) :- In>0,!.
check(In) :- In=<0,write('Error: label overlap detected; suggest increasing base node seperation in loc'),nl, fail.
                                                
breadth(Max,Max,_) :- !.
breadth(Level,Max,Struct) :-
                        New is Level + 1,
                        at(Level,1,Struct,0,_,[],Out,[],Tout,[],Dout), 
                        /* print dashes, nl, text, nl, lines, nl */
                        post(Level,Dout), nl,
                        prt(Tout), nl,
                        aux_lines(Out,0,New,Max),nl,
                        breadth(New,Max,Struct).

at(1,Arity,Struct,Cur,Pos,In,Out,Tin,Tout,Din,Dout) :- !,
                        functor(Struct,Name,_),
                        arg(1,Struct,Spaces), 
                        Actual is Spaces - Cur,
                        check(Actual), 
                        spaces(Actual,Tin,Tmid),
                        T is Cur + Actual,
                        size(Name,Size), Pos is T + Size,
                        M is (Size+1)/2, trunc(M,V),
                        L is T + V - 1,
                        len(Din,Len),
                        Chars_needed is ((L + 1) - Len),
                        cross(Arity,Din,Dout,Chars_needed),
                        append(In,[L],Out),
                        dummy(Name,Tmid,Tout).


at(Level,_,Struct,Cur,Pos,In,Out,Tin,Tout,Din,Dout) :- 
                                functor(Struct,_,Arity),
                                Next_lev is Level - 1,
        for_each(1,Arity,Struct,Next_lev,Cur,Pos,In,Out,Tin,Tout,Din,Dout).

for_each(Same,Same,_,_,X,X,Y,Y,Z,Z,Din,Dout) :- !, end(Din,Mid,End),
                                                choose(End,Mid,Dout).
                                        /* if End is ' ' then append '+'
                                        else append End */

for_each(Begin,End,Struct,Level,Cur,New_Pos,In,Out,Tin,Tout,Din,Dout) :-
                                Count is Begin + 1, 
                                arg(Count,Struct,Sub),
                        at(Level,End,Sub,Cur,Pos,In,Mid,Tin,TMid,Din,Dmid),
      for_each(Count,End,Struct,Level,Pos,New_Pos,Mid,Out,TMid,Tout,Dmid,Dout).

/* ------ end draw  ------------- max depth begin --------------------------*/

md(Level,Struct,Cur,Level) :- functor(Struct,_,0), Level >= Cur, !.
md(Level,Struct,Cur,Cur) :- functor(Struct,_,0), Cur < Level, !.

md(Level,Struct,Cur,Max) :- functor(Struct,_,Arity),
                                Next is Level + 1,
                                foreach(0,Arity,Struct,Cur,Max,Next).

foreach(Same,Same,_,Max,Max,_) :- !.

foreach(Begin,End,Struct,Cur,Max,Level) :- Count is Begin + 1,
                                        arg(Count,Struct,Sub),
                                        md(Level,Sub,Cur,Cmax),
                                       foreach(Count,End,Struct,Cmax,Max,Level).
/*--- max depth end ----------------------------------------------------------*/

/* the 4 in "New is (Cur + Size) + 4" below is the spacing between base nodes
   of the tree.  If you are having label overlap problems, increase it and
   they might go away */

loc(Struct,Cur,New,New_struct,Adj) :-   size(Struct,Size), 
                                        New is (Cur + Size) + 5,
                                        functor(Struct,Name,Arity),
                                        NArity is Arity + 1,
                                        functor(New_struct,Name,NArity),
                                        M is (Size + 1)/2, trunc(M,V), 
                                        Adj is Cur + (V - 1). 

al(0,Struct,New_struct,Cur,New,Adj) :- loc(Struct,Cur,New,New_struct,Adj),
                                        arg(1,New_struct,Cur).

/* everytime I get to bottom of branch, and level is not equal to maxdepth,
   add a level */

al(Needed_Depth,Struct,New_struct,Cur,Npos,Adj)  :- New is Needed_Depth - 1, 
                                        al(New,Struct,Tstruct,Cur,Npos,Adj),
                                        functor(New_struct,'~',2),
                                        arg(2,New_struct,Tstruct),
                                        arg(1,New_struct,Adj).

/* this is the case where we are at the leaf and it is the right depth */

add(1,Struct,New_struct,Cur,New) :- !,
                                        loc(Struct,Cur,New,New_struct,_),
                                        arg(1,New_struct,Cur).

/* this is the case where the leaf is not deep enough (add dummys) */
add(Depth,Struct,New_struct,Cur,New)  :- functor(Struct,_,0),!,
                                Next is Depth - 1,
                                /* insert add dummy structs here and return
                                   the result as New_struct */
                                al(Next,Struct,New_struct,Cur,New,_).

add(Depth,Struct,New_struct,Cur,New) :- functor(Struct,_,Arity),
                                Next is Depth - 1, 
                        fore(0,Arity,Struct,Next,New_struct,Cur,New,_,_).

fore(Same,Same,Struct,_,New_struct,Cur,Cur,First,Size) :- !,
                                                functor(Struct,Name,Arity), 
                                                NArity is Arity + 1,
                                                functor(New_struct,Name,NArity),
/*the pos of this struct is (first + cur)/2 or first depending on single
  or multiple sub structures */
                                                size(Name,Nsize),
                                        case(Same,Cur,Size,First,Pos,Nsize),
                                                 arg(1,New_struct,Pos).

fore(Begin,End,Struct,Cur_depth,New_struct,Cur,New,Ifst,Isize) :- 
                                                Count is Begin + 1,
                                                arg(Count,Struct,Sub),
                                        add(Cur_depth,Sub,Sub1,Cur,Mid),
                                                arg(1,Sub1,Spos),
                                                functor(Sub1,Name,_),
                                                size(Name,Nsize),
                                first(Count,Spos,Isize,Nsize,Size,Ifst,First),
        fore(Count,End,Struct,Cur_depth,New_struct,Mid,New,First,Size),
                                                Newcount is Count + 1,
                                                arg(Newcount,New_struct,Sub1).

first(1,Cur,_,Size,Size,_,Cur) :-!.
first(_,_,Size,_,Size,Same,Same).

/* single or multiple subargs; if End is 1, then single, else multiple */ 

case(1,_,Size,First,Pos,Nsize) :- !,T is First + ((Size+1)/2),
                                trunc(T,V), Mid is V - 1, 
                                T2 is ((Nsize+1)/2) - 1,
                                trunc(T2,T3),
                                Pos is Mid - T3.

case(_,Cur,_,First,Pos,_) :- Temp is ((Cur - 4) + First + 1)/2,
                        trunc(Temp,V), Pos is V - 1.



%PRUEBA s(np(d(el), n(perro)), vp(v(corre), prep(a), np(d(el), n(parque))))

% Imprime las hojas de un árbol como una oración y la subraya con guiones
imprimir_frase_subrayada(Arbol) :-
        % Imprimir la frase
        imprimir_frase(Arbol),
        nl,
        % Subrayar la frase con guiones
        imprimir_palabra_subrayada(Arbol),
        nl,
        imprimir_etiqueta_centrada(Arbol),
        nl,
        %Imprimir complementos
        imprimir_complementos(Arbol),
        nl,
        % Calcular la longitud del sujeto y del predicado
        Arbol = s(NP, VP),
        calcular_longitud_frase(NP, LongitudNP),
        calcular_longitud_frase(VP, LongitudVP),
        % Subrayar la frase nominal con guiones
        dashes(LongitudNP, [], GuionesNP),
        prt(GuionesNP),
        write(' '),
        % Subrayar la frase verbal con guiones
        dashes(LongitudVP, [], GuionesVP),
        prt(GuionesVP), nl,
        % Imprimir la etiqueta del sujeto y del predicado
        imprimir_etiqueta_suj_y_pred(Arbol), nl.

% Imprime las hojas de un árbol como una oración
imprimir_frase(d(Palabra)) :- 
        write(Palabra). % Nodo determinante

imprimir_frase(pn(Palabra)) :- 
        write(Palabra). % Nodo nombre propio

imprimir_frase(n(Palabra)) :- 
        write(Palabra). % Nodo sustantivo

imprimir_frase(v(Palabra)) :- 
        write(Palabra). % Nodo verbo

imprimir_frase(prep(Palabra)) :- 
        write(Palabra). % Nodo verbo

% Devuelve un nuevo término vp con todos los hijos menos el primero
vp_sin_primero(Term, NuevoTerm) :-
        functor(Term, Nombre, Arity), % Obtiene el nombre y la aridad del término original
        Arity > 1, % Asegúrate de que haya mas de un hijo
        ArityMenosUno is Arity - 1, % Calcula la nueva aridad
        functor(NuevoTerm, Nombre, ArityMenosUno), % Crea un nuevo término con aridad reducida
        copiar_hijos_desde(Term, NuevoTerm, 2, Arity, 1). % Copia los hijos desde el segundo

% Devuelve un nuevo término np con todos los hijos menos el primero
np_sin_primero(Term, NuevoTerm) :-
        functor(Term, Nombre, Arity), % Obtiene el nombre y la aridad del término original
        Arity > 1, % Asegúrate de que haya mas de un hijo
        ArityMenosUno is Arity - 1, % Calcula la nueva aridad
        functor(NuevoTerm, Nombre, ArityMenosUno), % Crea un nuevo término con aridad reducida
        copiar_hijos_desde(Term, NuevoTerm, 2, Arity, 1). % Copia los hijos desde el segundo

% Caso base: cuando el índice supera el número de hijos, detener
copiar_hijos_desde(_, _, Index, Arity, _) :-
        Index > Arity, !.
% Caso recursivo: copia el hijo actual al nuevo término
copiar_hijos_desde(Term, NuevoTerm, Index, Arity, NuevoIndex) :-
        arg(Index, Term, Hijo), % Obtiene el hijo en la posición actual
        arg(NuevoIndex, NuevoTerm, Hijo), % Copia el hijo al nuevo término
        NextIndex is Index + 1, % Incrementa el índice del término original
        NextNuevoIndex is NuevoIndex + 1, % Incrementa el índice del nuevo término
        copiar_hijos_desde(Term, NuevoTerm, NextIndex, Arity, NextNuevoIndex). % Llama recursivamente

imprimir_frase(s(NP, VP)) :- 
        imprimir_frase(NP), write(' '), imprimir_frase(VP). % Nodo oración

imprimir_frase(SUJ) :- 
        ( SUJ = np(Izq)
        ->  imprimir_frase(Izq) % Nodo frase verbal
        ; functor(SUJ, np, Arity), Arity > 1, 
          arg(1, SUJ, Izq), % Obtener el primer hijo
          np_sin_primero(SUJ, Resto), % Obtener los demás hijos
          imprimir_frase(Izq), write(' '), imprimir_frase(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_frase(PRED) :- 
        ( PRED = vp(Izq)
        ->  imprimir_frase(Izq) % Nodo frase verbal
        ; functor(PRED, vp, Arity), Arity > 1, 
          arg(1, PRED, Izq), % Obtener el primer hijo
          vp_sin_primero(PRED, Resto), % Obtener los demás hijos
          imprimir_frase(Izq), write(' '), imprimir_frase(Resto) % Imprimir el primer hijo y luego los demás
        ).

% Calcula la longitud total de la frase generada por el árbol
calcular_longitud_frase(d(Palabra), Longitud) :-
        name(Palabra, Lista), % Convierte el átomo en una lista de caracteres
        length(Lista, Longitud).

calcular_longitud_frase(pn(Palabra), Longitud) :-
        name(Palabra, Lista), 
        length(Lista, Longitud).

calcular_longitud_frase(n(Palabra), Longitud) :-
        name(Palabra, Lista),
        length(Lista, Longitud).

calcular_longitud_frase(v(Palabra), Longitud) :-
        name(Palabra, Lista),
        length(Lista, Longitud).

calcular_longitud_frase(prep(Palabra), Longitud) :-
        name(Palabra, Lista),
        length(Lista, Longitud).

calcular_longitud_frase(s(NP, VP), Longitud) :-
        calcular_longitud_frase(NP, LongNP),
        calcular_longitud_frase(VP, LongVP),
        Longitud is LongNP + LongVP + 1. % Suma 1 por el espacio entre sujeto y predicado

calcular_longitud_frase(SUJ, Longitud) :-
        (   SUJ = np(Izq) % Si solo hay un hijo
        ->      calcular_longitud_frase(Izq, Longitud) % Nodo frase nominal
        ;   functor(SUJ, np, Arity), Arity > 1 ->
                arg(1, SUJ, Izq), % Obtener el primer hijo
                vp_sin_primero(SUJ, Resto), % Obtener los demás hijos
                calcular_longitud_frase(Izq, LongIzq),
                calcular_longitud_frase(Resto, LongResto),
                Longitud is LongIzq + LongResto + 1 % Suma 1 por el espacio entre palabras
        ).

calcular_longitud_frase(PRED, Longitud) :-
        (   PRED = vp(Izq) % Si solo hay un hijo
        ->      calcular_longitud_frase(Izq, Longitud) % Nodo frase verbal
        ;   functor(PRED, vp, Arity), Arity > 1 ->
                arg(1, PRED, Izq), % Obtener el primer hijo
                vp_sin_primero(PRED, Resto), % Obtener los demás hijos
                calcular_longitud_frase(Izq, LongIzq),
                calcular_longitud_frase(Resto, LongResto),
                Longitud is LongIzq + LongResto + 1 % Suma 1 por el espacio entre palabras
        ).

% Subraya cada una de las hojas de un árbol

imprimir_palabra_subrayada(d(Palabra)) :- 
        calcular_longitud_frase(d(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo determinante

imprimir_palabra_subrayada(pn(Palabra)) :- 
        calcular_longitud_frase(pn(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo nombre propio

imprimir_palabra_subrayada(n(Palabra)) :- 
        calcular_longitud_frase(n(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo sustantivo

imprimir_palabra_subrayada(v(Palabra)) :- 
        calcular_longitud_frase(v(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo verbo

imprimir_palabra_subrayada(prep(Palabra)) :- 
        calcular_longitud_frase(prep(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo preposición

imprimir_palabra_subrayada(s(NP, VP)) :- 
        imprimir_palabra_subrayada(NP), write(' '), imprimir_palabra_subrayada(VP). % Nodo oración

imprimir_palabra_subrayada(SUJ) :- 
        ( SUJ = np(Izq)
        ->  imprimir_palabra_subrayada(Izq) % Nodo frase nominal
        ; functor(SUJ, np, Arity), Arity > 1, 
          arg(1, SUJ, Izq), % Obtener el primer hijo
          vp_sin_primero(SUJ, Resto), % Obtener los demás hijos
          imprimir_palabra_subrayada(Izq), write(' '), imprimir_palabra_subrayada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_palabra_subrayada(PRED) :- 
        ( PRED = vp(Izq)
        ->  imprimir_palabra_subrayada(Izq) % Nodo frase verbal
        ; functor(PRED, vp, Arity), Arity > 1, 
          arg(1, PRED, Izq), % Obtener el primer hijo
          vp_sin_primero(PRED, Resto), % Obtener los demás hijos
          imprimir_palabra_subrayada(Izq), write(' '), imprimir_palabra_subrayada(Resto) % Imprimir el primer hijo y luego los demás
        ).


% Imprime solo la etiqueta centrada debajo de los guiones para un determinante
imprimir_etiqueta_centrada(d(Palabra)) :- 
        calcular_longitud_frase(d(Palabra), Longitud),
        centrar_etiqueta('d', Longitud).

imprimir_etiqueta_centrada(pn(Palabra)) :- 
        calcular_longitud_frase(d(Palabra), Longitud),
        centrar_etiqueta('pn', Longitud).

% Imprime solo la etiqueta centrada debajo de los guiones para un sustantivo
imprimir_etiqueta_centrada(n(Palabra)) :- 
        calcular_longitud_frase(n(Palabra), Longitud),
        centrar_etiqueta('n', Longitud).

% Imprime solo la etiqueta centrada debajo de los guiones para un verbo
imprimir_etiqueta_centrada(v(Palabra)) :- 
        calcular_longitud_frase(v(Palabra), Longitud),
        centrar_etiqueta('v', Longitud).

% Imprime solo la etiqueta centrada debajo de los guiones para una preposición
imprimir_etiqueta_centrada(prep(Palabra)) :- 
        calcular_longitud_frase(prep(Palabra), Longitud),
        centrar_etiqueta('p', Longitud).

% Imprime las etiquetas centradas para una oración
imprimir_etiqueta_centrada(s(NP, VP)) :- 
        imprimir_etiqueta_centrada(NP), write(' '), imprimir_etiqueta_centrada(VP).

imprimir_etiqueta_centrada(SUJ) :- 
        ( SUJ = np(Izq)
        ->  imprimir_etiqueta_centrada(Izq) % Nodo frase verbal
        ; functor(SUJ, np, Arity), Arity > 1, 
          arg(1, SUJ, Izq), % Obtener el primer hijo
          vp_sin_primero(SUJ, Resto), % Obtener los demás hijos
          imprimir_etiqueta_centrada(Izq), write(' '), imprimir_etiqueta_centrada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_etiqueta_centrada(PRED) :- 
        ( PRED = vp(Izq)
        ->  imprimir_etiqueta_centrada(Izq) % Nodo frase verbal
        ; functor(PRED, vp, Arity), Arity > 1, 
          arg(1, PRED, Izq), % Obtener el primer hijo
          vp_sin_primero(PRED, Resto), % Obtener los demás hijos
          imprimir_etiqueta_centrada(Izq), write(' '), imprimir_etiqueta_centrada(Resto) % Imprimir el primer hijo y luego los demás
        ).


% Centra la etiqueta debajo de los guiones
centrar_etiqueta(Etiqueta, Longitud) :-
        EspaciosIzq is (Longitud - 1) // 2, % Espacios a la izquierda
        EspaciosDer is Longitud - EspaciosIzq - 1, % Espacios a la derecha
        wspaces(EspaciosIzq), % Imprime los espacios a la izquierda
        write(Etiqueta), % Imprime la etiqueta
        wspaces(EspaciosDer). % Imprime los espacios a la derecha

imprimir_etiqueta_suj_y_pred(s(NP,VP)) :-
        % Imprimir la etiqueta de la frase nominal
        calcular_longitud_frase(NP, LongitudNP),
        centrar_etiqueta('S', LongitudNP),
        write(' '),
        % Imprimir la etiqueta de la frase verbal
        calcular_longitud_frase(VP, LongitudVP),
        centrar_etiqueta('P', LongitudVP).


% Subraya el complemento directo (np dentro de vp)
imprimir_complemento_directo_subrayado(s(Suj, vp(Verb, NP))) :-
        calcular_longitud_frase(Suj, Espacios),
        wspaces(Espacios + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(Verb, EspaciosVerb),
        wspaces(EspaciosVerb + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(NP, Longitud),
        dashes(Longitud, [], Guiones), % Genera los guiones
        prt(Guiones). % Imprime los guiones y un salto de línea

% Subraya el complemento directo (np dentro de vp)
imprimir_complemento_indirecto_subrayado(s(Suj, vp(Verb, PREP, NP))) :-
        calcular_longitud_frase(Suj, Espacios),
        wspaces(Espacios + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(Verb, EspaciosVerb),
        wspaces(EspaciosVerb + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(NP, LongitudNP),
        calcular_longitud_frase(PREP, LongitudPrep),
        Longitud is LongitudNP + LongitudPrep + 1, % Suma 1 por el espacio entre la preposición y el complemento
        dashes(Longitud, [], Guiones), % Genera los guiones
        prt(Guiones). % Imprime los guiones y un salto de línea

%Imprimir la etiqueta de los complementos
imprimir_etiqueta_de_complemento_directo(s(Suj, vp(Verb, NP))) :-
        calcular_longitud_frase(Suj, EspaciosSuj),
        calcular_longitud_frase(Verb, EspaciosVerb),
        calcular_longitud_frase(NP, Longitud),
        Espacios is EspaciosSuj + EspaciosVerb + 1, % Calcula la longitud total
        wspaces(Espacios), % Imprime los espacios necesarios
        % Imprime la etiqueta centrada debajo de los guiones
        centrar_etiqueta('CD', Longitud). % Imprime la etiqueta centrada debajo de los guiones

imprimir_etiqueta_de_complemento_indirecto(s(Suj, vp(Verb, PREP, NP))) :-
        calcular_longitud_frase(Suj, EspaciosSuj),
        calcular_longitud_frase(Verb, EspaciosVerb),
        calcular_longitud_frase(NP, LongitudNP),
        calcular_longitud_frase(PREP, LongitudPrep),
        Longitud is LongitudNP + LongitudPrep + 1, % Suma 1 por el espacio entre la preposición y el complemento
        Espacios is EspaciosSuj + EspaciosVerb + 1, % Calcula la longitud total
        wspaces(Espacios), % Imprime los espacios necesarios
        % Imprime la etiqueta centrada debajo de los guiones
        centrar_etiqueta('CI', Longitud). % Imprime la etiqueta centrada debajo de los guiones

%Imprime la etiqueta de los complementos seguidos, es decir, en case de que haya
% un complemento directo y un complemento indirecto
imprimir_complementos(s(NP, VP)) :-
        %Imprimir subrayado del complemento directo si hay
        (   VP = vp(Verb, NP2) ->
            imprimir_complemento_directo_subrayado(s(NP, vp(Verb, NP2)))
        ;   %Imprimir subrayado del complemento indirecto si hay
            VP = vp(Verb, PREP, NP2) ->
            imprimir_complemento_indirecto_subrayado(s(NP, vp(Verb, PREP, NP2)))
        ;   % Si no hay complementos, no hacer nada
            true
        ),
        nl,
        % Imprimir la etiqueta de complemento directo si hay
        (   VP = vp(Verb, NP2) ->
            imprimir_etiqueta_de_complemento_directo(s(NP, vp(Verb, NP2)))
        ;   % Imprimir la etiqueta de complemento indirecto si hay
            VP = vp(Verb, PREP, NP2) ->
            imprimir_etiqueta_de_complemento_indirecto(s(NP, vp(Verb, PREP, NP2)))
        ;   % Si no hay complementos, no hacer nada
            true
        ).