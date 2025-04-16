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
                        Din \= [] % Asegúrate de que no sea el principio de la lista
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



%PRUEBA imprimir_frase_subrayada(o(gn(det(el), n(perro)), gv(v(corre), prep(a), gn(det(el), n(parque))))).
%PRUEBA draw(o(gn(det(el), n(perro)), gv(v(corre), prep(a), gn(det(el), n(parque))))).

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
        Arbol = o(GN, GV),
        calcular_longitud_frase(GN, LongitudGN),
        calcular_longitud_frase(GV, LongitudGV),
        % Subrayar la frase nominal con guiones
        dashes(LongitudGN, [], GuionesGN),
        prt(GuionesGN),
        write(' '),
        % Subrayar la frase verbal con guiones
        dashes(LongitudGV, [], GuionesGV),
        prt(GuionesGV), nl,
        % Imprimir la etiqueta del sujeto y del predicado
        imprimir_etiqueta_suj_y_pred(Arbol), nl.

% Imprime las hojas de un árbol como una oración
imprimir_frase(det(Palabra)) :- 
        write(Palabra). % Nodo determinante

imprimir_frase(n_p(Palabra)) :- 
        write(Palabra). % Nodo nombre propio

imprimir_frase(n(Palabra)) :- 
        write(Palabra). % Nodo sustantivo

imprimir_frase(v(Palabra)) :- 
        write(Palabra). % Nodo verbo

imprimir_frase(prep(Palabra)) :- 
        write(Palabra). % Nodo verbo

imprimir_frase(adj(Palabra)) :- 
        write(Palabra). % Nodo adjetivo

imprimir_frase(adv(Palabra)) :- 
        write(Palabra). % Nodo adverbio

% Devuelve un nuevo término gv con todos los hijos menos el primero
gv_sin_primero(Term, NuevoTerm) :-
        functor(Term, Nombre, Arity), % Obtiene el nombre y la aridad del término original
        Arity > 1, % Asegúrate de que haya mas de un hijo
        ArityMenosUno is Arity - 1, % Calcula la nueva aridad
        functor(NuevoTerm, Nombre, ArityMenosUno), % Crea un nuevo término con aridad reducida
        copiar_hijos_desde(Term, NuevoTerm, 2, Arity, 1). % Copia los hijos desde el segundo

% Devuelve un nuevo término gn con todos los hijos menos el primero
gn_sin_primero(Term, NuevoTerm) :-
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

imprimir_frase(o(GN, GV)) :- 
        imprimir_frase(GN), write(' '), imprimir_frase(GV). % Nodo oración

imprimir_frase(ADJ) :-
        ( ADJ = gadj(Izq)
        ->  imprimir_frase(Izq) % Nodo adjetivo
        ; functor(ADJ, gadj, Arity), Arity > 1, 
          arg(1, ADJ, Izq), % Obtener el primer hijo
          gv_sin_primero(ADJ, Resto), % Obtener los demás hijos
          imprimir_frase(Izq), write(' '), imprimir_frase(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_frase(ADV) :-
        ( ADV = gadv(Izq)
        ->  imprimir_frase(Izq) % Nodo adverbio
        ; functor(ADV, gadv, Arity), Arity > 1, 
          arg(1, ADV, Izq), % Obtener el primer hijo
          gv_sin_primero(ADV, Resto), % Obtener los demás hijos
          imprimir_frase(Izq), write(' '), imprimir_frase(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_frase(SUJ) :- 
        ( SUJ = gn(Izq)
        ->  imprimir_frase(Izq) % Nodo frase verbal
        ; functor(SUJ, gn, Arity), Arity > 1, 
          arg(1, SUJ, Izq), % Obtener el primer hijo
          gn_sin_primero(SUJ, Resto), % Obtener los demás hijos
          imprimir_frase(Izq), write(' '), imprimir_frase(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_frase(PRED) :- 
        ( PRED = gv(Izq)
        ->  imprimir_frase(Izq) % Nodo frase verbal
        ; functor(PRED, gv, Arity), Arity > 1, 
          arg(1, PRED, Izq), % Obtener el primer hijo
          gv_sin_primero(PRED, Resto), % Obtener los demás hijos
          imprimir_frase(Izq), write(' '), imprimir_frase(Resto) % Imprimir el primer hijo y luego los demás
        ).

% Calcula la longitud total de la frase generada por el árbol
calcular_longitud_frase(det(Palabra), Longitud) :-
        name(Palabra, Lista), % Convierte el átomo en una lista de caracteres
        length(Lista, Longitud).

calcular_longitud_frase(n_p(Palabra), Longitud) :-
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

calcular_longitud_frase(adj(Palabra), Longitud) :-
        name(Palabra, Lista),
        length(Lista, Longitud).

calcular_longitud_frase(adv(Palabra), Longitud) :-
        name(Palabra, Lista),
        length(Lista, Longitud).

calcular_longitud_frase(o(GN, GV), Longitud) :-
        calcular_longitud_frase(GN, LongGN),
        calcular_longitud_frase(GV, LongGV),
        Longitud is LongGN + LongGV + 1. % Suma 1 por el espacio entre sujeto y predicado

calcular_longitud_frase(ADJ, Longitud) :-
        (   ADJ = gadj(Izq) % Si solo hay un hijo
        ->      calcular_longitud_frase(Izq, Longitud) % Nodo adjetivo
        ;   functor(ADJ, gadj, Arity), Arity > 1 ->
                arg(1, ADJ, Izq), % Obtener el primer hijo
                gv_sin_primero(ADJ, Resto), % Obtener los demás hijos
                calcular_longitud_frase(Izq, LongIzq),
                calcular_longitud_frase(Resto, LongResto),
                Longitud is LongIzq + LongResto + 1 % Suma 1 por el espacio entre palabras
        ).

calcular_longitud_frase(ADV, Longitud) :-
        (   ADV = gadv(Izq) % Si solo hay un hijo
        ->      calcular_longitud_frase(Izq, Longitud) % Nodo adverbio
        ;   functor(ADV, gadv, Arity), Arity > 1 ->
                arg(1, ADV, Izq), % Obtener el primer hijo
                gv_sin_primero(ADV, Resto), % Obtener los demás hijos
                calcular_longitud_frase(Izq, LongIzq),
                calcular_longitud_frase(Resto, LongResto),
                Longitud is LongIzq + LongResto + 1 % Suma 1 por el espacio entre palabras
        ).

calcular_longitud_frase(SUJ, Longitud) :-
        (   SUJ = gn(Izq) % Si solo hay un hijo
        ->      calcular_longitud_frase(Izq, Longitud) % Nodo frase nominal
        ;   functor(SUJ, gn, Arity), Arity > 1 ->
                arg(1, SUJ, Izq), % Obtener el primer hijo
                gv_sin_primero(SUJ, Resto), % Obtener los demás hijos
                calcular_longitud_frase(Izq, LongIzq),
                calcular_longitud_frase(Resto, LongResto),
                Longitud is LongIzq + LongResto + 1 % Suma 1 por el espacio entre palabras
        ).

calcular_longitud_frase(PRED, Longitud) :-
        (   PRED = gv(Izq) % Si solo hay un hijo
        ->      calcular_longitud_frase(Izq, Longitud) % Nodo frase verbal
        ;   functor(PRED, gv, Arity), Arity > 1 ->
                arg(1, PRED, Izq), % Obtener el primer hijo
                gv_sin_primero(PRED, Resto), % Obtener los demás hijos
                calcular_longitud_frase(Izq, LongIzq),
                calcular_longitud_frase(Resto, LongResto),
                Longitud is LongIzq + LongResto + 1 % Suma 1 por el espacio entre palabras
        ).

% Subraya cada una de las hojas de un árbol

imprimir_palabra_subrayada(det(Palabra)) :- 
        calcular_longitud_frase(det(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo determinante

imprimir_palabra_subrayada(n_p(Palabra)) :- 
        calcular_longitud_frase(n_p(Palabra), Longitud),
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

imprimir_palabra_subrayada(adj(Palabra)) :- 
        calcular_longitud_frase(adj(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo adjetivo

imprimir_palabra_subrayada(adv(Palabra)) :- 
        calcular_longitud_frase(adv(Palabra), Longitud),
        dashes(Longitud, [], Guiones),
        prt(Guiones). % Nodo adverbio

imprimir_palabra_subrayada(o(GN, GV)) :- 
        imprimir_palabra_subrayada(GN), write(' '), imprimir_palabra_subrayada(GV). % Nodo oración

imprimir_palabra_subrayada(ADJ) :-
        ( ADJ = gadj(Izq)
        ->  imprimir_palabra_subrayada(Izq) % Nodo adjetivo
        ; functor(ADJ, gadj, Arity), Arity > 1, 
          arg(1, ADJ, Izq), % Obtener el primer hijo
          gv_sin_primero(ADJ, Resto), % Obtener los demás hijos
          imprimir_palabra_subrayada(Izq), write(' '), imprimir_palabra_subrayada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_palabra_subrayada(ADV) :-
        ( ADV = gadv(Izq)
        ->  imprimir_palabra_subrayada(Izq) % Nodo adverbio
        ; functor(ADV, gadv, Arity), Arity > 1, 
          arg(1, ADV, Izq), % Obtener el primer hijo
          gv_sin_primero(ADV, Resto), % Obtener los demás hijos
          imprimir_palabra_subrayada(Izq), write(' '), imprimir_palabra_subrayada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_palabra_subrayada(SUJ) :- 
        ( SUJ = gn(Izq)
        ->  imprimir_palabra_subrayada(Izq) % Nodo frase nominal
        ; functor(SUJ, gn, Arity), Arity > 1, 
          arg(1, SUJ, Izq), % Obtener el primer hijo
          gv_sin_primero(SUJ, Resto), % Obtener los demás hijos
          imprimir_palabra_subrayada(Izq), write(' '), imprimir_palabra_subrayada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_palabra_subrayada(PRED) :- 
        ( PRED = gv(Izq)
        ->  imprimir_palabra_subrayada(Izq) % Nodo frase verbal
        ; functor(PRED, gv, Arity), Arity > 1, 
          arg(1, PRED, Izq), % Obtener el primer hijo
          gv_sin_primero(PRED, Resto), % Obtener los demás hijos
          imprimir_palabra_subrayada(Izq), write(' '), imprimir_palabra_subrayada(Resto) % Imprimir el primer hijo y luego los demás
        ).


% Imprime solo la etiqueta centrada debajo de los guiones para un determinante
imprimir_etiqueta_centrada(det(Palabra)) :- 
        calcular_longitud_frase(det(Palabra), Longitud),
        centrar_etiqueta('d', Longitud).

imprimir_etiqueta_centrada(n_p(Palabra)) :- 
        calcular_longitud_frase(n_p(Palabra), Longitud),
        centrar_etiqueta('np', Longitud).

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

imprimir_etiqueta_centrada(adj(Palabra)) :- 
        calcular_longitud_frase(adj(Palabra), Longitud),
        centrar_etiqueta('adj', Longitud).

imprimir_etiqueta_centrada(adv(Palabra)) :- 
        calcular_longitud_frase(adv(Palabra), Longitud),
        centrar_etiqueta('adv', Longitud).

% Imprime las etiquetas centradas para una oración
imprimir_etiqueta_centrada(o(GN, GV)) :- 
        imprimir_etiqueta_centrada(GN), write(' '), imprimir_etiqueta_centrada(GV).

imprimir_etiqueta_centrada(ADJ) :-
        ( ADJ = gadj(Izq)
        ->  imprimir_etiqueta_centrada(Izq) % Nodo adjetivo
        ; functor(ADJ, gadj, Arity), Arity > 1, 
          arg(1, ADJ, Izq), % Obtener el primer hijo
          gv_sin_primero(ADJ, Resto), % Obtener los demás hijos
          imprimir_etiqueta_centrada(Izq), write(' '), imprimir_etiqueta_centrada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_etiqueta_centrada(ADV) :-
        ( ADV = gadv(Izq)
        ->  imprimir_etiqueta_centrada(Izq) % Nodo adverbio
        ; functor(ADV, gadv, Arity), Arity > 1, 
          arg(1, ADV, Izq), % Obtener el primer hijo
          gv_sin_primero(ADV, Resto), % Obtener los demás hijos
          imprimir_etiqueta_centrada(Izq), write(' '), imprimir_etiqueta_centrada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_etiqueta_centrada(SUJ) :- 
        ( SUJ = gn(Izq)
        ->  imprimir_etiqueta_centrada(Izq) % Nodo frase verbal
        ; functor(SUJ, gn, Arity), Arity > 1, 
          arg(1, SUJ, Izq), % Obtener el primer hijo
          gv_sin_primero(SUJ, Resto), % Obtener los demás hijos
          imprimir_etiqueta_centrada(Izq), imprimir_etiqueta_centrada(Resto) % Imprimir el primer hijo y luego los demás
        ).

imprimir_etiqueta_centrada(PRED) :- 
        ( PRED = gv(Izq)
        ->  imprimir_etiqueta_centrada(Izq) % Nodo frase verbal
        ; functor(PRED, gv, Arity), Arity > 1, 
          arg(1, PRED, Izq), % Obtener el primer hijo
          gv_sin_primero(PRED, Resto), % Obtener los demás hijos
          imprimir_etiqueta_centrada(Izq), imprimir_etiqueta_centrada(Resto) % Imprimir el primer hijo y luego los demás
        ).


% Centra la etiqueta debajo de los guiones
centrar_etiqueta(Etiqueta, Longitud) :-
        EspaciosIzq is (Longitud - 1) // 2, % Espacios a la izquierda
        EspaciosDer is Longitud - EspaciosIzq - 1, % Espacios a la derecha
        wspaces(EspaciosIzq), % Imprime los espacios a la izquierda
        write(Etiqueta), % Imprime la etiqueta
        wspaces(EspaciosDer). % Imprime los espacios a la derecha

imprimir_etiqueta_suj_y_pred(o(GN,GV)) :-
        % Imprimir la etiqueta de la frase nominal
        calcular_longitud_frase(GN, LongitudGN),
        centrar_etiqueta('S', LongitudGN),
        write(' '),
        % Imprimir la etiqueta de la frase verbal
        calcular_longitud_frase(GV, LongitudGV),
        centrar_etiqueta('P', LongitudGV).


% Subraya el complemento directo (gn dentro de gv)
imprimir_complemento_directo_subrayado(o(Suj, gv(Verb, GN))) :-
        calcular_longitud_frase(Suj, Espacios),
        wspaces(Espacios + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(Verb, EspaciosVerb),
        wspaces(EspaciosVerb + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(GN, Longitud),
        dashes(Longitud, [], Guiones), % Genera los guiones
        prt(Guiones). % Imprime los guiones y un salto de línea

% Subraya el complemento directo (gn dentro de gv)
imprimir_complemento_indirecto_subrayado(o(Suj, gv(Verb, PREP, GN))) :-
        calcular_longitud_frase(Suj, Espacios),
        wspaces(Espacios + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(Verb, EspaciosVerb),
        wspaces(EspaciosVerb + 1), % Imprime los espacios necesarios
        calcular_longitud_frase(GN, LongitudGN),
        calcular_longitud_frase(PREP, LongitudPrep),
        Longitud is LongitudGN + LongitudPrep + 1, % Suma 1 por el espacio entre la preposición y el complemento
        dashes(Longitud, [], Guiones), % Genera los guiones
        prt(Guiones). % Imprime los guiones y un salto de línea

%Imprimir la etiqueta de los complementos
imprimir_etiqueta_de_complemento_directo(o(Suj, gv(Verb, GN))) :-
        calcular_longitud_frase(Suj, EspaciosSuj),
        calcular_longitud_frase(Verb, EspaciosVerb),
        calcular_longitud_frase(GN, Longitud),
        Espacios is EspaciosSuj + EspaciosVerb + 1, % Calcula la longitud total
        wspaces(Espacios), % Imprime los espacios necesarios
        % Imprime la etiqueta centrada debajo de los guiones
        centrar_etiqueta('CD', Longitud). % Imprime la etiqueta centrada debajo de los guiones

imprimir_etiqueta_de_complemento_indirecto(o(Suj, gv(Verb, PREP, GN))) :-
        calcular_longitud_frase(Suj, EspaciosSuj),
        calcular_longitud_frase(Verb, EspaciosVerb),
        calcular_longitud_frase(GN, LongitudGN),
        calcular_longitud_frase(PREP, LongitudPrep),
        Longitud is LongitudGN + LongitudPrep + 1, % Suma 1 por el espacio entre la preposición y el complemento
        Espacios is EspaciosSuj + EspaciosVerb + 1, % Calcula la longitud total
        wspaces(Espacios), % Imprime los espacios necesarios
        % Imprime la etiqueta centrada debajo de los guiones
        centrar_etiqueta('CI', Longitud). % Imprime la etiqueta centrada debajo de los guiones

%Imprime la etiqueta de los complementos seguidos, es decir, en case de que haya
% un complemento directo y un complemento indirecto
imprimir_complementos(o(GN, GV)) :-
        %Imprimir subrayado del complemento directo si hay
        (   GV = gv(Verb, GN2) ->
            imprimir_complemento_directo_subrayado(o(GN, gv(Verb, GN2)))
        ;   %Imprimir subrayado del complemento indirecto si hay
            GV = gv(Verb, PREP, GN2) ->
            imprimir_complemento_indirecto_subrayado(o(GN, gv(Verb, PREP, GN2)))
        ;   % Si no hay complementos, no hacer nada
            true
        ),
        nl,
        % Imprimir la etiqueta de complemento directo si hay
        (   GV = gv(Verb, GN2) ->
            imprimir_etiqueta_de_complemento_directo(o(GN, gv(Verb, GN2)))
        ;   % Imprimir la etiqueta de complemento indirecto si hay
            GV = gv(Verb, PREP, GN2) ->
            imprimir_etiqueta_de_complemento_indirecto(o(GN, gv(Verb, PREP, GN2)))
        ;   % Si no hay complementos, no hacer nada
            true
        ).