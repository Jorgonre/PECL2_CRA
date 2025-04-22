/* draw_html.pl  ----------------------------------------------------------
   Prolog → HTML table generator para “gramática escolar”
   Integración con draw.pl para producir tabla estilizada

   Uso:
     ?- [draw].
     ?- [draw_html].
     ?- draw_html(
            and(
              o(g_nom_prop(n_p(jose)),   gv(v(is),  gadj(adj(dark-skinned)))),
              o(g_nom_prop(n_p(mary)),   gv(v(is),  gadj(adj(tall))))
            ),
            tree).
   Generará tree.html en el directorio actual.
--------------------------------------------------------------------------*/
:- module(draw_html, [draw_html/2]).
:- use_module(library(lists)).
:- use_module(library(apply)).

:- use_module(draw, [draw/1]).    % tu draw.pl original

%! tree_leaves(+Tree, -Words:list, -Tags:list) is det.
%  Extrae las hojas (palabras) y sus etiquetas léxicas de un árbol draw.pl.

% --- Caso especial: functor '-'/2 → token compuesto con guión
tree_leaves(Term, [W], [Tag]) :-
    compound(Term),
    functor(Term, '-', 2),
    arg(1, Term, L), atomic(L),
    arg(2, Term, R), atomic(R), !,
    % concatenar L-R en un solo átomo
    atom_concat(L, '-', Temp),
    atom_concat(Temp, R, Atom),
    atom_string(Atom, W),
    % la etiqueta la tomamos del functor '-'
    atom_string('-', Tag).

% Caso genérico de hoja: functor F/1 con argumento atómico
tree_leaves(Term, [W], [Tag]) :-
    compound(Term), functor(Term, F, 1),
    arg(1, Term, A), atomic(A), !,
    atom_string(A, W),
    atom_string(F, Tag).

% Recursión para nodos internos
tree_leaves(Term, Ws, Ts) :-
    Term =.. [_|Kids],
    maplist(tree_leaves, Kids, Wss, Tss),
    append(Wss, Ws),
    append(Tss, Ts).

%! draw_html(+Tree, +BaseName:atom) is det.
%  Genera BaseName.html con:
%    • fila de palabras
%    • fila de etiquetas léxicas
%    • fila de spans SN / Sujeto y SV / Predicado
draw_html(Tree, Base) :-
    % extraer los dos subárboles (Sujeto, Predicado) de cualquier functor binario
    Tree =.. [_Conj, S, P],
    % hojas del sujeto
    tree_leaves(S, W1, T1), length(W1, L1),
    % hojas del predicado
    tree_leaves(P, W2, T2), length(W2, L2),
    append(W1, W2, Words),
    append(T1, T2, Tags),
    End is L1 + L2 - 1,
    Cats = [
      span(0,    L1-1, "SN / Sujeto"),
      span(L1,  End,   "SV / Predicado")
    ],
    visualize(Words, Tags, Cats, Base).

%! visualize(+Words, +Tags, +Cats, +Base) is det.
%  Construye y escribe el HTML en Base.html.
visualize(Words, Tags, Cats, Base) :-
    format(atom(File), "~w.html", [Base]),
    open(File, write, Out),
    write(Out, "<!DOCTYPE html>\n<html lang=\"es\">\n<head>\n<meta charset=\"UTF-8\">\n"),
    write(Out,
"<style>
 table.tree {border-collapse:collapse;width:100%;}
 table.tree td   {text-align:center;padding:4px;font-family:sans-serif;}
 tr.words td     {background:#c2e7ff;}
 tr.tags  td     {background:#ffe599;}
 tr.cats  td     {border-top:2px solid #333;}
</style>\n</head>\n<body>\n<table class=\"tree\">\n"),
    % fila de palabras
    write(Out, "<tr class=\"words\">"),
    forall(member(W, Words),
           format(Out, "<td>~w</td>", [W])),
    write(Out, "</tr>\n"),
    % fila de etiquetas
    write(Out, "<tr class=\"tags\">"),
    forall(member(T, Tags),
           format(Out, "<td>~w</td>", [T])),
    write(Out, "</tr>\n"),
    % fila de categorías
    write(Out, "<tr class=\"cats\">"),
    forall(member(span(From,To,Lbl), Cats),
           ( Len is To - From + 1,
             format(Out, "<td colspan=\"~w\">~w</td>", [Len,Lbl]) )),
    write(Out, "</tr>\n"),
    write(Out, "</table>\n</body>\n</html>\n"),
    close(Out),
    format('HTML generado en "~w". Ábrelo en tu navegador.~n', [File]).
