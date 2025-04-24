:- module(draw_html, [draw_html/2]).
:- use_module(library(lists)).
:- use_module(library(apply)).

% IMPORTA el transformador de complementos de prueba_2.pl
:- use_module(prueba_2, [transformar_frase_con_complementos/2]).

% Para evitar “no together” en tree_leaves/3
:- discontiguous draw_html:tree_leaves/3.

%! draw_html(+RawTree, +BaseName:atom) is det.
%  Envuelve los complementos y genera el HTML.
draw_html(RawTree, Base) :-
    transformar_frase_con_complementos(RawTree, Tree),
    draw_html_transformed(Tree, Base).

%! draw_html_transformed(+Tree, +BaseName:atom) is det.
draw_html_transformed(Tree, Base) :-
    Tree =.. [_Conj, S, P],
    tree_leaves(S, W1, T1), length(W1,L1),
    tree_leaves(P, W2, T2), length(W2,L2),
    append(W1,W2,Words),   append(T1,T2,Tags),
    End is L1+L2-1,
    Cats = [ span(0,   L1-1, "SN / Sujeto"),
             span(L1, End,   "SV / Predicado") ],
    visualize(Words, Tags, Cats, L1, Base).

%! tree_leaves(+Tree, -Words, -Tags) is det.
tree_leaves(cd(Sub),  Words, Tags) :- wrap_tags(Sub,'CD',Words,Tags).
tree_leaves(ci(Sub),  Words, Tags) :- wrap_tags(Sub,'CI',Words,Tags).
tree_leaves(ccm(Sub), Words, Tags) :- wrap_tags(Sub,'CCM',Words,Tags).
tree_leaves(ccl(Sub), Words, Tags) :- wrap_tags(Sub,'CCL',Words,Tags).
tree_leaves(cct(Sub), Words, Tags) :- wrap_tags(Sub,'CCT',Words,Tags).

%! wrap_tags(+SubTree,+Wrap,-Words,-Tags) is det.
wrap_tags(Sub, Wrap, Words, Tags) :-
    tree_leaves(Sub, Words, _),
    ( Sub =.. [_Outer, Arg], Arg =.. [Inner|_]
    ; Sub =.. [Inner,_]
    ),
    atomic_list_concat([Inner,'/',Wrap],Comb),
    length(Words,N), length(Tags,N),
    maplist(=(Comb), Tags).

tree_leaves(Term, [W], ['-']) :-
    compound(Term), functor(Term,'-',2),
    arg(1,Term,L), atomic(L), arg(2,Term,R), atomic(R), !,
    atom_concat(L,'-',Temp), atom_concat(Temp,R,W).

tree_leaves(Term, [W1,W2], [F,F]) :-
    compound(Term),
    Term =.. [F, A1, A2],
    atomic(A1), atomic(A2),
    atom_string(A1, W1),
    atom_string(A2, W2).
    
tree_leaves(Term, [W], [Tag]) :-
    compound(Term), functor(Term,F,1),
    arg(1,Term,A), atomic(A), !,
    atom_string(A,W), atom_string(F,Tag).

tree_leaves(Term, Ws, Ts) :-
    Term =.. [_|Kids],
    maplist(tree_leaves, Kids, Wss, Tss),
    append(Wss,Ws), append(Tss,Ts).

%! visualize(+Words,+Tags,+Cats,+SubjLen,+Base) is det.
visualize(Words, Tags, Cats, SubjLen, Base) :-
    format(atom(File),"~w.html",[Base]),
    open(File,write,Out),
    % --- HEAD y ESTILOS básicos ---
    write(Out,"<!DOCTYPE html><html lang=\"es\"><head><meta charset=\"UTF-8\">\n"),
    write(Out,
"<style>
 table.tree{border-collapse:collapse;width:100%;}
 table.tree td{text-align:center;padding:4px;font-family:sans-serif;}
 tr.words td{background:#c2e7ff;font-weight:bold;}
 tr.words td.subj{background:#d1e7dd;}
 tr.words td.pred{background:#f8d7da;}
 tr.tags  td{background:#ffe599;}
 tr.cats  td{border-top:2px solid #333;}
 td.highlight { outline: 2px dashed orange; }
</style>\n"),
    % --- SCRIPT de interactividad ---
    write(Out,
"<script>
document.addEventListener('DOMContentLoaded', () => {

  // 1) Hover: resalta por data-tag
  document.querySelectorAll('td[data-tag]').forEach(td => {
    td.addEventListener('mouseover', () => {
      const tag = td.dataset.tag;
      document.querySelectorAll(`td[data-tag=\"${tag}\"]`)
              .forEach(x => x.classList.add('highlight'));
    });
    td.addEventListener('mouseout', () => {
      const tag = td.dataset.tag;
      document.querySelectorAll(`td[data-tag=\"${tag}\"]`)
              .forEach(x => x.classList.remove('highlight'));
    });
  });

  // 2) Click sobre palabras/etiquetas: mostrar rango de índices
  document.querySelectorAll('td[data-tag]').forEach(td => {
    td.addEventListener('click', () => {
      const tag = td.dataset.tag;
      // recogemos todos los índices
      const idxs = Array.from(document
        .querySelectorAll(`td[data-tag=\"${tag}\"]`))
        .map(c => parseInt(c.dataset.index));
      const min = Math.min(...idxs), max = Math.max(...idxs);
      alert(`Etiqueta “${tag}”: índices ${min}–${max}`);
    });
  });

  // 3) Click en SN/SV: como antes
  document.querySelectorAll('td.cats').forEach(td => {
    td.addEventListener('click', () => {
      const from = td.dataset.from, to = td.dataset.to, lbl = td.textContent;
      alert(`Rango ${from}–${to}: ${lbl}`);
    });
  });

});
</script>\n"),
    write(Out,"</head><body><table class=\"tree\">\n"),

    % --- FILA de PALABRAS ---
    write(Out,"<tr class=\"words\">"),
    forall(nth0(I,Words,W),
      ( nth0(I,Tags,Tag),
        Cls = (I<SubjLen->subj;pred),
        ( I =:= SubjLen-1
        -> format(Out,
            "<td class=\"~w\" data-index=\"~d\" data-tag=\"~w\" style=\"border-right:3px solid #333\">~w</td>",
            [Cls,I,Tag,W])
        ;  format(Out,
            "<td class=\"~w\" data-index=\"~d\" data-tag=\"~w\">~w</td>",
            [Cls,I,Tag,W])
        )
      )),
    write(Out,"</tr>\n"),

    % --- FILA de ETIQUETAS ---
    write(Out,"<tr class=\"tags\">"),
    forall(nth0(I,Tags,TTag),
      ( Cls = (I<SubjLen->subj;pred),
        ( I =:= SubjLen-1
        -> format(Out,
            "<td class=\"~w\" data-index=\"~d\" data-tag=\"~w\" style=\"border-right:3px solid #333\">~w</td>",
            [Cls,I,TTag,TTag])
        ;  format(Out,
            "<td class=\"~w\" data-index=\"~d\" data-tag=\"~w\">~w</td>",
            [Cls,I,TTag,TTag])
        )
      )),
    write(Out,"</tr>\n"),

    % --- FILA de SN / SV ---
    write(Out,"<tr class=\"cats\">"),
    forall(member(span(From,To,Lbl),Cats),
      ( Len is To-From+1,
        Cl2 = (From<SubjLen->subj;pred),
        format(Out,
          "<td class=\"cats ~w\" data-from=\"~d\" data-to=\"~d\" colspan=\"~w\">~w</td>",
          [Cl2,From,To,Len,Lbl])
      )),
    write(Out,"</tr></table></body></html>\n"),

    close(Out),
    format('HTML generado en "~w".~n',[File]).
