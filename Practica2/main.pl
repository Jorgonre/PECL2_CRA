:- module(main, [menu/0]).

:- ensure_loaded('traductorGoogle.pl').
:- use_module(prueba_2,  [oracion/4]).
:- use_module(draw,      [draw/1]).
:- use_module(draw_html, [draw_html/2]).
:- use_module(preprocesar, []).

:- dynamic current_lang/1.
:- dynamic current_phrase/1.

menu :-
    choose_phrase,
    menu_loop.

choose_phrase :-
    nl, writeln('?Como desea introducir la frase?'),
    writeln('  1) Manual'),
    writeln('  2) Desde frases.csv'),
    write('Opcion: '), read_choice(Op),
    ( Op =:= 1 ->
        prompt_language,
        ask_sentence('Frase', P)
    ; Op =:= 2 ->
        load_csv_phrases(Rows),
        select_csv_phrase(Rows, P),
        retractall(current_lang(_)),
        assertz(current_lang(en))
    ; writeln('Opcion invalida.'), choose_phrase
    ),
    retractall(current_phrase(_)),
    assertz(current_phrase(P)).

load_csv_phrases(Rows) :-
    csv_read_file('frases.csv', Rows,
        [ functor(frase)
        , arity(2)              % id;frase
        , separator(0';)
        , skip_header(true)     % descartamos la cabecera del fichero
        ]),
    % imprimimos nuestra propia cabecera
    writeln('id;frase'),
    forall(
      member(frase(Id,Atom), Rows),
      (
        atom_string(Atom, Txt),
        format('~w;~w~n', [Id, Txt])
      )
    ).

%% 2) Selecciona frase por Id
select_csv_phrase(Rows, P) :-
    write('Numero de frase: '),
    read_choice(Id2),
    member(frase(Id2, Atom2), Rows),
    atom_string(Atom2, P).

prompt_language :-
    writeln('?Idioma de la frase?'),
    writeln('  1) English'),
    writeln('  2) Espanol'),
    write('Opcion: '), read_choice(L),
    ( L =:= 1 -> Lang = en
    ; L =:= 2 -> Lang = es
    ; writeln('Opcion invalida.'), prompt_language
    ),
    retractall(current_lang(_)),
    assertz(current_lang(Lang)).

menu_loop :-
    repeat,
      current_lang(L), current_phrase(P),
      format('\nFrase actual (~w): ~s~n',[L,P]),
      findall(Label, menu_item(L,Label,_), Labels),
      findall(Code,  menu_item(L,_,Code),  Codes),
      print_menu_items(Labels,1),
      write('Seleccione opcion: '), read_choice(Idx),
      ( nth1(Idx,Codes,Code)
      -> ( Code == exit -> !
         ; perform(Code), fail
         )
      ; writeln('Opcion no valida.'), fail
      ).

menu_item(es, 'Preprocesar Espanol', pre_es).
menu_item(es, 'Traducir ES->EN',    trans_es_en).

menu_item(en, 'Preprocesar English', pre_en).
menu_item(en, 'Traducir EN->ES',     trans_en_es).
menu_item(en, 'Analisis sintactico', analysis).

menu_item(_, 'Cambiar frase', change).
menu_item(_, 'Salir',         exit).

print_menu_items([], _) :- !.
print_menu_items([L|Ls], N) :-
    format(' ~w) ~w~n',[N,L]),
    N1 is N+1,
    print_menu_items(Ls,N1).

perform(pre_es)      :- process_pre_es.
perform(trans_es_en) :- process_trans_es_en.
perform(pre_en)      :- process_pre_en.
perform(trans_en_es) :- process_trans_en_es.
perform(analysis)    :- do_analysis.
perform(change)      :- choose_phrase.
perform(exit).

process_pre_es :-
    current_phrase(P),
    preprocesar:preprocesar_es(P, Ts),
    format('Tokens ES: ~w~n',[Ts]).
process_pre_en :-
    current_phrase(P),
    preprocesar:preprocesar_en(P, Ts),
    format('Tokens EN: ~w~n',[Ts]).

process_trans_es_en :-
    current_phrase(P),
    translate_free(P, es, en, Out),
    format('ES->EN: ~s~n',[Out]).
process_trans_en_es :-
    current_phrase(P),
    translate_free(P, en, es, Out),
    format('EN->ES: ~s~n',[Out]).

do_analysis :-
    writeln('Introduce la lista de tokens (ej. [el, gato, duerme]):'),
    read(Toks),
    ( prueba_2:oracion(eng, Raw0, Toks, []) ->
        ( Raw0 = [F|_] -> Flat = F ; Flat = Raw0 ),
        normalise_trees(Raw0, Trees0),
        close_list(Trees0, Trees),
        analysis_menu(Trees)
    ; writeln('No se pudo analizar.')
    ).

%% ----------------------------
%% Sub-menú para los árboles analizados
%% ----------------------------
analysis_menu(Trees) :-
    repeat,
      nl, writeln('------ ANALISIS ------'),
      writeln(' 1) Mostrar arbol ASCII'),
      writeln(' 2) Generar HTML subrayado'),
      writeln(' 3) Mostrar estructuras Prolog'),     
      writeln(' 0) Volver'),
      write('Opcion: '), read_choice(Op),
      ( Op =:= 0 -> !
      ; run_analysis_option(Op, Trees), fail
      ).

run_analysis_option(1, Trees) :-
    forall(member(T, Trees),
      ( nl, 
      Trees = [T],
      prueba_2:unir_oraciones(T, U),
      draw:draw(U), nl )
    ).
run_analysis_option(2, Trees) :-
    forall(nth1(I, Trees, T),
      ( format('Generando subrayado_~w.html...~n',[I]),
        format(atom(Base),'subrayado_~w',[I]),
        draw_html:draw_html(T, Base)
      )
    ).
run_analysis_option(3, Trees) :-
    writeln(Trees),
    format('Estructura Prolog resultante:~n~n'),
    forall(member(Tree, Trees),
        ( portray_clause(Tree), nl )
    ).

run_analysis_option(_, _) :-
    writeln('Opcion no valida.').

read_choice(N) :-
    read_line_to_string(user_input, S),
    ( number_string(N, S) -> true
    ; writeln('Entrada invalida, ingrese un numero.'), read_choice(N)
    ).

ask_sentence(Prompt, S) :-
    format('~w:~n> ', [Prompt]),
    read_line_to_string(user_input, S0),
    normalize_space(string(S), S0).

normalise_trees(Raw, List) :-
    ( Raw = [|] -> List = Raw ; List = [Raw] ).

close_list(List, Closed) :-
    is_list(List), !,
    append(List, [], Closed).
close_list(Other, [Other]).  % por si acaso no era lista