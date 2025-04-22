:- module(main, [ menu/0 ]).

:- use_module(prueba_2, [oracion/4]).        % ← explicitly import your DCG
:- use_module(draw,    [draw/1, imprimir_frase_subrayada/1]).
:- use_module(infinitivo,          []).   % (these you always call module‑qualified)
:- use_module(preprocesar,         []).
:- use_module(flexion,             []).
:- use_module(traductor,           []).
:- use_module(draw_html, [draw_html/2]).      % para generar HTML con el subrayado




% --------------------------------------------------------------
%   LISTAS “ABIERTAS”  →  LISTAS NORMALES
% --------------------------------------------------------------
% close_list(+QuizáListaAbierta, -ListaCerrada)
close_list(Var,              []) :- var(Var), !.
close_list([],               []).
close_list([H|T], [H|Rest]) :- close_list(T, Rest).

% normalise_trees(+Raw, -ListaArboles)
%   • Raw ya es lista  -> la cierra por la cola.
%   • Raw es un árbol  -> lo mete en lista unitaria.
normalise_trees(Raw, List) :-
    (   Raw = [_|_] -> close_list(Raw, List)
    ;   List = [Raw]
    ).

% -------------------------------------------------------------------
%  Sub‑menú para los árboles analizados
% -------------------------------------------------------------------
analysis_menu(Trees) :-
    repeat,
        nl, writeln('======= ANALISIS ======='),
        writeln(' 1) Dibujar arbol'),
        writeln(' 2) Imprimir analisis subrayado'),
        writeln(' 0) Volver al menu principal'), nl,
        prompt_choice(Opt),
        (   Opt == 0
        ->  !                                  %  salir del repeat
        ;   run_analysis_option(Opt, Trees),
            fail                               %  volver a mostrar sub‑menú
        ).

run_analysis_option(1, Trees) :-               % dibujar
    forall(member(T, Trees),
          ( nl, draw:draw(T), nl )).
          
run_analysis_option(2, Trees) :-
    forall(
        nth1(I, Trees, Tree),
        (
            format('Generando árbol subrayado #~w…~n', [I]),
            format(atom(Base), 'analisis_subrayado_~w', [I]),
            draw_html:draw_html(Tree, Base)
        )
    ).


run_analysis_option(_, _) :-
    writeln('Opcion no reconocida, intente de nuevo.').


% -----------------------------------------------------------------------------
% PÚBLICO: menu/0 –  bucle interactivo
% -----------------------------------------------------------------------------
menu :-
    repeat,
        nl, writeln('===========  MENU PRINCIPAL  ==========='),
        writeln(' 1) Preprocesar frase en  ESPANOL'),
        writeln(' 2) Preprocesar frase en  INGLES'),
        writeln(' 3) Plurales & singulares   (ES)'),
        writeln(' 4) Plurals & singulars     (EN)'),
        writeln(' 5) Traducir frase ES -> EN'),
        writeln(' 6) Traducir frase EN -> ES'),
        writeln(' 7) Analizar & dibujar arbol de oracion inglesa'),
        writeln(' 0) Salir'), nl,
        prompt_choice(Choice),
        process_choice(Choice),
    Choice == 0, !.

% -----------------------------------------------------------------------------
% Entrada de la opción (entero)
% -----------------------------------------------------------------------------
prompt_choice(N) :-
    write('Seleccione opcion: '),
    read_line_to_string(user_input,S),
    catch(number_string(N,S),_,(writeln('Introduzca un numero valido.'), fail)).

% -----------------------------------------------------------------------------
% Procesamiento de cada opción
% -----------------------------------------------------------------------------
process_choice(0) :- writeln('Hasta la proxima!').

% 1) Preprocesar frase ES -------------------------------------------------------
process_choice(1) :-
    ask_sentence('Frase en espanol', In),
    preprocesar:preprocesar_es(In, Toks),
    format('Tokens: ~w~n', [Toks]), fail,
    !, fail.

% 2) Preprocesar frase EN -------------------------------------------------------
process_choice(2) :-
    ask_sentence('Sentence in English', In),
    preprocesar:preprocesar_en(In, Toks),
    format('Tokens: ~w~n', [Toks]), fail,
    !, fail.

% 3) Plural/Singular ES --------------------------------------------------------
process_choice(3) :-
    ask_atom('Palabra espanola', Word),
    flexion:pluralize(Word, Pl), flexion:singularize(Pl, Sing),
    format('Plural: ~w  |  Singular: ~w~n',[Pl,Sing]),
    !, fail.

% 4) Plural/Singular EN --------------------------------------------------------
process_choice(4) :-
    ask_atom('English word', Word),
    flexion:pluralize_en(Word, Pl), flexion:singularize_en(Pl, Sing),
    format('Plural: ~w  |  Singular: ~w~n',[Pl,Sing]),
    !, fail.

% 5) ES ➜ EN -------------------------------------------------------------------
process_choice(5) :-                       % ES ➜ EN
    ask_sentence('Frase en español', In),
    traductor:traducir_frase_es_en(In, Out),
    format('Traducción: ~s~n', [Out]),
    !,                                    % ← corta cualquier retroceso
    fail.                                 % ← fuerza volver al menú

% 6) EN ➜ ES ------------------------------------------------------------------
process_choice(6) :-
    ask_sentence('Sentence in English', In),
    traductor:traducir_frase_en_es(In, Out),
    format('Traduccion: ~s~n', [Out]), fail,
    !, fail.

% 7) Analizar + Dibujar árbol ---------------------------------------
process_choice(7) :-
    ask_sentence('Sentence in English', In),
    preprocesar:preprocesar_en(In, Toks),
    (   prueba_2:oracion(eng, Raw0, Toks, [])  % <-- llamada al DCG
    ->  (   Raw0 = [First|_] ,
            is_list(First)
        ->  Flat = First
        ;   Flat = Raw0
        ),
        normalise_trees(Flat, Trees),
        analysis_menu(Trees)
    ;   writeln('No se pudo analizar con la gramatica.')
    ),
    !, fail.

% -----------------------------------------------------------------------------
% UTILIDADES de entrada de texto/átomos
% -----------------------------------------------------------------------------
ask_sentence(Prompt, Sentence) :-
    format('~w:~n> ', [Prompt]),
    read_line_to_string(user_input, SentenceRaw),
    normalize_space(string(Sentence), SentenceRaw).

ask_string(Prompt, Str) :-
    format('~w: ', [Prompt]),
    read_line_to_string(user_input, Str).

ask_atom(Prompt, Atom) :-
    ask_string(Prompt, Str), atom_string(Atom, Str).
