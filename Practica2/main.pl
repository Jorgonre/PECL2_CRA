:- module(main, [menu/0]).

% -----------------------------------------------------------------------------
% DEPENDENCIAS
% -----------------------------------------------------------------------------
:- use_module(infinitivo,          []).   % infinitivo/2, load_verbs/0
:- use_module(preprocesar,         []).   % preprocesar_es/2, preprocesar_en/2
:- use_module(flexion,             []).   % load_verb_db/1, conjugado/5, plural/singular helpers
:- use_module(traductor,           []).   % load_verb_db/1 (alias), traducir_frase_es_en/2, traducir_frase_en_es/2
:- use_module(metricas,            []).   % oracion grammar utils & draw
:- use_module(draw,               [draw/1]).
:- consult('prueba_2.pl').                 % gramática oracion/4, etc.

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

% 7) Analizar + Dibujar árbol --------------------------------------------------
process_choice(7) :-
    ask_sentence('Sentence in English', In),
    preprocesar:preprocesar_en(In, Toks),
    ( oracion(eng, Tree, Toks, [])
    -> writeln('Analisis (estructura Prolog):'), writeln(Tree),
       writeln('\n→ Arbol:'), draw:draw(Tree)
    ;  writeln('No se pudo analizar con la gramatica.') ),
    !,fail.

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
