%------------------------------------------------------------------------------%
% Módulo principal con menú interactivo
% Exporta el predicado menu/0 para iniciar la aplicación.
%------------------------------------------------------------------------------%
:- module(main, [menu/0]).

%------------------------------------------------------------------------------%
% Carga de dependencias externas
% - traductorGoogle.pl: interfaz de traducción libre
% - prueba_2: analizador sintáctico oracion/4
% - draw: dibuja árboles en ASCII
% - draw_html: genera visualizaciones HTML subrayadas
% - preprocesar: preprocesado de texto (ES/EN)
%------------------------------------------------------------------------------%
:- ensure_loaded('traductorGoogle.pl').
:- use_module(prueba_2,  [oracion/4]).
:- use_module(draw,      [draw/1]).
:- use_module(draw_html, [draw_html/2]).
:- use_module(preprocesar, []).

%------------------------------------------------------------------------------%
% Predicados dinámicos (estado global)
% - current_lang/1: idioma actual de la frase (en o es)
% - current_phrase/1: frase actual (string)
%------------------------------------------------------------------------------%
:- dynamic current_lang/1.
:- dynamic current_phrase/1.

%------------------------------------------------------------------------------%
% menu/0
% Punto de entrada: solicita la frase y arranca el bucle de menú.
%------------------------------------------------------------------------------%
menu :-
    choose_phrase,  % selecciona frase e idioma
    menu_loop.      % entra en el menú principal

%------------------------------------------------------------------------------%
% choose_phrase/0
% Pregunta si el usuario quiere:
%   1) introducir la frase manualmente
%   2) seleccionar una frase de frases.csv
% Luego almacena el idioma y la frase elegida.
%------------------------------------------------------------------------------%
choose_phrase :-
    nl, writeln('?Como desea introducir la frase?'),
    writeln('  1) Manual'),
    writeln('  2) Desde frases.csv'),
    write('Opcion: '), read_choice(Op),
    (   Op =:= 1
    ->  prompt_language,           % pide idioma
        ask_sentence('Frase', P)   % lee frase manual
    ;   Op =:= 2
    ->  load_csv_phrases(Rows),    % carga y muestra CSV
        select_csv_phrase(Rows, P),% selecciona frase por ID
        retractall(current_lang(_)),% resetea idioma
        assertz(current_lang(en))  % fija 'en' al cargar CSV
    ;   writeln('Opcion invalida.'), choose_phrase
    ),
    retractall(current_phrase(_)),  % resetea frase previa
    assertz(current_phrase(P)).     % guarda la nueva frase

%------------------------------------------------------------------------------%
% load_csv_phrases(-Rows)
% Lee el CSV 'frases.csv' como lista de términos:
%   Rows = [frase(_,Id,Atom), ...]
% Luego muestra cada Id y texto al usuario.
%------------------------------------------------------------------------------%
load_csv_phrases(Rows) :-
    csv_read_file('frases.csv', Rows,
                  [ functor(frase), arity(3), separator(0';) ]),
    forall(
      member(frase(_,Id,Atom), Rows),
      ( atom_string(Atom, Txt),           % átomo → string
        format('~w) ~w~n', [Id,Txt])     % imprime "Id) Texto"
      )
    ).

%------------------------------------------------------------------------------%
% select_csv_phrase(+Rows, -Phrase)
% Solicita al usuario el ID de la frase y extrae el texto correspondiente.
%------------------------------------------------------------------------------%
select_csv_phrase(Rows, P) :-
    write('Numero de frase: '), read_choice(Id2),
    member(frase(_,Id2,Atom2), Rows),    % busca el término con ese ID
    atom_string(Atom2, P).               % átomo → string

%------------------------------------------------------------------------------%
% prompt_language/0
% Muestra opciones de idioma y guarda la elección en current_lang/1.
%------------------------------------------------------------------------------%
prompt_language :-
    writeln('?Idioma de la frase?'),
    writeln('  1) English'),
    writeln('  2) Espanol'),
    write('Opcion: '), read_choice(L),
    (   L =:= 1 -> Lang = en
    ;   L =:= 2 -> Lang = es
    ;   writeln('Opcion invalida.'), prompt_language
    ),
    retractall(current_lang(_)),         % elimina posible idioma anterior
    assertz(current_lang(Lang)).         % guarda nuevo idioma

%------------------------------------------------------------------------------%
% menu_loop/0
% Bucle principal de menú. Según current_lang/1:
%  - muestra opciones de preprocesado, traducción y análisis
%  - también permite cambiar de frase o salir
% Utiliza repeat/fail para iterar hasta que elija 'exit'.
%------------------------------------------------------------------------------%
menu_loop :-
    repeat,
      current_lang(L), current_phrase(P),
      format('\nFrase actual (~w): ~s~n', [L,P]),
      findall(Label, menu_item(L,Label,_), Labels),
      findall(Code,  menu_item(L,_,Code),  Codes),
      print_menu_items(Labels,1),
      write('Seleccione opcion: '), read_choice(Idx),
      (   nth1(Idx,Codes,Code)
      ->  ( Code == exit -> !            % si es exit, corta el repeat
          ; perform(Code), fail          % ejecuta y repite
          )
      ;   writeln('Opcion no valida.'), fail
      ).

%------------------------------------------------------------------------------%
% menu_item(+Lang, -Label, -Code)
% Define las entradas de menú por idioma o globales.
%------------------------------------------------------------------------------%
menu_item(es, 'Preprocesar Espanol', pre_es).
menu_item(es, 'Traducir ES->EN',    trans_es_en).

menu_item(en, 'Preprocesar English', pre_en).
menu_item(en, 'Traducir EN->ES',     trans_en_es).
menu_item(en, 'Analisis sintactico', analysis).

menu_item(_, 'Cambiar frase', change).
menu_item(_, 'Salir',         exit).

%------------------------------------------------------------------------------%
% print_menu_items(+Labels, +N)
% Muestra la lista numerada de etiquetas de menú.
%------------------------------------------------------------------------------%
print_menu_items([], _) :- !.
print_menu_items([L|Ls], N) :-
    format(' ~w) ~w~n', [N,L]),
    N1 is N+1,
    print_menu_items(Ls,N1).

%------------------------------------------------------------------------------%
% perform(+Code)
% Despacha el código de opción a la rutina correspondiente.
%------------------------------------------------------------------------------%
perform(pre_es)      :- process_pre_es.
perform(trans_es_en) :- process_trans_es_en.
perform(pre_en)      :- process_pre_en.
perform(trans_en_es) :- process_trans_en_es.
perform(analysis)    :- do_analysis.
perform(change)      :- choose_phrase.
perform(exit).       % No hace nada, provoca salida en menu_loop

%------------------------------------------------------------------------------%
% process_pre_es/0: preprocesa en español y muestra tokens
%------------------------------------------------------------------------------%
process_pre_es :-
    current_phrase(P),
    preprocesar:preprocesar_es(P, Ts),
    format('Tokens ES: ~w~n', [Ts]).

%------------------------------------------------------------------------------%
% process_pre_en/0: limpia comillas simples, preprocesa en inglés y muestra tokens
%------------------------------------------------------------------------------%
process_pre_en :-
    current_phrase(P),
    clean_single_quotes(P, CleanP),        % elimina comillas simples
    preprocesar:preprocesar_en(CleanP, Ts),
    format('Tokens EN: ~w~n', [Ts]).

%------------------------------------------------------------------------------%
% clean_single_quotes(+Input, -Cleaned)
% Elimina todos los códigos 39 (') de la cadena.
%------------------------------------------------------------------------------%
clean_single_quotes(Input, Cleaned) :-
    string_codes(Input, Codes),
    exclude(=(39), Codes, NoQuotes),       % filtra códigos igual a 39
    string_codes(Cleaned, NoQuotes).

%------------------------------------------------------------------------------%
% process_trans_es_en/0 y process_trans_en_es/0
% Traducción libre entre ES↔EN usando traductorGoogle.pl
%------------------------------------------------------------------------------%
process_trans_es_en :-
    current_phrase(P),
    translate_free(P, es, en, Out),
    format('ES->EN: ~s~n', [Out]).

process_trans_en_es :-
    current_phrase(P),
    translate_free(P, en, es, Out),
    format('EN->ES: ~s~n', [Out]).

%------------------------------------------------------------------------------%
% do_analysis/0
% Análisis sintáctico de la frase en inglés:
% 1. Limpia comillas
% 2. Preprocesa en EN
% 3. Parsea con oracion/4
% 4. Normaliza estructuras
% 5. Entra en sub-menú de análisis
%------------------------------------------------------------------------------%
do_analysis :-
    current_phrase(P),
    clean_single_quotes(P, CleanP),        % elimina comillas
    preprocesar:preprocesar_en(CleanP, Toks),
    (   prueba_2:oracion(eng, Raw0, Toks, [])  % intenta parsear todos los tokens
    ->  ( Raw0 = [F|_] -> Flat = F ; Flat = Raw0 ),
        normalise_trees(Flat, Trees),         % convierte a lista si hace falta
        analysis_menu(Trees)
    ;   writeln('No se pudo analizar.')
    ).

%------------------------------------------------------------------------------%
% Sub-menú de análisis sintáctico
% analysis_menu(+Trees)
% Opciones:
%  1) Mostrar árbol en ASCII
%  2) Generar HTML con subrayado
%  3) Mostrar la estructura Prolog interna
%  0) Volver
%------------------------------------------------------------------------------%
analysis_menu(Trees) :-
    repeat,
      nl, writeln('------ ANALISIS ------'),
      writeln(' 1) Mostrar arbol ASCII'),
      writeln(' 2) Generar HTML subrayado'),
      writeln(' 3) Mostrar estructuras Prolog'),
      writeln(' 0) Volver'),
      write('Opcion: '), read_choice(Op),
      (   Op =:= 0 -> !
      ;   run_analysis_option(Op, Trees), fail
      ).

%------------------------------------------------------------------------------%
% run_analysis_option(+Opcion, +Trees)
% Ejecuta la opción de análisis seleccionada.
%------------------------------------------------------------------------------%
run_analysis_option(1, Trees) :-
    % Dibuja cada árbol en ASCII
    forall(member(T, Trees),
      ( nl, draw:draw(T), nl )
    ).
run_analysis_option(2, Trees) :-
    % Genera un HTML subrayado para cada árbol
    forall(nth1(I, Trees, T),
      ( format('Generando subrayado_~w.html...~n', [I]),
        format(atom(Base), 'subrayado_~w', [I]),
        draw_html:draw_html(T, Base)
      )
    ).
run_analysis_option(3, Trees) :-
    % Muestra la lista de árboles en forma Prolog
    writeln('Estructura Prolog resultante:'), writeln(Trees), nl.
run_analysis_option(_, _) :-
    writeln('Opcion no valida.').

%------------------------------------------------------------------------------%
% read_choice(-N)
% Lee una línea de entrada y la convierte a número.
% Si falla, solicita de nuevo.
%------------------------------------------------------------------------------%
read_choice(N) :-
    read_line_to_string(user_input, S),
    (   number_string(N, S)
    ->  true
    ;   writeln('Entrada invalida, ingrese un numero.'), read_choice(N)
    ).

%------------------------------------------------------------------------------%
% ask_sentence(+Prompt, -S)
% Muestra un prompt y lee una línea, normalizando espacios.
%------------------------------------------------------------------------------%
ask_sentence(Prompt, S) :-
    format('~w:~n> ', [Prompt]),
    read_line_to_string(user_input, S0),
    normalize_space(string(S), S0).

%------------------------------------------------------------------------------%
% normalise_trees(+Raw, -List)
% Asegura que la salida sea lista de árboles, incluso si solo hay uno.
%------------------------------------------------------------------------------%
normalise_trees(Raw, List) :-
    (   Raw = [_|_] -> List = Raw
    ;   List = [Raw]
    ).
