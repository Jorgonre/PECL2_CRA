:- use_module(library(csv)).

:- set_prolog_flag(stack_limit, 2_147_483_648).

% Carga el CSV ubicado en 'verbos.csv'  
% Se asume que el CSV está separado por punto y coma (;) y tiene una cabecera.
load_verbs :-
    csv_read_file('verbos.csv', Rows,
                  [ separator(59),    % 59 es el código ASCII de ';'
                    skip_header(true)
                  ]),
    maplist(process_row, Rows).

% process_row(+Row)
% Procesa cada fila obtenida del CSV.
% Se asume que la fila es un término row(...).
% Extrae solo los dos primeros campos: Palabra y Raiz.
process_row(Row) :-
    Row =.. [_Functor|Fields],
    ( Fields = [Palabra, Raiz] 
    ; Fields = [Palabra, Raiz, _Extra]  % Si existe un tercer campo vacío, lo descartamos.
    ),
    assertz(verb(Palabra, Raiz)).

% Predicado infinitivo/2
% Dada una forma conjugada (ej. 'dovelo') retorna la raíz/infinitivo (ej. 'dovelar')
infinitivo(Conjugado, Infinitivo) :-
    verb(Conjugado, Infinitivo).
