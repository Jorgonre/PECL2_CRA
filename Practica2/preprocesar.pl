%------------------------------------------------------------------------------%
% Módulo: preprocesar
% Exporta dos predicados principales:
%   - preprocesar_es/2: tokeniza cadenas en español
%   - preprocesar_en/2: tokeniza cadenas en inglés
%------------------------------------------------------------------------------%
:- module(preprocesar, [
    preprocesar_es/2,
    preprocesar_en/2
]).

%------------------------------------------------------------------------------%
% Configuración
% Evita que Prolog imprima átomos con comillas cuando contienen caracteres
% especiales (tildes, eñes, etc.).
%------------------------------------------------------------------------------%
:- set_prolog_flag(answer_write_options, [quoted(false)]).

/*
   El objetivo:
   - Convertir la cadena a minúsculas
   - Eliminar signos de puntuación no deseados
   - Separar en palabras (tokens)
   - Devolver la lista de tokens como átomos
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) PREPROCESAR EN ESPAÑOL  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Sentence puede ser un átomo o string; Tokens es la lista de átomos resultante.
preprocesar_es(Sentence, Tokens) :-
    % 1. Convertir átomo o string a string uniforme
    atom_or_string_to_string(Sentence, Str),
    % 2. Pasar todo a minúsculas
    string_lower(Str, Lower),
    % 3. Eliminar puntuación manteniendo letras, dígitos, espacio y guión
    remove_punctuation_es(Lower, CleanPunct),
    % 4. Dividir por espacios (puede generar cadenas vacías)
    split_string(CleanPunct, " ", "", Split),
    % 5. Quitar las entradas vacías
    exclude(==( ""), Split, NonEmpty),
    % 6. Convertir cada string en átomo
    maplist(atom_string, Tokens, NonEmpty).

% Filtra la lista de caracteres para quedarse sólo con los válidos en español.
remove_punctuation_es(In, Out) :-
    string_chars(In, Chars),
    include(valid_char_es, Chars, Filtered),
    string_chars(Out, Filtered).


% Define los caracteres permitidos:
%   - alfanuméricos (letras y dígitos)
%   - espacio
%   - guión (para palabras compuestas)
valid_char_es(C) :-
    char_type(C, alnum)
    ; C = ' '
    ; C = '-' .   % guión

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) PREPROCESAR EN INGLÉS   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Igual que en español, pero antes expande contracciones típicas del inglés.
preprocesar_en(Sentence, Tokens) :-
    atom_or_string_to_string(Sentence, Str),
    string_lower(Str, Lower),
    % 1. Expansión de contracciones ("can't" → "can not", etc.)
    expand_contractions_en(Lower, Expanded),
    % 2. Eliminación de puntuación 
    remove_punctuation_en(Expanded, CleanPunct),
    % 3. División en tokens
    split_string(CleanPunct, " ", "", Split),
    exclude(==( ""), Split, NonEmpty),
    maplist(atom_string, Tokens, NonEmpty).

% Lista de pares (contracción, expansión) y llamado a expand_all.
expand_contractions_en(In, Out) :-
    ENContracciones = [
        ("isn't",    "is not"),
        ("aren't",   "are not"),
        ("wasn't",   "was not"),
        ("weren't",  "were not"),
        ("don't",    "do not"),
        ("doesn't",  "does not"),
        ("didn't",   "did not"),
        ("can't",    "can not"),
        ("couldn't", "could not"),
        ("shouldn't","should not"),
        ("wouldn't", "would not"),
        ("won't",    "will not"),
        ("hasn't",   "has not"),
        ("haven't",  "have not"),
        ("hadn't",   "had not"),
        ("mustn't",  "must not"),
        ("i'm",      "i am"),
        ("you're",   "you are"),
        ("he's",     "he is"),
        ("she's",    "she is"),
        ("it's",     "it is"),
        ("we're",    "we are"),
        ("they're",  "they are"),
        ("i've",     "i have"),
        ("you've",   "you have"),
        ("they've",  "they have"),
        ("i'll",     "i will"),
        ("you'll",   "you will"),
        ("he'll",    "he will"),
        ("she'll",   "she will"),
        ("they'll",  "they will"),
        ("i'd",      "i would"),
        ("you'd",    "you would"),
        ("he'd",     "he would"),
        ("she'd",    "she would"),
        ("they'd",   "they would"),
        ("that's",   "that is"),
        ("there's",  "there is")
    ],
    expand_all(ENContracciones, In, Out).


% Igual que remove_punctuation_es pero con su propio predicado de validación.
remove_punctuation_en(In, Out) :-
    string_chars(In, Chars),
    include(valid_char_en, Chars, Filtered),
    string_chars(Out, Filtered).

% Caracteres permitidos en inglés (idénticos a español en este caso además del apóstrofe para posesivos y contracciones).
valid_char_en(C) :-
    char_type(C, alnum)
    ; C = ' '
    ; C = ''''  % apóstrofe
    ; C = '-' . % guión

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%         Auxiliares         %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reemplaza cada ocurrencia de From por To recursivamente.
expand_all([], Text, Text).
expand_all([(From, To)|Rest], In, Out) :-
    % Divide In en partes separadas por From
    atomic_list_concat(Parts, From, In),
    % Reconstruye uniendo con To
    atomic_list_concat(Parts, To, Temp),
    % Continúa con el resto de la lista
    expand_all(Rest, Temp, Out).

% Si X es átomo, lo convierte a string; si ya es string, lo deja igual.
atom_or_string_to_string(X, Str) :-
    ( atom(X)
    -> atom_string(X, Str)
    ;  Str = X
    ).
