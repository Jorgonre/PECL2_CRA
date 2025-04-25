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
preprocesar_en(Sentence, Tokens) :-
    atom_or_string_to_string(Sentence, Str0),
    string_lower(Str0, Lower0),
    re_replace("^[0-9]+;", "", Lower0, WithoutId),
    % Aislar todas las comas (globalmente) como tokens separados
    re_replace("\\s*,\\s*"/g, " , ", WithoutId, WithSpaces),
    expand_contractions_en(WithSpaces, Expanded),
    remove_punctuation_en(Expanded, Clean),
    split_string(Clean, " ", "", Raw0),
    exclude(==(""), Raw0, Raw1),
    maplist(normalize_en_token, Raw1, Tokens).

% convierte coma en átomo "coma" y tokens exactos de la forma "'C'" → "C"
normalize_en_token(",", "coma") :- !.
normalize_en_token(S, C) :-
    string_chars(S, ['\'', Ch, '\'']), !,
    string_chars(C, [Ch]).
normalize_en_token(S, S).

remove_punctuation_en(In, Out) :-
    string_chars(In, Cs),
    include(valid_char_en, Cs, Fs),
    string_chars(Out, Fs).

valid_char_en(C) :-
       char_type(C, alnum)  % letras y dígitos
    ;  C = ' '
    ;  C = '\''
    ;  C = '-'
    ;  C = ','.  % permitimos coma como carácter válido para convertir luego en token

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Contracciones inglés → inglés expansión
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_contractions_en(In, Out) :-
    EN = [
      ("isn't","is not"),("aren't","are not"),("wasn't","was not"),
      ("weren't","were not"),("don't","do not"),("doesn't","does not"),
      ("didn't","did not"),("can't","can not"),("couldn't","could not"),
      ("shouldn't","should not"),("wouldn't","would not"),("won't","will not"),
      ("hasn't","has not"),("haven't","have not"),("hadn't","had not"),
      ("mustn't","must not"),("i'm","i am"),("you're","you are"),
      ("he's","he is"),("she's","she is"),("it's","it is"),
      ("we're","we are"),("they're","they are"),("i've","i have"),
      ("you've","you have"),("they've","they have"),("i'll","i will"),
      ("you'll","you will"),("he'll","he will"),("she'll","she will"),
      ("they'll","they will"),("i'd","i would"),("you'd","you would"),
      ("he'd","he would"),("she'd","she would"),("they'd","they would"),
      ("that's","that is"),("there's","there is")
    ],
    expand_all(EN, In, Out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliares
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_all([], T, T).
expand_all([(A,B)|R], In, Out) :-
    atomic_list_concat(Parts, A, In),
    atomic_list_concat(Parts, B, Mid),
    expand_all(R, Mid, Out).

atom_or_string_to_string(X, Str) :-
    ( atom(X) -> atom_string(X, Str) ; Str = X ).