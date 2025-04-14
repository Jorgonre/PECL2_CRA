:- module(preprocesar, [
    preprocesar_es/2,
    preprocesar_en/2
]).

/* 
   Preprocesa cadenas ES o EN:
   - to lowercase
   - elimina puntuación
   - expande contracciones
   - separa en tokens (átomos)
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 1) PREPROCESAR EN ESPAÑOL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
preprocesar_es(Sentence, Tokens) :-
    atom_or_string_to_string(Sentence, Str),
    string_lower(Str, Lower),
    expand_contractions_es(Lower, Expanded),
    remove_punctuation_es(Expanded, CleanPunct),
    split_string(CleanPunct, " ", "", Split),
    exclude(==( ""), Split, NonEmpty),
    maplist(atom_string, Tokens, NonEmpty).

%% Si quieres expandir alguna "contracción" en español, ajusta aquí:
expand_contractions_es(In, Out) :-
    % Un ejemplo minimalista: "del" -> "de el", "al" -> "a el"
    ESContracciones = [
        ("del", "de el"),
        ("al", "a el")
    ],
    expand_all(ESContracciones, In, Out).

remove_punctuation_es(In, Out) :-
    string_chars(In, Chars),
    include(valid_char_es, Chars, Filtered),
    string_chars(Out, Filtered).

valid_char_es(C) :-
    char_type(C, alnum)
    ; C = ' '
    ; C = ''''.  % por si aparece algún apóstrofe


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2) PREPROCESAR EN INGLÉS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
preprocesar_en(Sentence, Tokens) :-
    atom_or_string_to_string(Sentence, Str),
    string_lower(Str, Lower),
    expand_contractions_en(Lower, Expanded),
    remove_punctuation_en(Expanded, CleanPunct),
    split_string(CleanPunct, " ", "", Split),
    exclude(==( ""), Split, NonEmpty),
    maplist(atom_string, Tokens, NonEmpty).

expand_contractions_en(In, Out) :-
    ENContracciones = [
      ("isn't", "is not"),
      ("aren't", "are not"),
      ("wasn't", "was not"),
      ("weren't", "were not"),
      ("don't", "do not"),
      ("doesn't", "does not"),
      ("didn't", "did not"),
      ("can't", "can not"),
      ("couldn't", "could not"),
      ("shouldn't", "should not"),
      ("wouldn't", "would not"),
      ("won't", "will not"),
      ("hasn't", "has not"),
      ("haven't", "have not"),
      ("hadn't", "had not"),
      ("mustn't", "must not"),
      ("i'm", "i am"),
      ("you're", "you are"),
      ("he's", "he is"),
      ("she's", "she is"),
      ("it's", "it is"),
      ("we're", "we are"),
      ("they're", "they are"),
      ("i've", "i have"),
      ("you've", "you have"),
      ("they've", "they have"),
      ("i'll", "i will"),
      ("you'll", "you will"),
      ("he'll", "he will"),
      ("she'll", "she will"),
      ("they'll", "they will"),
      ("i'd", "i would"),
      ("you'd", "you would"),
      ("he'd", "he would"),
      ("she'd", "she would"),
      ("they'd", "they would"),
      ("that's", "that is"),
      ("there's", "there is")
    ],
    expand_all(ENContracciones, In, Out).

remove_punctuation_en(In, Out) :-
    string_chars(In, Chars),
    include(valid_char_en, Chars, Filtered),
    string_chars(Out, Filtered).

valid_char_en(C) :-
    char_type(C, alnum)
    ; C = ' '
    ; C = ''''.  % permitir apóstrofes para contracciones

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Auxiliares Comunes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
expand_all([], Text, Text).
expand_all([(From, To)|Rest], In, Out) :-
    atomic_list_concat(Parts, From, In),
    atomic_list_concat(Parts, To, Temp),
    expand_all(Rest, Temp, Out).

atom_or_string_to_string(X, Str) :-
    ( atom(X) -> atom_string(X, Str) ; Str = X ).
