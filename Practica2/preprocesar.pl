:- module(preprocesar, [preprocesar/2]).

%% preprocesar(+Sentence, -Tokens)
% Convierte la oración a minúsculas, expande contracciones, limpia puntuación
% y la convierte en una lista de tokens como átomos.

preprocesar(Sentence, Tokens) :-
    ( atom(Sentence) -> atom_string(Sentence, String) ; String = Sentence ),
    string_lower(String, Lower),
    expand_contractions(Lower, Expanded),
    remove_punctuation(Expanded, Cleaned),
    split_string(Cleaned, " ", "", Split),
    exclude(==( ""), Split, NonEmpty),
    maplist(atom_string, Tokens, NonEmpty).

%% expand_contractions(+In, -Out)
% Reemplaza contracciones comunes por sus formas completas (a mano).
expand_contractions(In, Out) :-
    contractions(Pairs),
    expand_all(Pairs, In, Out).

expand_all([], Text, Text).
expand_all([(From, To)|Rest], In, Out) :-
    atomic_list_concat(Words, From, In),
    atomic_list_concat(Words, To, Temp),
    expand_all(Rest, Temp, Out).

contractions([
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
]).

%% remove_punctuation(+In, -Out)
% Elimina caracteres de puntuación comunes excepto apóstrofes internos.
remove_punctuation(In, Out) :-
    string_chars(In, Chars),
    include(valid_char, Chars, CleanChars),
    string_chars(Out, CleanChars).

valid_char(Char) :-
    char_type(Char, alnum);
    Char = ' ';
    Char = '\''.
