:- module(preprocesar, [preprocesar/2]).

%% preprocesar(+Sentence, -Tokens)
%  Convierte la cadena Sentence a minúsculas, elimina algunos signos de puntuación
%  y la divide en una lista de tokens (átomos).
preprocesar(Sentence, Tokens) :-
    string_lower(Sentence, LowerSentence),
    split_string(LowerSentence, " ", ".,!?;:-_()[]", TokenStrings),
    maplist(atom_string, Tokens, TokenStrings).
