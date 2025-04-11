:- module(preprocesar, [preprocesar/2]).

%% preprocesar(+Sentence, -Tokens)
%  Convierte la cadena Sentence a minúsculas, elimina signos de puntuación
%  y la divide en una lista de tokens (átomos), respetando contracciones en inglés.
preprocesar(Sentence, Tokens) :-
    (   atom(Sentence)
    ->  atom_string(Sentence, SentenceStr)
    ;   SentenceStr = Sentence
    ),
    string_lower(SentenceStr, Lower),
    split_string(Lower, " ", " \n\r\t.,!?;:-_()[]{}\"", RawTokens),
    exclude(==( ""), RawTokens, CleanedTokens),
    maplist(atom_string, Tokens, CleanedTokens).
