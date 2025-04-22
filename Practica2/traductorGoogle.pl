:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

%% translate_free(+Texto, +IdiomaOrigen, +IdiomaDestino, -Traduccion)
%  Traduce Texto de IdiomaOrigen a IdiomaDestino usando el endpoint gtx.
translate_free(Texto, Src, Tgt, Traduccion) :-
    % 1) Codificamos el texto para URL
    uri_encoded(query_value, Texto, Q),
    % 2) Montamos la URL completa
    format(string(URL),
      'https://translate.googleapis.com/translate_a/single?client=gtx&sl=~w&tl=~w&dt=t&ie=UTF-8&oe=UTF-8&q=~s',
      [Src, Tgt, Q]),
    % 3) Hacemos la petición GET
    http_get(URL, JSONAtom, [request_header('Accept', 'application/json')]),
    % 4) Convertimos el átomo JSON en término Prolog (listas anidadas)
    atom_json_term(JSONAtom, JSONTerm, [value_string_as(atom)]),
    % 5) Extraemos la primera traducción
    nth0(0, JSONTerm, PrimerArray),
    nth0(0, PrimerArray, PrimerFragmento),
    nth0(0, PrimerFragmento, Traduccion).
