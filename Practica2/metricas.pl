:- module(metricas,
    [ metricas_sp2/1,
      metricas_verbos/1
    ]).

:- use_module(library(csv)).
:- use_module(library(apply)).
:- use_module(library(pairs)).    % ← necesario para group_pairs_by_key/2
:- use_module(draw).

:- consult('preprocesar.pl').
:- consult('prueba_2.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%FRASES DEL ENUNCIADO TRADUCIDAS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fe1('jose is dark-skinned and mary is tall').
fe2('jose is studying philosophy but mary is studying law').
fe3('mary drinks coffee while jose clears the table').
fe4('jose drinks coffee and reads the newspaper').
fe5('jose and hector eat chips and drink beer').
fe6('jose eats chips but mary prefers paella although hector drinks coffee and irene reads a novel').
fe7('irene sings and jumps while jose studies').
fe8('hector eats chips and drinks juice while jose sings and skips, although maria reads a novel').
fe9('jose, who is agile, climbs on the climbing wall in the afternoons').
fe10('jose, who is very delicate, eats only red apples').
fe11('the word processor, which is quite a powerful tool, is used to write documents').
fe12('the word processor is a very powerful tool for writing documents but it is quite slow').
fe13('the mouse that the cat caught was grey').
fe14('the man we saw yesterday was my neighbour').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%FRASES PARA LA TEMÁTICA DEPORTES%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

f1('juan plays football while pedro plays basketball').
f2('javier and juan are the goalkeepers of the team').
f3('rafa has won the roland-garros').
f4('aitana has won the golden ball').
f5('julian is a great player').
f6('miguel is the best basketball player and leo is the best football player').
f7('marta is the best swimmer in her club').
f8('alex plays tennis while marcos plays paddle tennis').
f9('paula has won a gold medal in synchronised swimming').
f10('sara and claudia compete together in rhythmic gymnastics').
f11('daniel is an expert in martial arts and has won several tournaments').
f12('beatriz runs marathons and trains every day').
f13('maria and carlos are the best sports dance couple in the region').
f14('andres plays rugby while diego practices boxing').
f15('lucas has been elected the best player of the tournament').
f16('elena and sofia are the captains of the volleyball team').
f17('tomas has broken the long jump record in his category').
f18('laura practices climbing and has climbed impressive mountains').
f19('javier has been selected for the national handball team').
f20('ana and luis are taking part in an international triathlon competition').
f21('jaime and natalia train together for the next cycling race').
f22('ruben practices archery while ines plays hockey').
f23('clara has been named the best chess player in her school').
f24('alvaro has won the junior tennis championship').
f25('cristina and david are taking part in an international athletics competition').
f26('santiago has beaten his personal record in weightlifting').
f27('jorge and rocio compete in whitewater kayaking').
f28('sofia has achieved a new record in freestyle swimming').
f29('pedro and angela play table tennis on weekends and have won several tournaments').
f30('diego goes skiing while martina enjoys snowboarding').
f31('lucia has won the title of regional champion in artistic gymnastics').
f32('alberto, who is very agile, practises parkour around the city').
f33('teresa has won a gold medal in shooting').
f34('miguel enjoys playing badminton after school').
f35('alexis, who is very strong, trains for the wrestling championship').
f36('the girl we greeted before was his ex-teammate').
f37('the man that irene likes was very skillful').
f38('ariana, who has great dexterity, plays volleyball on the beach').
f39('the artificial intelligence, which is changing the world of sports, is very advanced').
f40('the sports handbag, which is chinese, was imported last week').

%Predicado para analizar las frases en ingles
analizar_frases_en :-
        consult('prueba_2.pl'), % Cargar el archivo de frases
        consult('preprocesar.pl'), % Cargar el archivo de preprocesamiento
        %Lista de frases a analizar
        FrasesEnuncuiado = [fe1(Sentence), fe2(Sentence), fe3(Sentence), fe4(Sentence), fe5(Sentence), fe6(Sentence), fe7(Sentence), fe8(Sentence), fe9(Sentence), fe10(Sentence),
                fe11(Sentence), fe12(Sentence), fe13(Sentence), fe14(Sentence)],
        FrasesDeportes = [f1, f2, f3, f4, f5, f6, f7, f8, f9, f10,
                f11, f12, f13, f14, f15, f16, f17, f18,
                f19, f20, f21, f22, f23, f24, f25, f26,
                f27, f28, f29, f30, f31, f32, f33, f34,
                f35, f36, f37, f38, f39,f40],
        % Procesar las frases del enunciado
        forall(member(Frase, FrasesEnuncuiado), (
            Frase, % Obtener la frase
            preprocesar_en(Sentence, Tokens), % Preprocesar la frase
            write('Tokens: '), writeln(Tokens), % Mostrar los tokens
            oracion(eng,X,Tokens,[]), % Analizar la frase
            write('Frase analizada: '), writeln(X) % Mostrar la frase analizada
        )).
        /*% Procesar las frases de deportes
        forall(member(Frase, FrasesDeportes), (
            fe(Frase, Sentence), % Obtener la frase
            preprocesar_en(Sentence, Tokens), % Preprocesar la frase
            write('Tokens: '), writeln(Tokens) % Mostrar los tokens
            %oracion(eng,analizado,Tokens,[]), % Analizar la frase
            %write('Frase analizada: '), writeln(analizado) % Mostrar la frase analizada
        )).
        */
exportar_frases_csv2(File) :-
    setup_call_cleanup(
      open(File, write, Stream, [encoding(utf8)]),
      (
        format(Stream, 'id;frase~n', []),
        % recogemos las 14+40 frases
        findall(S, (between(1,14,N), atom_concat(fe,N,P), Goal=..[P,S], call(Goal)), L1),
        findall(S, (between(1,40,M), atom_concat(f, M, P), Goal=..[P,S], call(Goal)), L2),
        append(L1, L2, All),
        export_phrases(Stream, 1, All)
      ),
      close(Stream)
    ).

% Caso base
export_phrases(_,    _,    [])     :- !.
export_phrases(Stream, N, [S|Rest]) :-
    % Parto la frase en "palabras" (espacios)
    split_string(S, " ", "", WordsStrings),
    expand_commas(WordsStrings, TokenStrs),
    % Uno con comas
    atomic_list_concat(TokenStrs, ',', Inner),
    format(Stream, '~w;[~w]~n', [N, Inner]),
    N1 is N+1,
    export_phrases(Stream, N1, Rest).
%% expand_commas(+WordsStrings, -Tokens)
%% si un elemento acaba en ',' lo descompone en [PalabraSinComa, "','"]
expand_commas([], []).
expand_commas([W|Ws], Tokens) :-
    ( sub_string(W, _, 1, 0, ",") ->
        sub_string(W, 0, _, 1, Wno),
        Tokens = [Wno, "','" | Rest]
    ;
        Tokens = [W | Rest]
    ),
    expand_commas(Ws, Rest).

%% exportar_frases_csv(+File)
%% genera CSV "id;frase" con 54 filas, y las comas internas como literal "','"
exportar_frases_csv(File) :-
    Sep = 59,                              % ';' como separador
    Options = [separator(Sep)],
    setup_call_cleanup(
      open(File, write, Stream, [encoding(utf8)]),
      (
        % 1) Cabecera
        csv_write_stream(Stream, [row(id,frase)], Options),
        % 2) Arrancar contador
        nb_setval(frase_counter, 1),
        % 3) Emitir fe1–fe14 y f1–f40
        escribir_serie_seq(Stream, fe, 14, Options),
        escribir_serie_seq(Stream, f,  40, Options)
      ),
      close(Stream)
    ).

%% escribir_serie_seq(+Stream, +Prefijo, +Total, +Opts)
%% por cada N=1..Total:
%%   - llama a PrefijoN/1 si existe, o '' si no
%%   - tokeniza por espacios, expande comas, vuelve a ensamblar
%%   - escribe [Id, FraseEscapada], incrementa Id
escribir_serie_seq(Stream, Prefijo, Total, Opts) :-
    forall(
      between(1, Total, N),
      (
        format(atom(Pred), '~w~d', [Prefijo, N]),
        Goal =.. [Pred, Frase0],
        ( call(Goal) -> true ; Frase0 = '' ),

        % 1) partir en palabras
        split_string(Frase0, " ", "", Words0),
        % 2) expandir comas
        expand_commas(Words0, Tokens),
        % 3) volver a frase
        atomic_list_concat(Tokens, " ", FraseEscapada),

        % 4) obtener y escribir Id
        nb_getval(frase_counter, Id),
        csv_write_stream(Stream, [row(Id, FraseEscapada)], Opts),
        % 5) incrementar contador
        Id1 is Id + 1,
        nb_setval(frase_counter, Id1)
      )
    ).
%%--------------------------------------------------
%% Extraer sujeto/predicado de un término o/2 o lista
%%--------------------------------------------------

sujeto(o(S,_), S) :- !.
sujeto([T|_],    S) :- sujeto(T, S), !.
predicado(o(_,P), P) :- !.
predicado([T|_],    P) :- predicado(T, P), !.

%%--------------------------------------------------
%% Contar hojas (palabras) en un árbol
%%--------------------------------------------------

% contar_palabras(+Árbol, -N)
%    N = nº de nodos terminales (hojas)
contar_palabras(T, N) :-
    T =.. [_Functor|Args],
    ( Args = [] -> N = 1
    ; maplist(contar_palabras, Args, Ns), sum_list(Ns, N)
    ).

% contar_sujeto_predicado(+Oracion, -Ns, -Np)
%    Ns = hojas en sujeto, Np = hojas en predicado
contar_sujeto_predicado(Or, Ns, Np) :-
    sujeto(Or, S), contar_palabras(S, Ns),
    predicado(Or, P), contar_palabras(P, Np).

%%--------------------------------------------------
%% metricas_sp2(+ArchivoCSV)
%%  Lee CSV(categoria;id;frase), imprime:
%%  Cat Id Ns Np ( palabras sujeto/predicado)
%%--------------------------------------------------
metricas_sp2(File) :-
    csv_read_file(File, [rec(categoria,id,frase)|Rows],
                  [separator(59), functor(rec), arity(3)]),
    format('Cat\tId\tSubjWords\tPredWords~n'),
    forall(member(rec(Cat,Id,SentAtom), Rows),
      (
        atom_string(SentAtom,Sentence),
        preprocesar_en(Sentence,Tokens),
        ( oracion(eng, Tree0, Tokens, [])
        -> ( is_list(Tree0) -> Trees=Tree0 ; Trees=[Tree0] ),
           forall(member(Or,Trees),
             ( contar_sujeto_predicado(Or,NS,NP),
               format('~w\t~w\t~d\t~d~n',[Cat,Id,NS,NP])
             )
           )
        ; format('~w\t~w\t0\t0~n',[Cat,Id])
        )
      )
    ).


%% contar_verbos(+Árbol, -N)
%%   N = número de functors v(...) en Árb o en cualquier lista anidada.
contar_verbos(Term, 0) :-
    var(Term), !.
contar_verbos(Term, N) :-
    is_list(Term), !,
    maplist(contar_verbos, Term, Ns),
    sum_list(Ns, N).
contar_verbos(Term, N) :-
    nonvar(Term),
    Term =.. [F|Args],
    ( F == v -> N0 = 1 ; N0 = 0 ),
    maplist(contar_verbos, Args, Ns),
    sum_list(Ns, Sum),
    N is N0 + Sum.

%% metricas_verbos(+ArchivoCSV)
%% Lee CSV(categoria;id;frase), analiza cada frase,
%% y muestra por categoría:
%%   • número de oraciones (líneas) analizadas
%%   • total de verbos encontrados
%%   • media de verbos por oración
metricas_verbos(File) :-
    csv_read_file(File, [rec(categoria,id,frase)|Rows],
                  [separator(59), functor(rec), arity(3)]),
    % Para cada fila: procesar la frase y sumar verbos de todos sus árboles
    findall(Cat-VerbCount,
        (
            member(rec(Cat,_,Atom), Rows),
            atom_string(Atom, Sentence),
            preprocesar_en(Sentence, Tokens),
            (   oracion(eng, Tree0, Tokens, [])
            ->  ( is_list(Tree0) -> Trees = Tree0 ; Trees = [Tree0] )
            ;   Trees = []
            ),
            % contar verbos en cada sub-árbol y sumarlos
            maplist(contar_verbos, Trees, Counts),
            sum_list(Counts, VerbCount)
        ),
        Pairs),
    % agrupar por categoría
    group_pairs_by_key(Pairs, Grouped),
    % mostrar resultados
    forall(
        member(Cat-CountsPerSentence, Grouped),
        (
            length(CountsPerSentence, NumOr),
            sum_list(CountsPerSentence, TotalV),
            AvgV is TotalV / NumOr,
            format('\n--- Categoría: ~w ---\n', [Cat]),
            format('Oraciones:          ~d\n', [NumOr]),
            format('Total de verbos:    ~d\n', [TotalV]),
            format('Verbos por oración: avg=~2f\n', [AvgV])
        )
    ).
