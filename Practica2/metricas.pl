:- use_module(library(csv)).
:- use_module(library(apply)).

:- use_module(draw).
              
:- consult('preprocesar.pl').         % preprocesar_en/2
:- consult('prueba_2.pl').            % definiciones de fe*/f*


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%FRASES DEL ENUNCIADO TRADUCIDAS%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fe1('jose is dark-skinned and mary is tall').
fe2('jose is studying philosophy, but mary is studying law').
fe3('mary drinks coffee while jose clears the table').
fe4('jose drinks coffee and reads the newspaper').
fe5('jose and hector eat chips and drink beer').
fe6('jose eats chips, but mary prefers paella, although hector drinks coffee and irene reads a novel').
fe7('irene sings and jumps while jose studies').
fe8('hector eats chips and drinks juice while jose sings and skips, although maria reads a novel').
fe9('jose, who is agile, climbs on the climbing wall in the afternoons').
fe10('jose, who is very delicate, eats only red apples').
fe11('the word processor, which is quite a powerful tool, is used to write documents').
fe12('the word processor is a very powerful tool for writing documents, but it is quite slow').
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
f29('pedro and angela play table tennis on weekends').
f30('diego goes skiing while martina enjoys snowboarding').
f31('lucia has won the title of regional champion in artistic gymnastics').
f32('alberto, who is very agile, practises parkour around the city').
f33('teresa has won a gold medal in shooting').
f34('miguel and sergio enjoy playing badminton after school').
f35('alexis, who is very strong, trains for the wrestling championship').
f36('elisa and pablo are preparing for a relay race').
f37('eduardo practices judo and has obtained his black belt').
f38('ariana, who has great dexterity, plays volleyball on the beach').
f39('andrea has participated in her first triathlon and has managed to finish on the podium').
f40('oscar trains daily to improve his pole vaulting technique').

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

exportar_frases_csv(File) :-
    Sep = 59,                         % código ASCII del punto y coma
    Options = [separator(Sep)],        % opciones comunes de escritura
    setup_call_cleanup(
        open(File, write, Stream, [encoding(utf8)]),
        (
            % Cabecera
            csv_write_stream(Stream, [row(categoria,id,frase)], Options),
            % Frases del enunciado y de deportes
            escribir_serie(Stream, fe, 14, enunciado, Options),
            escribir_serie(Stream, f,  40, deportes,  Options)
        ),
        close(Stream)
    ).

%% escribir_serie(+Stream,+Prefijo,+Total,+Categoria,+Opts)
escribir_serie(Stream, Prefijo, Total, Categoria, Opts) :-
    forall(
        between(1, Total, N),
        (
            format(atom(PredAtom), '~w~d', [Prefijo, N]),
            Goal =.. [PredAtom, Frase],
            (   call(Goal)
            ->  csv_write_stream(Stream, [row(Categoria, N, Frase)], Opts)
            ;   true
            )
        )
    ).

%%--------------------------------------------------
%% Extraer sujeto/predicado de un término o/2 o de lista
%%--------------------------------------------------
sujeto(o(S, _), S) :- !.
sujeto([Primero|_], S) :- sujeto(Primero, S), !.

predicado(o(_, P), P) :- !.
predicado([Primero|_], P) :- predicado(Primero, P), !.

%%--------------------------------------------------
%% metricas_draw(+ArchivoCSV)
%%--------------------------------------------------
metricas_draw(File) :-
    % Leemos todas las filas, unificamos la primera con el header y quedamos sólo con Rows
    csv_read_file(File, [rec(categoria,id,frase)|Rows],
                  [separator(59), functor(rec), arity(3)]),
    % Imprimimos cabecera de salida
    format('Cat\tId\tCharsS\tCharsP\tTokS\tTokP~n'),
    % Recorremos sólo las filas de datos
    forall(
      member(rec(Cat,Id,SentAtom), Rows),
      (
        atom_string(SentAtom, Sentence),
        preprocesar_en(Sentence, Tokens),
        (   oracion(eng, Tree0, Tokens, [])
        ->  % el DCG puede devolver Tree0 como lista de o(...) o como un único o(...)
            ( is_list(Tree0) -> Tree0 = [Tree|_] ; Tree = Tree0 ),
            % extraemos sujeto/predicado
            sujeto(Tree, SujTree),
            predicado(Tree, PredTree),
            % calculamos #caracteres
            calcular_longitud_frase(SujTree, ChS),
            calcular_longitud_frase(PredTree, ChP),
            % calculamos #tokens
            with_output_to(atom(SujTxt),    imprimir_frase(SujTree)),
            split_string(SujTxt, " ", "", SL), length(SL, TokS),
            with_output_to(atom(PredTxt),   imprimir_frase(PredTree)),
            split_string(PredTxt, " ", "", PL), length(PL, TokP),
            % imprimimos la línea
            format('~w\t~w\t~d\t~d\t~d\t~d~n',
                   [Cat,Id,ChS,ChP,TokS,TokP])
        ;   % si no parsea, avisamos pero seguimos
            format('% Warning: no parse for id=~w: "~w"~n', [Id,Sentence])
        )
      )
    ).
