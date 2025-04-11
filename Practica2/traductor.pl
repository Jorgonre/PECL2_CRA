:- module(traductor, [traducir/2]).

%% Diccionario de traduccion (palabra por palabra)
%% Nombres propios
traduccion_dic(juan, juan).
traduccion_dic(pedro, pedro).
traduccion_dic(javier, javier).
traduccion_dic(rafa, rafa).
traduccion_dic(aitana, aitana).
traduccion_dic(julian, julian).
traduccion_dic(miguel, miguel).
traduccion_dic(leo, leo).
traduccion_dic(marta, marta).
traduccion_dic(alex, alex).
traduccion_dic(marcos, marcos).
traduccion_dic(paula, paula).
traduccion_dic(sara, sara).
traduccion_dic(claudia, claudia).
traduccion_dic(daniel, daniel).
traduccion_dic(beatriz, beatriz).
traduccion_dic(maria, maria).
traduccion_dic(carlos, carlos).
traduccion_dic(andres, andres).
traduccion_dic(diego, diego).
traduccion_dic(lucas, lucas).
traduccion_dic(elena, elena).
traduccion_dic(sofia, sofia).
traduccion_dic(tomas, tomas).
traduccion_dic(laura, laura).
traduccion_dic(ana, ana).
traduccion_dic(luis, luis).
traduccion_dic(jaime, jaime).
traduccion_dic(natalia, natalia).
traduccion_dic(ruben, ruben).
traduccion_dic(ines, ines).
traduccion_dic(clara, clara).
traduccion_dic(alvaro, alvaro).
traduccion_dic(cristina, cristina).
traduccion_dic(david, david).
traduccion_dic(santiago, santiago).
traduccion_dic(jorge, jorge).
traduccion_dic(rocio, rocio).
traduccion_dic(angela, angela).
traduccion_dic(martina, martina).
traduccion_dic(lucia, lucia).
traduccion_dic(alberto, alberto).
traduccion_dic(teresa, teresa).
traduccion_dic(sergio, sergio).
traduccion_dic(alexis, alexis).
traduccion_dic(elisa, elisa).
traduccion_dic(pablo, pablo).
traduccion_dic(eduardo, eduardo).
traduccion_dic(ariana, ariana).
traduccion_dic(andrea, andrea).
traduccion_dic(oscar, oscar).

%% Articulos y determinantes
traduccion_dic(el, the).
traduccion_dic(la, the).
traduccion_dic(un, a).
traduccion_dic(una, a).
traduccion_dic(del, 'of the').
traduccion_dic(de, of).
traduccion_dic(al, '').  % se omite "al"
traduccion_dic(los, the).
traduccion_dic(las, the).

%% Pronombres personales y otros
traduccion_dic(yo, i).

%% Verbos (formas básicas)
traduccion_dic(gano, win).
traduccion_dic(juega, plays).
traduccion_dic(juegan, play).
traduccion_dic(practica, practices).
traduccion_dic(corre, runs).
traduccion_dic(entrena, trains).
traduccion_dic(forman, form).
traduccion_dic(compiten, compete).
traduccion_dic(seleccionado, selected).
traduccion_dic(ha, has).
traduccion_dic(es, is).
traduccion_dic(han, have).
traduccion_dic(ganado, won).
traduccion_dic(batido, broken).
traduccion_dic(sido, been).
traduccion_dic(elegido, elected).
traduccion_dic(subido, climbed).
traduccion_dic(obtenido, obtained).
traduccion_dic(entrenan, train).
traduccion_dic(disfrutan, enjoy).
traduccion_dic(preparan, 'are preparing').
traduccion_dic(participan, participate).
traduccion_dic(participado, participated).
traduccion_dic(terminar, finish).
traduccion_dic(logrado, achieved).

%% Adjetivos y adverbios
traduccion_dic(futbol, football).
traduccion_dic(baloncesto, basketball).
traduccion_dic(gran, great).
traduccion_dic(mejor, best).
traduccion_dic(nadadora, swimmer).
traduccion_dic(medalla, medal).
traduccion_dic(oro, gold).
traduccion_dic(sincronizada, synchronised).
traduccion_dic(experto, expert).
traduccion_dic(marciales, 'martial arts').
traduccion_dic(varios, several).
traduccion_dic(torneos, tournaments).
traduccion_dic(maratones, marathons).
traduccion_dic(dias, days).
traduccion_dic(capitanas, captains).
traduccion_dic(record, record).
traduccion_dic(salto, jump).
traduccion_dic(longitud, 'long jump').
traduccion_dic(categoria, category).
traduccion_dic(nacional, national).
traduccion_dic(competicion, competition).
traduccion_dic(internacional, international).
traduccion_dic(proxima, next).
traduccion_dic(carrera, race).
traduccion_dic(ciclismo, cycling).
traduccion_dic(tiro, archery).
traduccion_dic(hockey, hockey).
traduccion_dic(ajedrez, chess).
traduccion_dic(escuela, school).
traduccion_dic(campeonato, championship).
traduccion_dic(badminton, badminton).
traduccion_dic(lucha, wrestling).
traduccion_dic(relevos, relays).
traduccion_dic(judo, judo).
traduccion_dic(negro, black).
traduccion_dic(destreza, dexterity).
traduccion_dic(snowboard, snowboarding).
traduccion_dic(cinturon, belt).

%% Sustantivos y terminos deportivos
traduccion_dic(seleccion, team).
traduccion_dic(competidor, competitor).
traduccion_dic(competicion, competition).
traduccion_dic(participante, participant).
traduccion_dic(porteros, goalkeepers).
traduccion_dic(equipo, team).
traduccion_dic(roland-garros, roland-garros).
traduccion_dic(balon, ball).
traduccion_dic(jugador, player).
traduccion_dic(club, club).
traduccion_dic(tenis, tennis).
traduccion_dic(padel, 'paddle tennis').
traduccion_dic(natacion, swimming).
traduccion_dic(gimnasia, gymnastics).
traduccion_dic(ritmica, rhythmic).
traduccion_dic(artes, arts).
traduccion_dic(balonmano, handball).
traduccion_dic(triatlon, triathlon).
traduccion_dic(atletismo, athletics).
traduccion_dic(levantamiento, weightlifting).
traduccion_dic(pesas, weights).
traduccion_dic(kayak, kayaking).
traduccion_dic(aguas, waters).
traduccion_dic(rapidas, rapid).
traduccion_dic(estilo, freestyle).
traduccion_dic('ping-pong', 'ping-pong').
traduccion_dic(esqui, skiing).
traduccion_dic(titulo, title).
traduccion_dic(campeona, champion).
traduccion_dic(artistica, artistic).
traduccion_dic(parkour, parkour).
traduccion_dic(ciudad, city).
traduccion_dic(tiro, shooting).

%% Nuevas palabras de deporte (sustantivos adicionales)
traduccion_dic(voleibol, volleyball).
traduccion_dic(rugby, rugby).
traduccion_dic(boxeo, boxing).
traduccion_dic(esgrima, fencing).
traduccion_dic(motociclismo, motorcycling).
traduccion_dic(automovilismo, 'auto racing').
traduccion_dic(patinaje, skating).
traduccion_dic(softbol, softball).
traduccion_dic(beisbol, baseball).
traduccion_dic('futbol americano', 'american football').
traduccion_dic(golf, golf).
traduccion_dic(bowling, bowling).
traduccion_dic(motocross, motocross).
traduccion_dic('patinaje de velocidad', 'speed skating').
traduccion_dic(taekwondo, taekwondo).
traduccion_dic(karate, karate).

%% Verbos conjugados ampliados
%
% Verbo GANAR
traduccion_dic(gano, win).
traduccion_dic(ganas, win).
traduccion_dic(gana, wins).
traduccion_dic(ganamos, win).
traduccion_dic(ganan, win).
traduccion_dic(ganaba, won).
traduccion_dic(ganabas, won).
traduccion_dic(ganabamos, won).
traduccion_dic(ganaron, won).
traduccion_dic(ganare, will_win).
traduccion_dic(ganaria, would_win).

%
% Verbo JUGAR
traduccion_dic(juego, play).
traduccion_dic(juegas, play).
traduccion_dic(juega, plays).
traduccion_dic(jugamos, play).
traduccion_dic(juegan, play).
traduccion_dic(jugaba, played).
traduccion_dic(jugabas, played).
traduccion_dic(jugabamos, played).
traduccion_dic(jugaron, played).
traduccion_dic(jugare, will_play).
traduccion_dic(jugaria, would_play).

%
% Verbo CORRER
traduccion_dic(corro, run).
traduccion_dic(corras, run).
traduccion_dic(corre, runs).
traduccion_dic(corremos, run).
traduccion_dic(corren, run).
traduccion_dic(corria, ran).
traduccion_dic(corrias, ran).
traduccion_dic(corrabamos, ran).
traduccion_dic(correron, ran).
traduccion_dic(correre, will_run).
traduccion_dic(correria, would_run).

%
% Verbo ENTRENAR
traduccion_dic(entreno, train).
traduccion_dic(entrenas, train).
traduccion_dic(entrena, trains).
traduccion_dic(entrenamos, train).
traduccion_dic(entrenan, train).
traduccion_dic(entrenaba, trained).
traduccion_dic(entrenabas, trained).
traduccion_dic(entrenabamos, trained).
traduccion_dic(entrenaron, trained).
traduccion_dic(entrenare, will_train).
traduccion_dic(entrenaria, would_train).

%
% Verbo PRACTICAR
traduccion_dic(practico, practice).
traduccion_dic(practicas, practice).
traduccion_dic(practica, practices).
traduccion_dic(practicamos, practice).
traduccion_dic(practican, practice).
traduccion_dic(practicaba, practiced).
traduccion_dic(practicabas, practiced).
traduccion_dic(practicabamos, practiced).
traduccion_dic(practicaron, practiced).
traduccion_dic(practicare, will_practice).
traduccion_dic(practicaria, would_practice).

%
% Verbo COMPETIR
traduccion_dic(compito, compete).
traduccion_dic(compites, compete).
traduccion_dic(compite, competes).
traduccion_dic(competimos, compete).
traduccion_dic(compiten, compete).
traduccion_dic(competia, competed).
traduccion_dic(competias, competed).
traduccion_dic(competibamos, competed).
traduccion_dic(competieron, competed).
traduccion_dic(competire, will_compete).
traduccion_dic(competiria, would_compete).

%
% Verbo FORMAR
traduccion_dic(formo, form).
traduccion_dic(formas, form).
traduccion_dic(forma, forms).
traduccion_dic(formamos, form).
traduccion_dic(forman, form).
traduccion_dic(formaba, formed).
traduccion_dic(formabas, formed).
traduccion_dic(formabamos, formed).
traduccion_dic(formaron, formed).
traduccion_dic(formare, will_form).
traduccion_dic(formaria, would_form).

%
% Verbo PARTICIPAR
traduccion_dic(participo, participate).
traduccion_dic(participas, participate).
traduccion_dic(participa, participates).
traduccion_dic(participamos, participate).
traduccion_dic(participan, participate).
traduccion_dic(participaba, participated).
traduccion_dic(participabas, participated).
traduccion_dic(participabamos, participated).
traduccion_dic(participaron, participated).
traduccion_dic(participare, will_participate).
traduccion_dic(participaria, would_participate).

%
% Verbo TERMINAR
traduccion_dic(termino, finish).
traduccion_dic(terminas, finish).
traduccion_dic(termina, finishes).
traduccion_dic(terminamos, finish).
traduccion_dic(terminan, finish).
traduccion_dic(terminaba, finished).
traduccion_dic(terminabas, finished).
traduccion_dic(terminabamos, finished).
traduccion_dic(terminaron, finished).
traduccion_dic(terminare, will_finish).
traduccion_dic(terminaria, would_finish).

%
% Verbo LOGRAR
traduccion_dic(logro, achieve).
traduccion_dic(logras, achieve).
traduccion_dic(logra, achieves).
traduccion_dic(logramos, achieve).
traduccion_dic(logran, achieve).
traduccion_dic(lograba, achieved).
traduccion_dic(lograbas, achieved).
traduccion_dic(lograbamos, achieved).
traduccion_dic(lograron, achieved).
traduccion_dic(lograre, will_achieve).
traduccion_dic(lograria, would_achieve).

%% Conectores y otros
traduccion_dic(mientras, while).
traduccion_dic(y, and).
traduccion_dic(con, with).
traduccion_dic(en, in).
traduccion_dic(para, for).
traduccion_dic(juntos, together).
traduccion_dic(quien, who).
traduccion_dic(que, who).
traduccion_dic(muy, very).

%% traducir(+ListaEsp, -ListaEng)
%
% Traduce recursivamente cada token de la lista segun el diccionario.
% Si no se encuentra traduccion, deja la palabra igual.
% Si la traduccion es la cadena vacia (''), se omite ese token.
traducir([], []).
traducir([Palabra|Resto], Result) :-
    ( traduccion_dic(Palabra, Trans) ->
         ( Trans == '' ->
               traducir(Resto, Result)
         ;  Result = [Trans|RestResult],
            traducir(Resto, RestResult)
         )
    ;  Result = [Palabra|RestResult],
       traducir(Resto, RestResult)
    ).
