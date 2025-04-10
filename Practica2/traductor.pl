:- module(traductor, [traducir/2]).

%% Diccionario de traducción (palabra por palabra)
% Nombres propios
traduccion_dic(juan, juan).
traduccion_dic(pedro, pedro).
traduccion_dic(javier, javier).
traduccion_dic(rafa, rafa).
traduccion_dic(aitana, aitana).
traduccion_dic('julían', julian).
traduccion_dic(miguel, miguel).
traduccion_dic(leo, leo).
traduccion_dic(marta, marta).
traduccion_dic('álex', alex).
traduccion_dic(marcos, marcos).
traduccion_dic(paula, paula).
traduccion_dic(sara, sara).
traduccion_dic(claudia, claudia).
traduccion_dic(daniel, daniel).
traduccion_dic(beatriz, beatriz).
traduccion_dic('maría', maria).
traduccion_dic(carlos, carlos).
traduccion_dic(andrés, andres).
traduccion_dic(diego, diego).
traduccion_dic(lucas, lucas).
traduccion_dic(elena, elena).
traduccion_dic(sofía, sofia).
traduccion_dic('tomás', tomas).
traduccion_dic(laura, laura).
traduccion_dic(ana, ana).
traduccion_dic(luis, luis).
traduccion_dic(jaime, jaime).
traduccion_dic(natalia, natalia).
traduccion_dic(rubén, ruben).
traduccion_dic(inés, ines).
traduccion_dic(clara, clara).
traduccion_dic('álvaro', alvaro).
traduccion_dic(cristina, cristina).
traduccion_dic(david, david).
traduccion_dic(santiago, santiago).
traduccion_dic(jorge, jorge).
traduccion_dic(rocío, rocio).
traduccion_dic('ángela', angela).
traduccion_dic(martina, martina).
traduccion_dic('lucía', lucia).
traduccion_dic(alberto, alberto).
traduccion_dic(teresa, teresa).
traduccion_dic(sergio, sergio).
traduccion_dic(alexis, alexis).
traduccion_dic(elisa, elisa).
traduccion_dic(pablo, pablo).
traduccion_dic(eduardo, eduardo).
traduccion_dic(ariana, ariana).
traduccion_dic(andrea, andrea).
traduccion_dic(óscar, oscar).

% Artículos y determinantes
traduccion_dic(el, the).
traduccion_dic(la, the).
traduccion_dic(un, a).
traduccion_dic(una, a).
traduccion_dic(del, 'of the').
traduccion_dic(de, of).
traduccion_dic(al, '').  % se omite "al"
traduccion_dic(los, the).
traduccion_dic(las, the).

% Pronombres personales y otros
traduccion_dic(yo, i).

% Verbos
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

% Adjetivos y adverbios
traduccion_dic(fútbol, football).
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
traduccion_dic(días, days).
traduccion_dic(capitanas, captains).
traduccion_dic(récord, record).
traduccion_dic(salto, jump).
traduccion_dic(longitud, 'long jump').
traduccion_dic(categoría, category).
traduccion_dic(nacional, national).
traduccion_dic(competición, competition).
traduccion_dic(internacional, international).
traduccion_dic(próxima, next).
traduccion_dic(carrera, race).
traduccion_dic(ciclismo, cycling).
traduccion_dic(tiro, archery).
traduccion_dic(hockey, hockey).
traduccion_dic(ajedrez, chess).
traduccion_dic(escuela, school).
traduccion_dic(campeonato, championship).
traduccion_dic(bádminton, badminton).
traduccion_dic(lucha, wrestling).
traduccion_dic(relevos, relays).
traduccion_dic(judo, judo).
traduccion_dic(negro, black).
traduccion_dic(destreza, dexterity).
traduccion_dic(snowboard, snowboarding).
traduccion_dic(cinturón, belt).

% Sustantivos y términos deportivos
traduccion_dic(selección, team).
traduccion_dic(competidor, competitor).
traduccion_dic(competición, competition).
traduccion_dic(participante, participant).
traduccion_dic(porteros, goalkeepers).
traduccion_dic(equipo, team).
traduccion_dic('roland-garrós', 'roland-garros').
traduccion_dic(balón, ball).
traduccion_dic(jugador, player).
traduccion_dic(club, club).
traduccion_dic(tenis, tennis).
traduccion_dic(pádel, 'paddle tennis').
traduccion_dic(natación, swimming).
traduccion_dic(gimnasia, gymnastics).
traduccion_dic(rítmica, rhythmic).
traduccion_dic(artes, arts).
traduccion_dic(balonmano, handball).
traduccion_dic(triatlón, triathlon).
traduccion_dic(atletismo, athletics).
traduccion_dic(levantamiento, weightlifting).
traduccion_dic(pesas, weights).
traduccion_dic(kayak, kayaking).
traduccion_dic(aguas, waters).
traduccion_dic(rápidas, rapid).
traduccion_dic(estilo, freestyle).
traduccion_dic('ping-pong', 'ping-pong').
traduccion_dic(esquí, skiing).
traduccion_dic(título, title).
traduccion_dic(campeona, champion).
traduccion_dic(artística, artistic).
traduccion_dic(parkour, parkour).
traduccion_dic(ciudad, city).
traduccion_dic(tiro, shooting).

% Conectores y otros
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
% Traduce recursivamente cada token de la lista según el diccionario.
% Si no se encuentra traducción, deja la palabra igual.
% Si la traducción es la cadena vacía (''), se omite ese token.
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
