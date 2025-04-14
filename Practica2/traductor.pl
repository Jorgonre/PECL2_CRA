:- module(traductor, [
    traducir_frase_es_en/2,
    traducir_frase_en_es/2
]).

/* 
  IMPORTA el preprocesador 
  (Debes tener "preprocesar.pl" con los predicados:
     preprocesar_es/2 y preprocesar_en/2
*/
:- use_module(preprocesar).  

/**************************************
 * BILINGUAL DICTIONARY (ES -> EN)
 **************************************/

%% Nombres propios
bilingual(juan, juan).
bilingual(pedro, pedro).
bilingual(javier, javier).
bilingual(rafa, rafa).
bilingual(aitana, aitana).
bilingual(julian, julian).
bilingual(miguel, miguel).
bilingual(leo, leo).
bilingual(marta, marta).
bilingual(alex, alex).
bilingual(marcos, marcos).
bilingual(paula, paula).
bilingual(sara, sara).
bilingual(claudia, claudia).
bilingual(daniel, daniel).
bilingual(beatriz, beatriz).
bilingual(maria, maria).
bilingual(carlos, carlos).
bilingual(andres, andres).
bilingual(diego, diego).
bilingual(lucas, lucas).
bilingual(elena, elena).
bilingual(sofia, sofia).
bilingual(tomas, tomas).
bilingual(laura, laura).
bilingual(ana, ana).
bilingual(luis, luis).
bilingual(jaime, jaime).
bilingual(natalia, natalia).
bilingual(ruben, ruben).
bilingual(ines, ines).
bilingual(clara, clara).
bilingual(alvaro, alvaro).
bilingual(cristina, cristina).
bilingual(david, david).
bilingual(santiago, santiago).
bilingual(jorge, jorge).
bilingual(rocio, rocio).
bilingual(angela, angela).
bilingual(martina, martina).
bilingual(lucia, lucia).
bilingual(alberto, alberto).
bilingual(teresa, teresa).
bilingual(sergio, sergio).
bilingual(alexis, alexis).
bilingual(elisa, elisa).
bilingual(pablo, pablo).
bilingual(eduardo, eduardo).
bilingual(ariana, ariana).
bilingual(andrea, andrea).
bilingual(oscar, oscar).

/* Artículos y determinantes */
bilingual(el, the).
bilingual(la, the).
bilingual(un, a).
bilingual(una, a).
bilingual(del, 'of the').
bilingual(de, of).
bilingual(al, '').
bilingual(los, the).
bilingual(las, the).

/* PRONOMBRES PERSONALES (INGLÉS -> ESPAÑOL) */
bilingual(i, yo).
bilingual(you, tu).
bilingual(he, el).
bilingual(she, ella).
bilingual(it, eso).
bilingual(we, nosotros).
bilingual(they, ellos).

/* PRONOMBRES POSESIVOS */
bilingual(my, mi).
bilingual(your, tuyo).
bilingual(his, su).
bilingual(her, su).
bilingual(its, su).
bilingual(our, nuestro).
bilingual(their, su).

/* PRONOMBRES REFLEXIVOS Y OBJETO */
bilingual(me, me).
bilingual(you, te).
bilingual(him, lo).
bilingual(her, la).
bilingual(us, nos).
bilingual(them, los).

/* PRONOMBRES RELATIVOS */
bilingual(that, que).
bilingual(who, que).
bilingual(which, que).
bilingual(whom, quien).
bilingual(whose, cuyo).

/* POSESIVOS (ESPAÑOL -> INGLÉS) */
bilingual(mi, my).
bilingual(mis, my).
bilingual(tu, your).
bilingual(tus, your).
bilingual(su, his).     % también puede ser her, their...
bilingual(sus, their).
bilingual(nuestro, our).
bilingual(nuestros, our).
bilingual(vuestro, your).

/* ARTÍCULOS DEFINIDOS Y INDEFINIDOS */
bilingual(lo, it).
bilingual(la, the).
bilingual(le, it).

/* Verbos (formas básicas) */
bilingual(gano, win).
bilingual(juega, plays).
bilingual(juegan, play).
bilingual(practica, practices).
bilingual(corre, runs).
bilingual(entrena, trains).
bilingual(forman, form).
bilingual(compiten, compete).
bilingual(seleccionado, selected).
bilingual(ha, has).
bilingual(es, is).
bilingual(han, have).
bilingual(ganado, won).
bilingual(batido, broken).
bilingual(sido, been).
bilingual(elegido, elected).
bilingual(subido, climbed).
bilingual(obtenido, obtained).
bilingual(entrenan, train).
bilingual(disfrutan, enjoy).
bilingual(preparan, 'are preparing').
bilingual(participan, participate).
bilingual(participado, participated).
bilingual(terminar, finish).
bilingual(logrado, achieved).

/* Adjetivos y adverbios */
bilingual(futbol, football).
bilingual(baloncesto, basketball).
bilingual(gran, great).
bilingual(mejor, best).
bilingual(nadadora, swimmer).
bilingual(medalla, medal).
bilingual(oro, gold).
bilingual(sincronizada, synchronised).
bilingual(experto, expert).
bilingual(marciales, 'martial arts').
bilingual(varios, several).
bilingual(torneos, tournaments).
bilingual(maratones, marathons).
bilingual(dias, days).
bilingual(capitanas, captains).
bilingual(record, record).
bilingual(salto, jump).
bilingual(longitud, 'long jump').
bilingual(categoria, category).
bilingual(nacional, national).
bilingual(competicion, competition).
bilingual(internacional, international).
bilingual(proxima, next).
bilingual(carrera, race).
bilingual(ciclismo, cycling).
bilingual(tiro, archery).
bilingual(hockey, hockey).
bilingual(ajedrez, chess).
bilingual(escuela, school).
bilingual(campeonato, championship).
bilingual(badminton, badminton).
bilingual(lucha, wrestling).
bilingual(relevos, relays).
bilingual(judo, judo).
bilingual(negro, black).
bilingual(destreza, dexterity).
bilingual(snowboard, snowboarding).
bilingual(cinturon, belt).

/* Sustantivos y términos deportivos */
bilingual(seleccion, team).
bilingual(competidor, competitor).
bilingual(competicion, competition).
bilingual(participante, participant).
bilingual(porteros, goalkeepers).
bilingual(equipo, team).
bilingual('roland-garros', 'roland-garros').
bilingual(balon, ball).
bilingual(jugador, player).
bilingual(club, club).
bilingual(tenis, tennis).
bilingual(padel, 'paddle tennis').
bilingual(natacion, swimming).
bilingual(gimnasia, gymnastics).
bilingual(ritmica, rhythmic).
bilingual(artes, arts).
bilingual(balonmano, handball).
bilingual(triatlon, triathlon).
bilingual(atletismo, athletics).
bilingual(levantamiento, weightlifting).
bilingual(pesas, weights).
bilingual(kayak, kayaking).
bilingual(aguas, waters).
bilingual(rapidas, rapid).
bilingual(estilo, freestyle).
bilingual('ping-pong', 'ping-pong').
bilingual(esqui, skiing).
bilingual(titulo, title).
bilingual(campeona, champion).
bilingual(artistica, artistic).
bilingual(parkour, parkour).
bilingual(ciudad, city).
bilingual(tiro, shooting).

/* Nuevas palabras de deporte */
bilingual(voleibol, volleyball).
bilingual(rugby, rugby).
bilingual(boxeo, boxing).
bilingual(esgrima, fencing).
bilingual(motociclismo, motorcycling).
bilingual(automovilismo, 'auto racing').
bilingual(patinaje, skating).
bilingual(softbol, softball).
bilingual(beisbol, baseball).
bilingual('futbol americano', 'american football').
bilingual(golf, golf).
bilingual(bowling, bowling).
bilingual(motocross, motocross).
bilingual('patinaje de velocidad', 'speed skating').
bilingual(taekwondo, taekwondo).
bilingual(karate, karate).

/* Verbos conjugados ampliados (GANAR, JUGAR, CORRER, ENTRENAR, PRACTICAR, COMPETIR, etc.) */
bilingual(ganas, win).
bilingual(gana, wins).
bilingual(ganamos, win).
bilingual(ganan, win).
bilingual(ganaba, won).
bilingual(ganabas, won).
bilingual(ganabamos, won).
bilingual(ganaron, won).
bilingual(ganare, will_win).
bilingual(ganaria, would_win).

bilingual(juego, play).
bilingual(juegas, play).
bilingual(jugamos, play).
bilingual(jugaba, played).
bilingual(jugabas, played).
bilingual(jugabamos, played).
bilingual(jugaron, played).
bilingual(jugare, will_play).
bilingual(jugaria, would_play).

bilingual(corro, run).
bilingual(corras, run).
bilingual(corremos, run).
bilingual(corren, run).
bilingual(corria, ran).
bilingual(corrias, ran).
bilingual(corrabamos, ran).
bilingual(correron, ran).
bilingual(correre, will_run).
bilingual(correria, would_run).

bilingual(entreno, train).
bilingual(entrenas, train).
bilingual(entrenamos, train).
bilingual(entrenaba, trained).
bilingual(entrenabas, trained).
bilingual(entrenabamos, trained).
bilingual(entrenaron, trained).
bilingual(entrenare, will_train).
bilingual(entrenaria, would_train).

bilingual(practico, practice).
bilingual(practicas, practice).
bilingual(practicamos, practice).
bilingual(practicaba, practiced).
bilingual(practicabas, practiced).
bilingual(practicabamos, practiced).
bilingual(practicaron, practiced).
bilingual(practicare, will_practice).
bilingual(practicaria, would_practice).

bilingual(compito, compete).
bilingual(compites, compete).
bilingual(compite, competes).
bilingual(competimos, compete).
bilingual(competia, competed).
bilingual(competias, competed).
bilingual(competibamos, competed).
bilingual(competieron, competed).
bilingual(competire, will_compete).
bilingual(competiria, would_compete).

bilingual(formo, form).
bilingual(formas, form).
bilingual(forma, forms).
bilingual(formamos, form).
bilingual(formaba, formed).
bilingual(formabas, formed).
bilingual(formabamos, formed).
bilingual(formaron, formed).
bilingual(formare, will_form).
bilingual(formaria, would_form).

bilingual(participo, participate).
bilingual(participas, participate).
bilingual(participa, participates).
bilingual(participamos, participate).
bilingual(participaba, participated).
bilingual(participabas, participated).
bilingual(participabamos, participated).
bilingual(participaron, participated).
bilingual(participare, will_participate).
bilingual(participaria, would_participate).

bilingual(termino, finish).
bilingual(terminas, finish).
bilingual(termina, finishes).
bilingual(terminamos, finish).
bilingual(terminan, finish).
bilingual(terminaba, finished).
bilingual(terminabas, finished).
bilingual(terminabamos, finished).
bilingual(terminaron, finished).
bilingual(terminare, will_finish).
bilingual(terminaria, would_finish).

bilingual(logro, achieve).
bilingual(logras, achieve).
bilingual(logra, achieves).
bilingual(logramos, achieve).
bilingual(logran, achieve).
bilingual(lograba, achieved).
bilingual(lograbas, achieved).
bilingual(lograbamos, achieved).
bilingual(lograron, achieved).
bilingual(lograre, will_achieve).
bilingual(lograria, would_achieve).

/* Conectores */
bilingual(mientras, while).
bilingual(y, and).
bilingual(con, with).
bilingual(en, in).
bilingual(para, for).
bilingual(juntos, together).
bilingual(quien, who).
bilingual(que, who).
bilingual(muy, very).


/* Frases generales y descripciones */
bilingual(dark-skinned, 'dark-skinned').   % se traduce como "moreno" (preferiblemente usar "moreno")
bilingual(studying, studies).               % para "is studying", se traducirá palabra a palabra
bilingual(philosophy, filosofia).
bilingual(law, derecho).
bilingual(clears, recoge).  
bilingual(newspaper, periodico).
bilingual(coffee, cafe).
bilingual(juice, zumo).
bilingual(skips, salta).
bilingual(climbs, escala).
bilingual(climbing, escalada).
bilingual(wall, pared).
bilingual(afternoons, tardes).
bilingual(only, solo).  
bilingual(apples, manzanas).
bilingual(red, rojas).
bilingual(word_processor, procesador_de_textos).
bilingual(powerful, potente).
bilingual(tool, herramienta).
bilingual(used, utilizado).
bilingual(write, escribir).
bilingual(documents, documentos).
bilingual(mouse, raton).
bilingual(cat, gato).
bilingual(caught, atrapado).
bilingual(grey, gris).
bilingual(man, hombre).
bilingual(saw, vimos).
bilingual(yesterday, ayer).
bilingual(neighbour, vecino).

/* Palabras extra para deportes (y frases deportivas) */
bilingual(studies, estudia).
bilingual(drinks, bebe).
bilingual(reads, lee).
bilingual(sings, canta).
bilingual(jumps, salta).
bilingual(juice, zumo).  % ya agregado
bilingual(chips, 'patatas fritas').
bilingual(beer, cerveza).
bilingual(prefers, prefiere).
bilingual(clears, recoge).  % ya agregado
bilingual(cheers, anima).     % opcional
bilingual(consiguido, obtenido).
bilingual(juntas, together).   % para "juntas"
bilingual(couple, pareja).
bilingual(dance, baile).
bilingual(escalada, climbing).
bilingual(montanas, mountains).
bilingual(impresionantes, impressive).
bilingual(nombrada, named).
bilingual(juvenil, junior).
bilingual(personal, personal).
bilingual(saw, vimos).         % ya agregado
bilingual(caught, atrapado).   % ya agregado

/* Para deportes y competencias */
bilingual(coach, entrenador).
bilingual(competitor, competidor).

/* Palabras de transcripción para inglés (frases deportivas) */
bilingual(has, has).      % se traduce igual, pero para EN→ES se puede agregar inverso
bilingual(plays, plays).  % idem (preferiblemente, para EN->ES, "plays" debe traducirse a "juega")
bilingual(drinks, drinks).% idem
bilingual(eats, eats).    % idem

/* Extras de inglés para deporte (para que en EN->ES se realice la traducción): */
bilingual(football, futbol).
bilingual(basketball, baloncesto).
bilingual(goalkeepers, porteros).
bilingual(rugby, rugby).
bilingual(boxing, boxeo).
bilingual(fencing, esgrima).
bilingual(motorcycling, motociclismo).
bilingual('auto racing', automovilismo).
bilingual(skating, patinaje).
bilingual(softball, softbol).
bilingual(baseball, beisbol).
bilingual('american football', 'futbol americano').
bilingual(golf, golf).
bilingual(bowling, bowling).
bilingual(motocross, motocross).
bilingual('speed skating', 'patinaje de velocidad').
bilingual(taekwondo, taekwondo).
bilingual(karate, karate).

/* Extras en inglés para frases generales */
bilingual(studying, studies).
bilingual(clears, clears).      % ya traducido en ES->EN como "recoge"
bilingual(climbs, climbs).       % para EN->ES, "climbs" se traducirá a "escala"
bilingual(contains, contiene).   % opcional
bilingual(processor, procesador).
bilingual(powerful, powerful).   % para luego EN->ES, "powerful" se puede definir como "potente"
bilingual(used, used).
bilingual(write, write).
bilingual(documents, documents).

/* Palabras complementarias para EN -> ES */
bilingual(is, es).
bilingual(are, son).
bilingual(studying, estudia).    % inglés "studying" -> español "estudia"
bilingual(drinks, bebe).
bilingual(reads, lee).
bilingual(sings, canta).
bilingual(jumps, salta).
bilingual(plays, juega).
bilingual(competes, compiten).
bilingual(trains, entrena).
bilingual(wins, ganado).        % se usa para has won → ha ganado
bilingual(broken, batido).       % for "has broken" → ha batido
bilingual(clears, recoge).
bilingual(newspaper, periodico).
bilingual(juice, zumo).
bilingual(skips, salta).
bilingual(climbs, escala).
bilingual(climbing, escalada).
bilingual(wall, pared).
bilingual(afternoons, tardes).
bilingual(apples, manzanas).
bilingual(red, rojas).
bilingual(word_processor, procesador_de_textos).
bilingual(powerful, potente).
bilingual(tool, herramienta).
bilingual(used, utilizado).
bilingual(write, escribir).
bilingual(documents, documentos).
bilingual(mouse, raton).
bilingual(cat, gato).
bilingual(caught, atrapado).
bilingual(grey, gris).
bilingual(man, hombre).
bilingual(saw, vimos).
bilingual(yesterday, ayer).
bilingual(neighbour, vecino).

bilingual(coffee, cafe).
bilingual(philosophy, filosofia).
bilingual(law, derecho).

/* Extras deportivos en inglés -> español */
bilingual(football, futbol).
bilingual(basketball, baloncesto).
bilingual(goalkeepers, porteros).
bilingual(swimming, natacion).
bilingual(tennis, tenis).
bilingual('paddle tennis', 'padel').
bilingual(gymnastics, gimnasia).
bilingual(rhythmic, ritmica).
bilingual(handball, balonmano).
bilingual(triathlon, triatlon).
bilingual(athletics, atletismo).
bilingual(weightlifting, levantamiento).
bilingual(weights, pesas).
bilingual(kayaking, kayak).
bilingual(waters, aguas).
bilingual(rapid, rapidas).
bilingual(freestyle, estilo).
bilingual('ping-pong', 'ping-pong').
bilingual(skiing, esqui).
bilingual(title, titulo).
bilingual(championship, campeonado).
bilingual(badminton, badminton).
bilingual(wrestling, lucha).
bilingual(relays, relevos).
bilingual(judo, judo).
bilingual(belt, cinturon).
bilingual(volleyball, voleibol).
bilingual(rugby, rugby).
bilingual(boxing, boxeo).
bilingual(fencing, esgrima).
bilingual(motorcycling, motociclismo).
bilingual('auto racing', automovilismo).
bilingual(skating, patinaje).
bilingual(softball, softbol).
bilingual(baseball, beisbol).
bilingual('american football', 'futbol americano').
bilingual(golf, golf).
bilingual(bowling, bowling).
bilingual(motocross, motocross).
bilingual('speed skating', 'patinaje de velocidad').
bilingual(taekwondo, taekwondo).
bilingual(karate, karate).

bilingual(juntas, juntas).
bilingual(couple, pareja).
bilingual(dance, baile).

bilingual(climbing, escalada).
bilingual(mountains, montanas).
bilingual(impressive, impresionantes).
bilingual(named, nombrada).
bilingual(junior, juvenil).

bilingual(pareja, pareja).
bilingual(parkour, parkour).

bilingual(playa, playa).

bilingual(first, primer).
bilingual(podium, podio).

bilingual(daily, diariamente).
bilingual(pertiga, pertiga).

%----------------------------------------------------------
%       TRADUCCIÓN DE FRASES: ES->EN  /  EN->ES
%----------------------------------------------------------

%% traducir_frase_es_en(+OracionES, -OracionEN)
%  1) Preprocesa la frase en español
%  2) Traduce token a token
%  3) Devuelve string concatenado en inglés
traducir_frase_es_en(FraseEsp, FraseIng) :-
    preprocesar_es(FraseEsp, TokensES),
    traducir_es_en(TokensES, TokensEN),
    atomic_list_concat(TokensEN, ' ', FraseIng).

traducir_es_en([], []).
traducir_es_en([Es|ResEs], [En|ResEn]) :-
    ( bilingual(Es, En) ->
        true
    ; En = Es  % si no se encuentra en el diccionario, deja la palabra tal cual
    ),
    traducir_es_en(ResEs, ResEn).

%% traducir_frase_en_es(+OracionEN, -OracionES)
%  1) Preprocesa la frase en inglés
%  2) Traduce token a token
%  3) Devuelve string concatenado en español
traducir_frase_en_es(FraseEng, FraseEsp) :-
    preprocesar_en(FraseEng, TokensEN),
    traducir_en_es(TokensEN, TokensES),
    atomic_list_concat(TokensES, ' ', FraseEsp).

traducir_en_es([], []).
traducir_en_es([En|ResEn], [Es|ResEs]) :-
    ( bilingual(Es, En) ->
        true
    ; Es = En  % si no se encuentra
    ),
    traducir_en_es(ResEn, ResEs).
