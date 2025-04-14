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
bilingual(maria, mary).
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

/* PRONOMBRES PERSONALES (ESPAÑOL -> INGLÉS) */
bilingual(yo, i).
bilingual(tu, you).
bilingual(el, he).
bilingual(ella, she).
bilingual(eso, it).
bilingual(nosotros, we).
bilingual(ellos, they).

/* PRONOMBRES POSESIVOS */
bilingual(mi, my).
bilingual(tuyo, your).
bilingual(su, his).
bilingual(su, her). 
bilingual(su, its). 
bilingual(nuestro, our).
bilingual(sus, their).

/* PRONOMBRES REFLEXIVOS Y OBJETO */
bilingual(me, me).
bilingual(te, you).
bilingual(lo, him).
bilingual(la, her).
bilingual(nos, us).
bilingual(los, them).

/* PRONOMBRES RELATIVOS */
bilingual(que, that).
bilingual(quien, who).
bilingual(cuyo, whose).
bilingual(cual, which). 

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

/* Sustantivos y términos deportivos */
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
bilingual(torneo, tournament).
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
bilingual(moreno, 'dark-skinned').
bilingual(moreno, darkskinned).
bilingual(estudiando, studying).
bilingual(filosofia, philosophy).
bilingual(derecho, law).
bilingual(recoge, clears).
bilingual(periodico, newspaper).
bilingual(cafe, coffee).
bilingual(zumo, juice).
bilingual(salta, skips).
bilingual(escala, climbs).
bilingual(escalada, climbing).
bilingual(pared, wall).
bilingual(tardes, afternoons).
bilingual(solo, only).
bilingual(manzanas, apples).
bilingual(rojas, red).
bilingual(procesador_de_textos, word_processor).
bilingual(potente, powerful).
bilingual(herramienta, tool).
bilingual(utilizado, used).
bilingual(escribir, write).
bilingual(documentos, documents).
bilingual(raton, mouse).
bilingual(gato, cat).
bilingual(atrapado, caught).
bilingual(gris, grey).
bilingual(hombre, man).
bilingual(vimos, saw).
bilingual(ayer, yesterday).
bilingual(vecino, neighbour).
bilingual(alto, tall).
bilingual(novela,novel).

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
bilingual(despues, after).
bilingual(antes, before).
bilingual(pero, but).
bilingual(aunque, although).

/* Verbos conjugados (GANAR, JUGAR, CORRER, ENTRENAR, PRACTICAR, COMPETIR, etc.) */
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

/* Conjugaciones de "beber" → "drink" */
bilingual(bebo, drink).
bilingual(bebes, drink).
bilingual(bebe, drinks).
bilingual(bebemos, drink).
bilingual(beben, drink).
bilingual(bebia, drank).
bilingual(bebias, drank).
bilingual(bebiamos, drank).
bilingual(bebian, drank).
bilingual(bebi, drank).
bilingual(bebiste, drank).
bilingual(bebimos, drank).
bilingual(bebieron, drank).
bilingual(bebere, will_drink).
bilingual(beberia, would_drink).

/* Conjugaciones de "leer" → "read" */
bilingual(leo, read).
bilingual(lees, read).
bilingual(lee, reads).
bilingual(leemos, read).
bilingual(leemos, read).
bilingual(leian, read).
bilingual(leia, read).
bilingual(leias, read).
bilingual(leiamos, read).
bilingual(leyeron, read).
bilingual(lei, read).
bilingual(leiste, read).
bilingual(leimos, read).
bilingual(leere, will_read).
bilingual(leeria, would_read).

/* Conjugaciones de "comer" → "eat" */
bilingual(como, eat).
bilingual(comes, eat).
bilingual(come, eats).
bilingual(comemos, eat).
bilingual(comen, eat).
bilingual(comia, ate).
bilingual(comias, ate).
bilingual(comiamos, ate).
bilingual(comian, ate).
bilingual(comi, ate).
bilingual(comiste, ate).
bilingual(comimos, ate).
bilingual(comieron, ate).
bilingual(comere, will_eat).
bilingual(comeria, would_eat).

/* Conjugaciones de "preferir" → "prefer" */
bilingual(prefiero, prefer).
bilingual(prefieres, prefer).
bilingual(prefiere, prefers).
bilingual(preferimos, prefer).
bilingual(prefieren, prefer).
bilingual(preferia, preferred).
bilingual(preferias, preferred).
bilingual(preferiamos, preferred).
bilingual(preferian, preferred).
bilingual(prefiriendo, preferring).
bilingual(preferi, preferred).
bilingual(preferiste, preferred).
bilingual(preferimos, preferred).
bilingual(prefirieron, preferred).
bilingual(preferire, will_prefer).
bilingual(preferiria, would_prefer).


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
