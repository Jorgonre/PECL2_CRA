:- use_module(library(csv)).

% Declaramos el predicado dinámico para almacenar cada fila del CSV.
% Cada fila se almacena como un hecho: verb_db/17.
:- dynamic verb_db/17.

% ------------------------------------------------------------------------------
% load_verb_db(+FilePath)
%
% Carga el archivo CSV y crea hechos verb_db/17.
% Se asumen 17 columnas, y se usa skip_header(true) para omitir la cabecera.

% ------------------------------------------------------------------------------
load_verb_db(FilePath) :-
    csv_read_file(FilePath, Rows,
                        [ separator(59), 
        functor(verb_db),
        arity(17),
        skip_header(true)
        ]),
    maplist(assertz, Rows).

% ------------------------------------------------------------------------------
% conjugado(+Infinitive, +Mood, +Tense, +PersonCode, -Conjugated)
%
% Devuelve conjugada la forma verbal en español para la persona especificada,
%
% Ejemplo de uso:
%   ?- conjugado('abandonar', 'Indicativo', 'Presente', '1s', Form).
%   Form = 'abandono'.
% ------------------------------------------------------------------------------
conjugado(Infinitive, Mood, Tense, PersonCode, Conjugado) :-
    verb_db(Infinitive, _InfEng,        % Columna 1,2 (infinitivo en español, inglés)
            Mood,       _MoodEng,         % Columna 3,4 (modo)
            Tense,      _TenseEng,        % Columna 5,6 (tiempo)
            _VerbEng,                     % Columna 7 (verbo en inglés)
            Form_1s, Form_2s, Form_3s,     % Columnas 8,9,10
            Form_1p, Form_2p, Form_3p,     % Columnas 11,12,13
            _Gerund, _GerundEng,          % Columnas 14,15 (gerundio)
            _PastPart, _PastPartEng),     % Columnas 16,17 (participio pasado)
    ( PersonCode = '1s' -> Conjugado = Form_1s;
      PersonCode = '2s' -> Conjugado = Form_2s;
      PersonCode = '3s' -> Conjugado = Form_3s;
      PersonCode = '1p' -> Conjugado = Form_1p;
      PersonCode = '2p' -> Conjugado = Form_2p;
      PersonCode = '3p' -> Conjugado = Form_3p ).


% ------------------------------------------------------------------------------
% conjugate_en_to_es(+PartialInfEng, +MoodEn, +TenseEn, +PersonCode, -SpanishConjugated)
%
% Permite buscar y obtener la forma conjugada en español utilizando una subcadena
% parcial del campo "infinitive_english". Por ejemplo, solo "give up".
%
% Ejemplo de uso:
%   ?- conjugate_en_to_es('give up', 'Indicative', 'Present', '1s', SpanishForm).
%   SpanishForm = 'abandono'.
%
% Se utiliza sub_string/5 para comprobar que PartialInfEng es una subcadena de la
% cadena completa almacenada.
% ------------------------------------------------------------------------------
conjugate_en_to_es(PartialInfEng, MoodEn, TenseEn, PersonCode, SpanishConjugated) :-
    verb_db(_Inf, FullInfEng,           % Columna 2: infinitivo en inglés
            _Mood, MoodEn,             % Columna 4: modo en inglés
            _Tense, TenseEn,           % Columna 6: tiempo en inglés
            _VerbEng,                  % Columna 7: verbo en inglés
            Form_1s, Form_2s, Form_3s,  % Columnas 8,9,10
            Form_1p, Form_2p, Form_3p,  % Columnas 11,12,13
            _Gerund, _GerundEng,       % Columnas 14,15
            _PastPart, _PastPartEng),  % Columnas 16,17
    sub_string(FullInfEng, _, _, _, PartialInfEng),
    ( PersonCode = '1s' -> SpanishConjugated = Form_1s;
      PersonCode = '2s' -> SpanishConjugated = Form_2s;
      PersonCode = '3s' -> SpanishConjugated = Form_3s;
      PersonCode = '1p' -> SpanishConjugated = Form_1p;
      PersonCode = '2p' -> SpanishConjugated = Form_2p;
      PersonCode = '3p' -> SpanishConjugated = Form_3p ).




/*
===============================================================================
 EJEMPLO DE USO
===============================================================================

1. Cargar la base de datos desde la ruta:
   ?- load_verb_db('spaninglishcsv.csv').
   true.

2. Consultar la forma conjugada de "abandonar" en Indicativo, Presente, persona 1s:
   ?- conjugate('abandonar', 'Indicativo', 'Presente', '1s', Form).
   Form = 'abandono'.

3. Para futuras consultas asegúrate de que los valores (como 'Indicativo' y 'Presente')
   coincidan exactamente con los que aparecen en el CSV (revisa mayúsculas, tildes, etc.).
===============================================================================
*/


/*
===============================================================================
 EJEMPLO DE USO
===============================================================================

1. Cargar la base de datos (asegúrate de que la ruta es correcta):

   ?- load_verb_db('spaninglishcsv.csv').
   true.

2. Búsqueda con coincidencia completa (como antes):
   
   ?- conjugate_en_to_es('to abandon, leave behind, desert; to quit, give up', 'Indicative', 'Present', '1s', SpanishForm).
   SpanishForm = 'abandono'.

3. Búsqueda con coincidencia parcial:
   Por ejemplo, buscando sólo con "give up" (asegúrate de respetar mayúsculas y espacios):

   ?- conjugate_en_to_es_partial('give up', 'Indicative', 'Present', '1s', SpanishForm).
   SpanishForm = 'abandono'.

===============================================================================
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        FLEXIÓN DE PLURALES                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pluralize(+Singular, -Plural)
%
% Dada una palabra en singular (como átomo), unifica Plural con la forma plural
% según las siguientes reglas:
%
%   1. Si la palabra termina en vocal, se añade "s".
%   2. Si la palabra termina en "z", se elimina la "z" y se añade "ces".
%   3. En cualquier otro caso, se añade "es".
%
% Ejemplo:
%   ?- pluralize('casa', P).  % P = 'casas'

pluralize(Singular, Plural) :-
    atom_chars(Singular, Chars),
    (   % Caso: termina en "z"
        last(Chars, 'z')
    ->  append(BaseChars, ['z'], Chars),
        atom_chars(Base, BaseChars),
        atom_concat(Base, 'ces', Plural)
    ;   % Caso: termina en vocal
        last(Chars, LastChar),
        is_vowel(LastChar)
    ->  atom_concat(Singular, 's', Plural)
    ;   % Caso general
        atom_concat(Singular, 'es', Plural)
    ).

% singularize(+Plural, -Singular)
%
% Dada una palabra en plural (como átomo), unifica Singular con la forma singular
% según la inversión de las reglas de pluralización:
%
%   1. Si la palabra termina en "ces", se elimina ese sufijo y se añade "z".
%   2. Si la palabra termina en "s" y la letra inmediatamente anterior es vocal,
%      se elimina la "s".
%   3. Si la palabra termina en "es", se elimina ese sufijo.
%
% Ejemplo:
%   ?- singularize('casas', S).   % S = 'casa'

singularize(Plural, Singular) :-
    atom_chars(Plural, Chars),
    (   % Caso: termina en "ces" -> originalmente terminaba en "z"
        append(BaseChars, ['c','e','s'], Chars)
    ->  atom_chars(Base, BaseChars),
        atom_concat(Base, 'z', Singular)
    ;   % Caso: termina en "s" y la penúltima letra es vocal (regla de pluralización de vocal + "s")
        append(BaseChars, [V, 's'], Chars),
        is_vowel(V)
    ->  atom_chars(Singular, BaseChars)
    ;   % Caso: termina en "es" -> eliminar "es"
        append(BaseChars, ['e','s'], Chars)
    ->  atom_chars(Singular, BaseChars)
    ;   % Si no cumple ningún patrón, se deja igual.
        Singular = Plural
    ).

% is_vowel(+Char)
%
% Verifica que Char sea una vocal (minúscula o mayúscula).
is_vowel(Char) :-
    member(Char, ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']).


% Predicado auxiliar para identificar vocales (mayúsculas y minúsculas).
is_vowel(Char) :-
    member(Char, ['a', 'e', 'i', 'o', 'u', 'A', 'E', 'I', 'O', 'U']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    PLURALIZACIÓN DE PALABRAS (INGLÉS)                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pluralize_en(+Singular, -Plural)
%
% Dada una palabra en singular (como átomo), unifica Plural con la forma
% plural en inglés, siguiendo reglas simplificadas.
pluralize_en(Singular, Plural) :-
    atom_chars(Singular, Chars),
    (   % Regla 1: termina en "ch" o "sh"
        (append(_, ['c','h'], Chars); append(_, ['s','h'], Chars))
    ->  atom_concat(Singular, 'es', Plural)
    ;   % Regla 2: termina en s, x o z
        (last(Chars, L), member(L, ['s', 'x', 'z']))
    ->  atom_concat(Singular, 'es', Plural)
    ;   % Regla 3: termina en "y"
        append(Rest, ['y'], Chars)
    ->  (   Rest \= [],
            last(Rest, Prev),
            (   is_vowel(Prev)
            ->  atom_concat(Singular, 's', Plural)
            ;   atom_chars(Base, Rest),
                atom_concat(Base, 'ies', Plural)
            )
        )
    ;   % Regla 4: termina en "fe"
        append(Base, ['f','e'], Chars)
    ->  atom_chars(BaseAtom, Base),
        atom_concat(BaseAtom, 'ves', Plural)
    ;   % Regla 5: termina en "f"
        append(Base, ['f'], Chars)
    ->  atom_chars(BaseAtom, Base),
        atom_concat(BaseAtom, 'ves', Plural)
    ;   % Regla general: se añade "s"
        atom_concat(Singular, 's', Plural)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      SINGULARIZACIÓN DE PALABRAS (INGLÉS)                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% singularize_en(+Plural, -Singular)
%
% Dada una palabra en plural (como átomo), unifica Singular con la forma
% singular en inglés, aplicando la inversión de las reglas.
singularize_en(Plural, Singular) :-
    atom_chars(Plural, Chars),
    (   % Si termina en "ies", se reemplaza por "y"
        append(Base, ['i','e','s'], Chars)
    ->  atom_chars(BaseAtom, Base),
        atom_concat(BaseAtom, 'y', Singular)
    ;   % Si termina en "ves", se asume (simplificadamente) que la forma singular termina en "f"
        append(Base, ['v','e','s'], Chars)
    ->  atom_chars(BaseAtom, Base),
        atom_concat(BaseAtom, 'f', Singular)
    ;   % Si termina en "es" y originalmente se agregó por reglas 1 o 2,
        % se quita "es"
        append(Base, ['e','s'], Chars)
    ->  atom_chars(BaseAtom, Base),
        Singular = BaseAtom
    ;   % Si termina en "s", se elimina la "s"
        append(Base, ['s'], Chars)
    ->  atom_chars(BaseAtom, Base),
        Singular = BaseAtom
    ;   Singular = Plural
    ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          EXCEPCIONES DE GÉNERO                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Excepciones para la conversión de masculino a femenino.
% Si la palabra es una de estas, se usará la forma indicada.
masc_to_fem_exception('actor',       'actriz').
masc_to_fem_exception('duque',       'duquesa').
masc_to_fem_exception('modelo',      'modelo').  % Invariable
masc_to_fem_exception('rey',         'reina').
masc_to_fem_exception('príncipe',    'princesa').
masc_to_fem_exception('alcalde',     'alcaldesa').
masc_to_fem_exception('jefe',        'jefa').
masc_to_fem_exception('trabajador',  'trabajadora').
masc_to_fem_exception('doctor',      'doctora').
masc_to_fem_exception('profesor',    'profesora').
masc_to_fem_exception('ingeniero',   'ingeniera').
masc_to_fem_exception('cocinero',    'cocinera').
masc_to_fem_exception('escritor',    'escritora').
masc_to_fem_exception('padrino',     'madrina').
masc_to_fem_exception('gobernador',  'gobernadora').
masc_to_fem_exception('ministro',    'ministra').
masc_to_fem_exception('señor',       'señora').
masc_to_fem_exception('embajador',   'embajadora').
masc_to_fem_exception('director',    'directora').
masc_to_fem_exception('esposo',      'esposa').
masc_to_fem_exception('bailarín',    'bailarina').
masc_to_fem_exception('emperador',    'emperatriz').
masc_to_fem_exception('zar',          'zarina').
masc_to_fem_exception('héroe',        'heroína').
masc_to_fem_exception('varón',        'hembra').
masc_to_fem_exception('yerno',        'nuera').
masc_to_fem_exception('marido',       'mujer').
masc_to_fem_exception('poeta',        'poetisa').
masc_to_fem_exception('profeta',      'profetisa').
masc_to_fem_exception('abad',         'abadesa').
masc_to_fem_exception('conde',        'condesa').
masc_to_fem_exception('barón',        'baronesa').
masc_to_fem_exception('macho',        'hembra').
masc_to_fem_exception('jabalí',       'jabalina').
masc_to_fem_exception('tigre',        'tigresa').
masc_to_fem_exception('caballo',      'yegua').
masc_to_fem_exception('carnero',      'oveja').
masc_to_fem_exception('toro',         'vaca').
masc_to_fem_exception('gallo',        'gallina').
masc_to_fem_exception('rey',          'reina').
masc_to_fem_exception('príncipe',     'princesa').
masc_to_fem_exception('papá',         'mamá').
masc_to_fem_exception('padre',        'madre').
masc_to_fem_exception('fray',         'sor').
masc_to_fem_exception('mago',         'maga').
masc_to_fem_exception('cura',         'monja').
masc_to_fem_exception('hombre',       'mujer').
masc_to_fem_exception('papa',         'papisa').
masc_to_fem_exception('infante',      'infanta').
masc_to_fem_exception('jinete',       'amazona').
masc_to_fem_exception('don',          'doña').
masc_to_fem_exception('sacerdote',    'sacerdotisa').
masc_to_fem_exception('juez',         'jueza').
masc_to_fem_exception('modisto',      'modista').
masc_to_fem_exception('soldado',      'soldado'). % Invariable
masc_to_fem_exception('testigo',      'testigo'). % Invariable
masc_to_fem_exception('colega',       'colega').  % Invariable
masc_to_fem_exception('artista',      'artista'). % Invariable
masc_to_fem_exception('víctima',      'víctima'). % Invariable
masc_to_fem_exception('personaje',    'personaje'). % Invariable
masc_to_fem_exception('genio',        'genio'). % Invariable
masc_to_fem_exception('casa',        'casa'). % Invariable
masc_to_fem_exception('cantante',     'cantante').  % Invariable
masc_to_fem_exception('estudiante',   'estudiante'). % Invariable
masc_to_fem_exception('guía',         'guía').       % Invariable
masc_to_fem_exception('joven',        'joven').      % Invariable
masc_to_fem_exception('piloto',       'piloto').     % Invariable
masc_to_fem_exception('agente',       'agente').     % Invariable
masc_to_fem_exception('representante','representante'). % Invariable
masc_to_fem_exception('atleta',       'atleta').     % Invariable
masc_to_fem_exception('gerente',      'gerente').    % Invariable
masc_to_fem_exception('periodista',   'periodista'). % Invariable
masc_to_fem_exception('astronauta',   'astronauta'). % Invariable
masc_to_fem_exception('especialista', 'especialista'). % Invariable
masc_to_fem_exception('policía',      'policía').    % Invariable
masc_to_fem_exception('líder',        'líder').      % Invariable
masc_to_fem_exception('amante',       'amante').     % Invariable
masc_to_fem_exception('artífice',     'artífice').   % Invariable
masc_to_fem_exception('intérprete',   'intérprete'). % Invariable
masc_to_fem_exception('chofer',       'chofer').     % Invariable
masc_to_fem_exception('fiscal',       'fiscal').     % Invariable
masc_to_fem_exception('maratonista',  'maratonista'). % Invariable
masc_to_fem_exception('dibujante',    'dibujante').  % Invariable
masc_to_fem_exception('recepcionista','recepcionista'). % Invariable
masc_to_fem_exception('consorte',     'consorte').   % Invariable
masc_to_fem_exception('acompañante',     'acompañante').   % Invariable
masc_to_fem_exception('analista',        'analista').      % Invariable
masc_to_fem_exception('aprendiz',        'aprendiz').      % Invariable
masc_to_fem_exception('auxiliar',        'auxiliar').      % Invariable
masc_to_fem_exception('cliente',         'cliente').       % Invariable
masc_to_fem_exception('comerciante',     'comerciante').   % Invariable
masc_to_fem_exception('cómplice',        'cómplice').      % Invariable
masc_to_fem_exception('conserje',        'conserje').      % Invariable
masc_to_fem_exception('contable',        'contable').      % Invariable
masc_to_fem_exception('dentista',        'dentista').      % Invariable
masc_to_fem_exception('estilista',       'estilista').     % Invariable
masc_to_fem_exception('guardia',         'guardia').       % Invariable
masc_to_fem_exception('hipócrita',       'hipócrita').     % Invariable
masc_to_fem_exception('individuo',       'individuo').     % Invariable
masc_to_fem_exception('militar',         'militar').       % Invariable
masc_to_fem_exception('oculista',        'oculista').      % Invariable
masc_to_fem_exception('paciente',        'paciente').      % Invariable
masc_to_fem_exception('pediatra',        'pediatra').      % Invariable
masc_to_fem_exception('personajillo',    'personajillo').  % Invariable
masc_to_fem_exception('profesional',     'profesional').   % Invariable
masc_to_fem_exception('protagonista',    'protagonista').  % Invariable
masc_to_fem_exception('psiquiatra',      'psiquiatra').    % Invariable
masc_to_fem_exception('publicista',      'publicista').    % Invariable
masc_to_fem_exception('sindicalista',    'sindicalista').  % Invariable
masc_to_fem_exception('socialista',      'socialista').    % Invariable
masc_to_fem_exception('suicida',         'suicida').       % Invariable
masc_to_fem_exception('turista',         'turista').       % Invariable
masc_to_fem_exception('visitante',       'visitante').     % Invariable
masc_to_fem_exception('atacante',        'atacante').      % Invariable
masc_to_fem_exception('vigilante',       'vigilante').     % Invariable
masc_to_fem_exception('belga',           'belga').         % Invariable
masc_to_fem_exception('árabe',           'árabe').         % Invariable
masc_to_fem_exception('canadiense',      'canadiense').    % Invariable
masc_to_fem_exception('estadounidense',  'estadounidense').% Invariable
masc_to_fem_exception('croata',          'croata').        % Invariable
masc_to_fem_exception('danés',           'danés').         % Invariable 
masc_to_fem_exception('deportista',      'deportista').    % Invariable
masc_to_fem_exception('ecologista',      'ecologista').    % Invariable
masc_to_fem_exception('futbolista',      'futbolista').    % Invariable
masc_to_fem_exception('pianista',        'pianista').      % Invariable
masc_to_fem_exception('guitarrista',     'guitarrista').   % Invariable
masc_to_fem_exception('humanista',       'humanista').     % Invariable
masc_to_fem_exception('idealista',       'idealista').     % Invariable
masc_to_fem_exception('idiota',          'idiota').        % Invariable
masc_to_fem_exception('optimista',       'optimista').     % Invariable
masc_to_fem_exception('pesimista',       'pesimista').     % Invariable
masc_to_fem_exception('artesano',        'artesano').      % En algunos 
masc_to_fem_exception('indígena',        'indígena').      % Invariable
masc_to_fem_exception('poliglota',       'poliglota').     % Invariable

% Excepciones para la conversión de femenino a masculino.
% Se definen las inversas de las anteriores.
fem_to_masc_exception('actriz',      'actor').
fem_to_masc_exception('duquesa',     'duque').
fem_to_masc_exception('modelo',      'modelo').
fem_to_masc_exception('reina',       'rey').
fem_to_masc_exception('princesa',    'príncipe').
fem_to_masc_exception('alcaldesa',   'alcalde').
fem_to_masc_exception('jefa',        'jefe').
fem_to_masc_exception('trabajadora', 'trabajador').
fem_to_masc_exception('doctora',     'doctor').
fem_to_masc_exception('profesora',   'profesor').
fem_to_masc_exception('cocinera',    'cocinero').
fem_to_masc_exception('escritora',   'escritor').
fem_to_masc_exception('madrina',     'padrino').
fem_to_masc_exception('gobernadora', 'gobernador').
fem_to_masc_exception('señora',      'señor').
fem_to_masc_exception('embajadora',  'embajador').
fem_to_masc_exception('directora',   'director').
fem_to_masc_exception('bailarina',   'bailarín').
fem_to_masc_exception('emperatriz',   'emperador').
fem_to_masc_exception('zarina',       'zar').
fem_to_masc_exception('heroína',      'héroe').
fem_to_masc_exception('hembra',       'varón').
fem_to_masc_exception('nuera',        'yerno').
fem_to_masc_exception('mujer',        'hombre').
fem_to_masc_exception('poetisa',      'poeta').
fem_to_masc_exception('profetisa',    'profeta').
fem_to_masc_exception('abadesa',      'abad').
fem_to_masc_exception('condesa',      'conde').
fem_to_masc_exception('baronesa',     'barón').
fem_to_masc_exception('jabalina',     'jabalí').
fem_to_masc_exception('tigresa',      'tigre').
fem_to_masc_exception('yegua',        'caballo').
fem_to_masc_exception('oveja',        'carnero').
fem_to_masc_exception('vaca',         'toro').
fem_to_masc_exception('gallina',      'gallo').
fem_to_masc_exception('reina',        'rey').
fem_to_masc_exception('princesa',     'príncipe').
fem_to_masc_exception('mamá',         'papá').
fem_to_masc_exception('madre',        'padre').
fem_to_masc_exception('sor',          'fray').
fem_to_masc_exception('monja',        'cura').
fem_to_masc_exception('papisa',       'papa').
fem_to_masc_exception('infanta',      'infante').
fem_to_masc_exception('amazona',      'jinete').
fem_to_masc_exception('doña',         'don').
fem_to_masc_exception('sacerdotisa',  'sacerdote').
fem_to_masc_exception('jueza',        'juez').
fem_to_masc_exception('modista',      'modisto').
fem_to_masc_exception('soldado',      'soldado'). % Invariable
fem_to_masc_exception('testigo',      'testigo'). % Invariable
fem_to_masc_exception('colega',       'colega').  % Invariable
fem_to_masc_exception('artista',      'artista'). % Invariable
fem_to_masc_exception('víctima',      'víctima'). % Invariable
fem_to_masc_exception('personaje',    'personaje'). % Invariable
fem_to_masc_exception('genio',        'genio'). % Invariable
fem_to_masc_exception('casa',        'casa'). % Invariable
fem_to_masc_exception('cantante',     'cantante').  % Invariable
fem_to_masc_exception('estudiante',   'estudiante'). % Invariable
fem_to_masc_exception('guía',         'guía').       % Invariable
fem_to_masc_exception('joven',        'joven').      % Invariable
fem_to_masc_exception('piloto',       'piloto').     % Invariable
fem_to_masc_exception('agente',       'agente').     % Invariable
fem_to_masc_exception('representante','representante'). % Invariable
fem_to_masc_exception('atleta',       'atleta').     % Invariable
fem_to_masc_exception('gerente',      'gerente').    % Invariable
fem_to_masc_exception('periodista',   'periodista'). % Invariable
fem_to_masc_exception('astronauta',   'astronauta'). % Invariable
fem_to_masc_exception('especialista', 'especialista'). % Invariable
fem_to_masc_exception('policía',      'policía').    % Invariable
fem_to_masc_exception('líder',        'líder').      % Invariable
fem_to_masc_exception('amante',       'amante').     % Invariable
fem_to_masc_exception('artífice',     'artífice').   % Invariable
fem_to_masc_exception('intérprete',   'intérprete'). % Invariable
fem_to_masc_exception('chofer',       'chofer').     % Invariable
fem_to_masc_exception('fiscal',       'fiscal').     % Invariable
fem_to_masc_exception('maratonista',  'maratonista'). % Invariable
fem_to_masc_exception('dibujante',    'dibujante').  % Invariable
fem_to_masc_exception('recepcionista','recepcionista'). % Invariable
fem_to_masc_exception('consorte',     'consorte').   % Invariable
% Inversas correspondientes (femenino ↔ masculino)
fem_to_masc_exception('acompañante',     'acompañante').   % Invariable
fem_to_masc_exception('analista',        'analista').      % Invariable
fem_to_masc_exception('aprendiz',        'aprendiz').      % Invariable
fem_to_masc_exception('auxiliar',        'auxiliar').      % Invariable
fem_to_masc_exception('cliente',         'cliente').       % Invariable
fem_to_masc_exception('comerciante',     'comerciante').   % Invariable
fem_to_masc_exception('cómplice',        'cómplice').      % Invariable
fem_to_masc_exception('conserje',        'conserje').      % Invariable
fem_to_masc_exception('contable',        'contable').      % Invariable
fem_to_masc_exception('dentista',        'dentista').      % Invariable
fem_to_masc_exception('estilista',       'estilista').     % Invariable
fem_to_masc_exception('guardia',         'guardia').       % Invariable
fem_to_masc_exception('hipócrita',       'hipócrita').     % Invariable
fem_to_masc_exception('individuo',       'individuo').     % Invariable
fem_to_masc_exception('militar',         'militar').       % Invariable
fem_to_masc_exception('oculista',        'oculista').      % Invariable
fem_to_masc_exception('paciente',        'paciente').      % Invariable
fem_to_masc_exception('pediatra',        'pediatra').      % Invariable
fem_to_masc_exception('personajillo',    'personajillo').  % Invariable
fem_to_masc_exception('profesional',     'profesional').   % Invariable
fem_to_masc_exception('protagonista',    'protagonista').  % Invariable
fem_to_masc_exception('psiquiatra',      'psiquiatra').    % Invariable
fem_to_masc_exception('publicista',      'publicista').    % Invariable
fem_to_masc_exception('sindicalista',    'sindicalista').  % Invariable
fem_to_masc_exception('socialista',      'socialista').    % Invariable
fem_to_masc_exception('suicida',         'suicida').       % Invariable
fem_to_masc_exception('turista',         'turista').       % Invariable
fem_to_masc_exception('visitante',       'visitante').     % Invariable
fem_to_masc_exception('atacante',        'atacante').      % Invariable
fem_to_masc_exception('vigilante',       'vigilante').     % Invariable
fem_to_masc_exception('belga',           'belga').         % Invariable
fem_to_masc_exception('árabe',           'árabe').         % Invariable
fem_to_masc_exception('canadiense',      'canadiense').    % Invariable
fem_to_masc_exception('estadounidense',  'estadounidense').% Invariable
fem_to_masc_exception('croata',          'croata').        % Invariable
fem_to_masc_exception('danés',           'danés').         % Invariable
fem_to_masc_exception('deportista',      'deportista').    % Invariable
fem_to_masc_exception('ecologista',      'ecologista').    % Invariable
fem_to_masc_exception('futbolista',      'futbolista').    % Invariable
fem_to_masc_exception('pianista',        'pianista').      % Invariable
fem_to_masc_exception('guitarrista',     'guitarrista').   % Invariable

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    CONVERSIÓN DE MASCULINO A FEMENINO                    %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% masculine_to_feminine(+Masculine, -Feminine)
%
% Dada una forma en masculino (como átomo), unifica Feminine con la forma
% correspondiente en femenino. El proceso es:
%
%   1. Se revisa si la palabra es una excepción mediante masc_to_fem_exception/2.
%      Si es el caso, se usa esa conversión y se corta la búsqueda.
%   2. Si no, se aplica la regla general: si la palabra termina en "o",
%      se reemplaza la "o" final por "a".
%   3. Si la palabra no termina en "o", se deja sin cambios.

masculine_to_feminine(Masculine, Feminine) :-
    masc_to_fem_exception(Masculine, Feminine), !.
masculine_to_feminine(Masculine, Feminine) :-
    atom_chars(Masculine, Chars),
    (   append(BaseChars, ['o'], Chars)
    ->  atom_chars(Base, BaseChars),
        atom_concat(Base, 'a', Feminine)
    ;   Feminine = Masculine
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    CONVERSIÓN DE FEMENINO A MASCULINO                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% feminine_to_masculine(+Feminine, -Masculine)
%
% Dada una forma en femenino (como átomo), unifica Masculine con la forma
% correspondiente en masculino. El procedimiento es:
%
%   1. Primero se revisa si la palabra se encuentra en las excepciones
%      definidas por fem_to_masc_exception/2; si es así, se utiliza ese valor.
%   2. Si no, se aplica la regla general: si la palabra termina en "a",
%      se reemplaza esa "a" final por "o".
%   3. Si no termina en "a", se mantiene la palabra sin cambios.

feminine_to_masculine(Feminine, Masculine) :-
    fem_to_masc_exception(Feminine, Masculine), !.
feminine_to_masculine(Feminine, Masculine) :-
    atom_chars(Feminine, Chars),
    (   append(BaseChars, ['a'], Chars)
    ->  atom_chars(Base, BaseChars),
        atom_concat(Base, 'o', Masculine)
    ;   Masculine = Feminine
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%            EXCEPCIONES DE GÉNERO (INGLÉS) – MASCULINO A FEMENINO           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Se definen 50 pares de excepciones en las que la forma femenina es diferente
% de la que se obtendría aplicando una regla simple.

masc_to_fem_exception_en('actor',        'actress').
masc_to_fem_exception_en('waiter',       'waitress').
masc_to_fem_exception_en('prince',       'princess').
masc_to_fem_exception_en('duke',         'duchess').
masc_to_fem_exception_en('king',         'queen').
masc_to_fem_exception_en('man',          'woman').
masc_to_fem_exception_en('hero',         'heroine').
masc_to_fem_exception_en('conductor',    'conductress').
masc_to_fem_exception_en('steward',      'stewardess').
masc_to_fem_exception_en('bachelor',     'bachelorette').
masc_to_fem_exception_en('boy',          'girl').
masc_to_fem_exception_en('husband',      'wife').
masc_to_fem_exception_en('uncle',        'aunt').
masc_to_fem_exception_en('nephew',       'niece').
masc_to_fem_exception_en('son',          'daughter').
masc_to_fem_exception_en('gentleman',    'lady').
masc_to_fem_exception_en('lord',         'lady').
masc_to_fem_exception_en('fiancé',       'fiancée').
masc_to_fem_exception_en('priest',       'priestess').
masc_to_fem_exception_en('baron',        'baroness').
masc_to_fem_exception_en('earl',         'countess').
masc_to_fem_exception_en('chairman',     'chairwoman').
masc_to_fem_exception_en('master',       'mistress').
masc_to_fem_exception_en('landlord',     'landlady').
masc_to_fem_exception_en('host',         'hostess').
masc_to_fem_exception_en('comedian',     'comedienne').
masc_to_fem_exception_en('cowboy',       'cowgirl').
masc_to_fem_exception_en('singer',       'songstress').
masc_to_fem_exception_en('poet',         'poetess').
masc_to_fem_exception_en('lion',         'lioness').
masc_to_fem_exception_en('tiger',        'tigress').
masc_to_fem_exception_en('patriarch',    'matriarch').
masc_to_fem_exception_en('salesman',     'saleswoman').
masc_to_fem_exception_en('spokesman',    'spokeswoman').
masc_to_fem_exception_en('Congressman',  'Congresswoman').
masc_to_fem_exception_en('businessman',  'businesswoman').
masc_to_fem_exception_en('policeman',    'policewoman').
masc_to_fem_exception_en('sportsman',    'sportswoman').
masc_to_fem_exception_en('auctioneer',   'auctioneress').
masc_to_fem_exception_en('baronet',      'baronetess').
masc_to_fem_exception_en('governor',     'governess').
masc_to_fem_exception_en('advisor',      'advisress').
masc_to_fem_exception_en('novelist',     'novelistess').
masc_to_fem_exception_en('author',       'authoress').
masc_to_fem_exception_en('presenter',    'presentress').
masc_to_fem_exception_en('director',     'directress').
masc_to_fem_exception_en('prosecutor',   'prosecutress').
masc_to_fem_exception_en('messenger',    'messengress').
masc_to_fem_exception_en('knight',       'dame').
masc_to_fem_exception_en('fireman',        'firewoman').
masc_to_fem_exception_en('mailman',        'mailwoman').
masc_to_fem_exception_en('postman',        'postwoman').
masc_to_fem_exception_en('cameraman',      'camerawoman').
masc_to_fem_exception_en('masseur',        'masseuse').
masc_to_fem_exception_en('impresario',     'impresaria').
masc_to_fem_exception_en('executor',       'executrix').
masc_to_fem_exception_en('virtuoso',       'virtuosa').
masc_to_fem_exception_en('editor',         'editress').
masc_to_fem_exception_en('barman',         'barmaid').
masc_to_fem_exception_en('chap',           'chick').
masc_to_fem_exception_en('fisherman',      'fisherwoman').
masc_to_fem_exception_en('anchorman',      'anchorwoman').
masc_to_fem_exception_en('doorman',        'doorwoman').
masc_to_fem_exception_en('gentleman',      'gentlewoman').
masc_to_fem_exception_en('monk',           'nun').
masc_to_fem_exception_en('postmaster',     'postmistress').
masc_to_fem_exception_en('milkman',        'milkmaid').
masc_to_fem_exception_en('usher',          'usherette').
masc_to_fem_exception_en('handyman',       'handywoman').
masc_to_fem_exception_en('groundsman',     'groundswoman').
masc_to_fem_exception_en('tailor',         'seamstress').
masc_to_fem_exception_en('newspaperman',   'newspaperwoman').
masc_to_fem_exception_en('bartender',      'barmaid').
masc_to_fem_exception_en('pageboy',        'pagegirl').
masc_to_fem_exception_en('bouncer',        'bounceress').
masc_to_fem_exception_en('weatherman',     'weatherwoman').
masc_to_fem_exception_en('miner',          'mineress').
masc_to_fem_exception_en('captain',        'captainess').
masc_to_fem_exception_en('commander',      'commandress').
masc_to_fem_exception_en('vintner',        'vintneress').
masc_to_fem_exception_en('winemaker',      'winemakeress').
masc_to_fem_exception_en('brewer',         'breweress').
masc_to_fem_exception_en('miller',         'milleress').
masc_to_fem_exception_en('butcher',        'butcheress').
masc_to_fem_exception_en('driver',         'driveress').
masc_to_fem_exception_en('porter',         'porteress').
masc_to_fem_exception_en('officer',        'officeress').
masc_to_fem_exception_en('detective',      'detectivess').
masc_to_fem_exception_en('reporter',       'reporteress').
masc_to_fem_exception_en('magistrate',     'magistratess').
masc_to_fem_exception_en('advocate',       'advocatress').
masc_to_fem_exception_en('lawyer',         'lawyress').
masc_to_fem_exception_en('historian',      'historicess').
masc_to_fem_exception_en('scholar',        'scolaress').
masc_to_fem_exception_en('bellhop',        'bellhopette').
masc_to_fem_exception_en('groundskeeper',  'groundskeepress').
masc_to_fem_exception_en('chauffeur',      'chauffeuse').
masc_to_fem_exception_en('footman',        'footmaid').
masc_to_fem_exception_en('salesman',       'saleswoman').




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           CONVERSIÓN DE MASCULINO A FEMENINO EN INGLÉS                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

masculine_to_feminine_en(Masc, Fem) :-
    masc_to_fem_exception_en(Masc, Fem), !.
masculine_to_feminine_en(Masc, Fem) :-
    % Si la palabra no está en la lista de excepciones, se deja sin cambio.
    Fem = Masc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%           CONVERSIÓN DE FEMENINO A MASCULINO EN INGLÉS                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Definimos el inverso utilizando las mismas excepciones.
feminine_to_masculine_en(Fem, Masc) :-
    masc_to_fem_exception_en(Masc, Fem), !.
feminine_to_masculine_en(Fem, Masc) :-
    Masc = Fem.


%% -------------------------------------------------------------------
%% Excepciones para artículos definidos e indefinidos (masculino → femenino)
%% -------------------------------------------------------------------
% artículos definidos
masc_to_fem_article('el',   'la').
masc_to_fem_article('los',  'las').
% artículos indefinidos
masc_to_fem_article('un',   'una').
masc_to_fem_article('unos', 'unas').

%% -------------------------------------------------------------------
%% Excepciones inversas (femenino → masculino)
%% -------------------------------------------------------------------
fem_to_masc_article('la',   'el').
fem_to_masc_article('las', 'los').
fem_to_masc_article('una', 'un').
fem_to_masc_article('unas','unos').

%% -------------------------------------------------------------------
%% generar_oracion/8
%%  Construye una oración flexionando artículo, sustantivo, adjetivo y verbo
%%
%%  Parámetros:
%%    + ArtMas: artículo masculino singular ('el' o 'un')
%%    + NomMas: sustantivo masculino singular
%%    + AdjMas: adjetivo masculino singular
%%    + Num: s (singular) o p (plural)
%%    + Gen: m (masculino) o f (femenino)
%%    + Inf: infinitivo del verbo ('ser', 'comer', …)
%%    + Pers: persona para conjugar ('1s','2p',…)
%%    - Oracion: átomo resultante, p.ej. 'la niña es alta'
%% -------------------------------------------------------------------
generar_oracion(ArtMas, NomMas, AdjMas, Num, Gen, Inf, Pers, Oracion) :-
    % 1) seleccionar artículo base según género
    ( Gen = f, masc_to_fem_article(ArtMas, ArtFem) ->
        ArtS = ArtFem
    ;   ArtS = ArtMas
    ),
    % 2) flexionar artículo al número
    ( Num = p, masc_to_fem_article(ArtS, ArtPl) ->
        Art = ArtPl
    ;   ( Num = p -> atom_concat(ArtS, 's', Art) ; Art = ArtS )
    ),

    % 3) convertir sustantivo al género y número
    ( Gen = f -> masculine_to_feminine(NomMas, NomGen) ; NomGen = NomMas ),
    ( Num = p  -> pluralize(NomGen, Nom)                ; Nom = NomGen ),

    % 4) convertir adjetivo al género y número
    ( Gen = f -> masculine_to_feminine(AdjMas, AdjGen) ; AdjGen = AdjMas ),
    ( Num = p  -> pluralize(AdjGen, Adj)               ; Adj = AdjGen ),

    % 5) conjugar el verbo (y concordar en número si es 'ser')
    ( Inf = 'ser', Num = p ->
        conjugado(Inf, 'Indicativo', 'Presente', '3p', Verbo)
    ;   conjugado(Inf, 'Indicativo', 'Presente', Pers, Verbo)
    ),

    % 6) ensamblar palabras con espacios
    atomic_list_concat([Art, Nom, Verbo, Adj], ' ', Oracion).
