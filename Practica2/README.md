Para la flexion:
% Carga el módulo
[flexion].

% (Opcional) Cargar base de datos si tienes el CSV:
load_verb_db('spaninglishcsv.csv').

% Conjugar en español:
conjugado('abandonar', 'Indicativo', 'Presente', '1s', Form1s).
conjugado('abandonar', 'Indicativo', 'Presente', '3p', Form3p).

% Inglés → español:
conjugate_en_to_es('give up', 'Indicative', 'Present', '1s', Esp1s).

% Plurales y singulares (español):
pluralize('casa', P1).
pluralize('luz',  P2).
singularize('casas', S1).
singularize('luces', S2).

% Plurales y singulares (inglés):
pluralize_en('dish', P3).
pluralize_en('box',  P4).
singularize_en('dishes', S3).
singularize_en('boxes',  S4).

% Cambio de género (español):
masculine_to_feminine('actor', Fem1).
masculine_to_feminine('gato',  Fem2).
feminine_to_masculine('actriz', Masc1).
feminine_to_masculine('gata',   Masc2).

% Cambio de género (inglés):
masculine_to_feminine_en('waiter',   FemEn).
feminine_to_masculine_en('waitress', MascEn).

% Generar oraciones:
generar_oracion('el','niño','alto', s, m, 'ser',   '3s', Or1).
generar_oracion('la','niña','pequeño', p, f, 'ser','1p', Or2).