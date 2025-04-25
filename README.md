## No hemos sido capaces de solucionar el problema que teníamos a la hora de ejecutar los predicados preprocesar_en\2 y oracion\4 conjuntamente, por lo que aquí mostramos como se realizan ciertas ejecuciones.

### Ayuda para el uso del menu:

Por el problema que se menciona arriba para el empleo de la opción 3 (Analisis sintactico) del menú se debe introducir la frase tokenizada. Para llevar a cabo dicha tokenización hemos dispuesto dos formas. **INCISO IMPORTANTE, SE DEBEN COPIAR LOS CORCHETES TAMBIÉN**:
  1. Llamar previamente a la opción 1 (Preprocesar English) que ya nos devuelve entre corchetes los tokens necesarios. Habría que copiar el resultado
     devuelto y pegarlo en la entrada que espera la llamada de la opción 3 (Análisis sintáctico).
     Adjunto ejemplo gráfico:
       ![imagen](https://github.com/user-attachments/assets/e7a1e0f8-5dc2-412e-8b82-985750e7b5ed)

  3. Se ha dejado preparado también un archivo.csv (artificial.csv) que contiene todas las frases tokenizadas de la misma forma que las dejaría la opción
     1 (Preprocesar English). De nuevo el proceso sería copiarla y pegarla en la entrada que espera la opción 3(Análisis sintáctico).


### Para la ejecución de la mejora "Análisis sintáctico subrayado de frases simples":

Llamar desde el fichero draw.pl a la función imprimir_frase_subrayada(Arbol).
  El árbol será una frase ya analizada y transformada para que contenga los complementos
  Ejemplos de Arbol:
    o(g_nom_prop(n_p(jose)),gv(v(climbs),ccl(gp(prep(on))),ccl(gn(det(the),n(climbing_wall))),cct(gp(prep(in))),cct(gn(det(the),n(afternoons)))))
    o(g_nom_prop(n_p(jose)),gv(v(studies),cd(gn(n(philosophy)))))
    o(gn(det(the),n(mouse)),gv(v(was),ccm(gadj(adj(yellow)))))
  Si no se utiliza alguno de estos ejemplos y se quiere utilizar una frase nueva, se puede ejecutar de la siguiente forma tras hacer los consults correspondientes a los     
  ficheros preprocesar.pl y prueba_2.pl:
    1 ?- preprocesar_en('Frase que se desea probar', X)
    X = [Lista de tokens de la frase].
    2 ?- oracion(eng, X, [Lista de tokens de la frase], []),X=[Y], nl, transformar_frase_con_complementos(Y, FFinal), nl, imprimir_frase_subrayada(FFinal).

### Para la ejecución de la mejora de "Oraciones en japonés": 

Se debe llamar a un predicado oracion_japones desde dentro del archivo prueba_2.pl. A continuación adjunto un ejemplo de una llamada para esta mejora:
    oracion_japones(jpn, X, ["HARUKO", wa, hana, ga, suki, soushite, koohii, ga, kirai],[]).

### Para la mejora de la flexion:

Prerequisitos:
  Carga el módulo [flexion].
     (Opcional) Cargar base de datos si tienes el CSV: load_verb_db('spaninglishcsv.csv').

Características:
  - Conjugar en español: conjugado('abandonar', 'Indicativo', 'Presente', '1s', Form1s). conjugado('abandonar', 'Indicativo', 'Presente', '3p', Form3p).
  - Inglés → español: conjugate_en_to_es('give up', 'Indicative', 'Present', '1s', Esp1s).
  - Plurales y singulares (español): pluralize('casa', P1). pluralize('luz', P2). singularize('casas', S1). singularize('luces', S2).
  - Plurales y singulares (inglés): pluralize_en('dish', P3). pluralize_en('box', P4). singularize_en('dishes', S3). singularize_en('boxes', S4).
  - Cambio de género (español): masculine_to_feminine('actor', Fem1). masculine_to_feminine('gato', Fem2). feminine_to_masculine('actriz', Masc1).
    feminine_to_masculine('gata', Masc2).
  - Cambio de género (inglés): masculine_to_feminine_en('waiter', FemEn). feminine_to_masculine_en('waitress', MascEn).
    
Ejecución:
  Generar oraciones: generar_oracion('el','niño','alto', s, m, 'ser', '3s', Or1). generar_oracion('la','niña','pequeño', p, f, 'ser','1p', Or2).

