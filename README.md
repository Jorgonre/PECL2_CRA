No hemos sido capaces de solucionar el problema que teníamos a la hora de ejecutar los predicados preprocesar_en\2 y oracion\4 conjuntamente, por lo que aquí mostramos como se realizan ciertas ejeuciones.

- Para la ejecución de la mejora "Análisis sintáctico subrayado de frases simples":

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
