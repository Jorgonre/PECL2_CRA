Para la ejecución de la mejora "Análisis sintáctico subrayado de frases simples":

Llamar desde el fichero draw.pl a la función imprimir_frase_subrayada(Arbol).
  El árbol será una frase ya analizada y transformada para que contenga los complementos
  Ejemplos de árbol:
    o(g_nom_prop(n_p(jose)),gv(v(climbs),ccl(gp(prep(on))),ccl(gn(det(the),n(climbing_wall))),cct(gp(prep(in))),cct(gn(det(the),n(afternoons)))))
    o(g_nom_prop(n_p(jose)),gv(v(is,studying),cd(gn(n(philosophy)))))
    o(gn(det(the),n(mouse)),gv(v(was),ccm(gadj(adj(yellow)))))
