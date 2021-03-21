-- PD-Práctica 8.4 
-- Árboles Trie
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Un árbol Trie es un árbol de búsqueda donde los nodos internos codifican
-- un alfabeto de Claves y las hojas contienen Valores asociados
-- a las Claves. A continuación se muestra un ejemplo, donde las Claves
-- son cadenas de caracteres y los Valores son enteros.
--
--                              ""
--                             /  \
--                           "J"  "I"
--                            |     \
--                           "U"    "V"
--                           / \      \
--                         "A" "L"    "A"
--                         /     \      \
--                       "N"    "I"     "N"
--                        |      |       |
--                      68972   "A"     69712
--                              / \
--                          67321 62375
--
-- el árbol de ejemplo almacena los teléfonos de los siguientes contactos:
--  "JUAN" -> 68972, "JULIA" -> 67321, "JULIA" -> 62375, "IVAN" -> 69712
-- Nótese que hay dos nombres repetidos ("JULIA"). También nótese que las
-- claves se distribuyen en los nodos internos, de tal forma que cada nodo
-- tiene asociado tan solo un carácter en forma de cadena.
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el tipo de datos para un árbol Trie polimórfico,
-- donde los nodos internos almacenen un elemento de un tipo Clave y puedan
-- tener más de un hijo, y las hojas almacenen tan solo un Valor. El árbol
-- debe ser imprimible. Además, definir a continuación un sinónimo de árbol
-- Trie que emplee cadenas como Claves y enteros como Valores.



-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir las funciones siguientes:
--    (a) (arbolTrieVacio), que devuelva un árbol con solo el nodo raíz,
--         el cual tiene como clave la cadena vacía ("") y ningún hijo.
--    (b) (clave n), que devuelva la clave asociado al nodo n. Si n es
--         una hoja, devolver la cadena vacía "".
--    (c) (esHoja n), que indique con un booleano si el nodo n es una hoja.

arbolTrieVacio = undefined

clave = undefined

esHoja = undefined
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función (siguienteNodo hs s), que reciba una
-- lista de árboles as y una cadena de un solo carácter s, y devuelva un
-- par tal que:
--  1. El primer elemento del par será el nodo h de la lista as tal que su
--     clave coincida con s. Si tal nodo no existe, entonces será un nodo
--     nuevo con clave igual a s y sin hijos.
--  2. El segundo elemento del par serán todos los nodos de hs cuya clave no
--     coincidan con s.

siguienteNodo = undefined
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función (insertaEnArbol a p ), que reciba un
-- árbol Trie, a, y un par, p, con (clave, valor), siendo clave una cadena
-- de caracteres y valor un entero. La función debe devolver el árbol a
-- incluyendo el nuevo par (clave,valor).

insertaEnArbol = undefined
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función (insertaEnArbol a cs), que reciba un
-- árbol Trie, a, y una lista, cs, de pares (clave, valor), y devuelva un
-- árbol con todos los elementos insertados. Por ejemplo, lo siguiente
-- debería devolver el árbol ilustrado en el enunciado.
--    insertaEnArbol arbolTrieVacio
--        [("IVAN",69712),("JULIA",62375),("JULIA",67321),("JUAN",68972)]

insertaElemsEnArbol = undefined
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la (consultaValor a cs), tal que reciba un árbol
-- Trie a y una Clave cs, y devuelva los valores asociados a ella. Si la
-- clave no está en el árbol o no tiene asociados valores, devolver la
-- lista vacía.

consultaValor = undefined
-- ---------------------------------------------------------------------

