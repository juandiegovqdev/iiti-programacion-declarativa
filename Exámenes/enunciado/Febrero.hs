-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Examen Febrero                                 1 de Febrero de 2019
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · 1. Antes de continuar, cambie el nombre de este archivo por:
--                   Febrero_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · 2. Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · 3. Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
--   4. Se recomienda entregar un fichero que cargue correctamente,
--   dejando comentado todo código con errores.
-- -------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified ColaConDosListas as C
import CodeWorld
import Data.Aeson
import GHC.Generics
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Text.CSV
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. [1 pto]
-- Definir la función largas tal que (largas xs) es la
-- lista de las palabras más largas de la lista xs. Por ejemplo,
--    largas ["no", "tengo", "claro", "que", "haga", "sol", "estos", "dias"]
--      == ["tengo", "claro", "estos"]
-- ---------------------------------------------------------------------

largas = undefined

-- -------------------------------------------------------------------
-- Ejercicio 2. [1 pto]
-- Desarrollar una funcion principal (juego) con un programa
-- interactivo basado en CodeWorld, que inicialmente pinte un círculo
-- rojo en la parte izquierda de la pantalla y un cuadrado verde
-- en la parte derecha.
-- A partir de ahí, si pulsamos en la flecha izquierda deberá aumentar
-- el tamaño del objeto de la izquierda y disminuir el de la derecha,
-- y si pulsamos la derecha todo lo contrario; en cualquiera de los
-- casos, si se supera un tamaño que haga que el objeto no quepa
-- en la pantalla, la acción debería dejar el estado tal cual.
-- -------------------------------------------------------------------

juego = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 3. [1,5 ptos]
-- Se considera la funcion aplicaNSats tal que (aplicaNSats n f ps xs)
-- es la lista obtenida aplicándole la función f a los n primeros
-- elementos de xs que cumplan alguno de los predicados de ps.
-- Por ejemplo:
--    aplicaNSats 4 (2+) [even,\x -> mod x 5 == 0]  [1..10]
--      ==  [4,6,7,8]
-- Se pide, definir la función de las tres formas siguientes:
-- 1. Usando listas por comprensión
-- 2. Usando funciones como map, filter, foldr, foldl, all, any, etc.
-- 3. Usando recursión
--
--    ** Nota: si no puede resolver este apartado,
--       intente al menos realizar una versión simplificada del problema,
--       y se valorará parcialmente el ejercicio.
-- ---------------------------------------------------------------------

aplicaNSats1 = undefined

aplicaNSats2 = undefined

aplicaNSats3 = undefined

-- -------------------------------------------------------------------
-- Ejercicio 4. [1 pto]
-- Definir la función extremosCola, tal que (extremosCola c) devuelve
-- un par de elementos con el mínimo y el máximo de la misma, tras
-- recorrerla una sola vez y sin pasar a lista. Si la cola tiene menos
-- de dos elementos debería devolver un error apropiado. Por ejemplo, 
--    extremosCola c4 == (-1,10)
--    extremosCola c7 == (1,20)
--    extremosCola c6 == (3,3)
-- -------------------------------------------------------------------

c1, c2, c3, c4, c5, c6, c7 :: C.Cola Int
c1 = foldr C.inserta C.vacia [1..20]
c2 = foldr C.inserta C.vacia [2,5..18]
c3 = foldr C.inserta C.vacia [3..10]
c4 = foldr C.inserta C.vacia [4,-1,7,3,8,10,0,3,3,4]
c5 = foldr C.inserta C.vacia [15..20]
c6 = foldr C.inserta C.vacia [3,3,3,3,3,3,3]
c7 = foldr C.inserta C.vacia ([1..10]++[20,19..10])

extremosCola = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. [1,5 ptos]
-- Se denomina matriz dispersa aquella cuyos elementos son 
-- mayoritariamente nulos. La representación de matrices dispersas se
-- suele comprimir en una forma densa, donde se dejan en las filas
-- tan solo los elementos no nulos dispuestos en un par (columna,valor).
-- Puesto que en algunas filas pueden haber más elementos que en otras,
-- aquellas filas con menos elementos se rellenan con (0,0.0).
-- A continuación se muestran dos ejemplos de una matriz dispersa y su
-- representación densa.

matrizEjDispersa :: Matrix Double
matrizEjDispersa = fromLists [[0.0,0.0,2.1],[1.6,0.0,-2.5],[0.5,0.0,0.0]]

matrizEjDensa :: Matrix (Int,Double)
matrizEjDensa = fromLists [[(3,2.1),(0,0.0)],[(1,1.6),(3,-2.5)],[(1,0.5),(0,0.0)]]

-- Ejercicio 5.1. Definir la función (coefDispersion m), tal que reciba
-- una matriz m en representación dispersa, y devuelva el coeficiente
-- de dispersión calculado como número de elementos nulos entre número
-- de no nulos. Por ejemplo,
--   coefDispersion matrizEjDispersa == 1.25

coefDispersion = undefined
     
-- Ejercicio 5.2. Definir la función (matrizDispersa m), que reciba
-- una matriz m en representación densa, y devuelva su correspondiente
-- en dispersa. Indicación: se puede emplear como número de columnas
-- la mayor columna que aparezca en los pares de la representación densa.
-- Por ejemplo,
--   matrizDispersa matrizEjDensa == matrizEjDispersa

matrizDispersa = undefined
                
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6. [2,5 ptos]
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

-- Ejercicio 6.1. Definir el tipo de datos para un árbol Trie polimórfico,
-- donde los nodos internos almacenen un elemento de un tipo Clave y puedan
-- tener más de un hijo, y las hojas almacenen tan solo un Valor. El árbol
-- debe ser imprimible. Además, definir a continuación un sinónimo de árbol
-- Trie que emplee cadenas como Claves y enteros como Valores.



-- Ejercicio 6.2. Definir las funciones siguientes:
--    (a) (arbolTrieVacio), que devuelva un árbol con solo el nodo raíz,
--         el cual tiene como clave la cadena vacía ("") y ningún hijo.
--    (b) (clave n), que devuelva la clave asociado al nodo n. Si n es
--         una hoja, devolver la cadena vacía "".
--    (c) (esHoja n), que indique con un booleano si el nodo n es una hoja.

arbolTrieVacio = undefined

clave = undefined

esHoja = undefined

-- Ejercicio 6.3. Definir la función (siguienteNodo hs s), que reciba una
-- lista de árboles as y una cadena de un solo carácter s, y devuelva un
-- par tal que:
--  1. El primer elemento del par será el nodo h de la lista as tal que su
--     clave coincida con s. Si tal nodo no existe, entonces será un nodo
--     nuevo con clave igual a s y sin hijos.
--  2. El segundo elemento del par serán todos los nodos de hs cuya clave no
--     coincidan con s.

siguienteNodo = undefined

-- Ejercicio 6.4. Definir la función (insertaEnArbol a p ), que reciba un
-- árbol Trie, a, y un par, p, con (clave, valor), siendo clave una cadena
-- de caracteres y valor un entero. La función debe devolver el árbol a
-- incluyendo el nuevo par (clave,valor).

insertaEnArbol = undefined

-- Ejercicio 6.5. Definir la función (insertaEnArbol a cs), que reciba un
-- árbol Trie, a, y una lista, cs, de pares (clave, valor), y devuelva un
-- árbol con todos los elementos insertados. Por ejemplo, lo siguiente
-- debería devolver el árbol ilustrado en el enunciado.
--    insertaEnArbol arbolTrieVacio
--        [("IVAN",69712),("JULIA",62375),("JULIA",67321),("JUAN",68972)]

insertaElemsEnArbol = undefined

-- Ejercicio 6.6. Definir la (consultaValor a cs), tal que reciba un árbol
-- Trie a y una Clave cs, y devuelva los valores asociados a ella. Si la
-- clave no está en el árbol o no tiene asociados valores, devolver la
-- lista vacía.

consultaValor = undefined

-- ---------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 7. [1,5 ptos]
-- Se desea representar una estructura que almacena datos sobre
-- categorías deportivas diferenciadas por la web de una importante
-- cadena especializada en deportes. En relación con esto:

-- Ejercicio 7.1. Defina, con sintaxis de registro, los tipos (nuevos
-- y sinónimos) adecuados para almacenar una estructura JSON como la
-- siguiente:
-- {
--   "dataList": [
--      {
--	 "id": 441,
--	 "attributes": {
--		"name": "Snow hiking",
--		"slug": "snow-hiking"
--	 }
--      },
--      ...
--    ]
-- }



-- Ejercicio 7.2. Realice un programa principal, (deportes), que:
--    a) Importe el archivo "sports.json" a un elemento
--       del tipo anterior
--    b) Imprima por pantalla un mensaje indicando el número
--       de deportes contenidos en la estructura,
--       calculado a partir de la misma. Debe quedar algo como:

--          Hay un total de 251 deportes

--    c) Para cada uno de los 10 primeros deportes,
--       imprima por pantalla su número de orden y su nombre,
--       seguido de su radio (la mitad de su diámetro):
--       Debe quedar algo como:

--          Los nombres de los 10 primeros son los siguientes:
--          1: "Sledding"
--          2: "Hiking"
--          3: "Snow hiking"
--          4: "Nordic skiing"
--          5: "Glacier hiking"
--          6: "Mountain hiking"
--          7: "Running"
--          8: "Skiing"
--          9: "Cycling"
--          10: "Horseback western riding"
--
--    ** Nota: si no puede resolver este apartado,
--       intente al menos realizar un tratamiento básico con
--       la estructura importada,
--       y se valorará parcialmente el ejercicio.
-- -------------------------------------------------------------------

deportes = undefined
