-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Examen Diciembre                                   17 de Diciembre de 2019
-- --------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- --------------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · 1. Antes de continuar, cambie el nombre de este archivo por:
--                   diciembre_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · 2. Por favor, entregue un fichero que cargue correctamente, dejando
--   comentado todo código con errores.
-- · 3. Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · 4. Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- --------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Default
import Data.Aeson
import GHC.Generics
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Data.Matrix
import TADPila


-- --------------------------------------------------------------------------
-- Ejercicio 1. [2 ptos]
-- --------------------------------------------------------------------------
-- La función extremosCumplen recibe un predicado p, una función f y
-- una lista de listas xss, y devuelve una lista de pares de elementos
-- tal que, para cada lista de la lista de listas original, devuelva el par de
-- elementos resultantes de aplicar la función indicada sobre el menor y
-- mayor elemento que cumpla el predicado. Veámoslo a través de ejemplos:

--  λ> extremosCumplen even (^2) [[1..5],[(-9)..(-1)]]
--     [(4,16),(64,4)]
--
--  λ> extremosCumplen (\x -> length x > 3)
--                     (take 3)
--                     [["paco","es","poco","pico"],
--                      ["tres", "tristes", "tigres"]]
--     [("pac","poc"),("tig","tri")]

-- En definitiva, se pide definir la función anterior usando
-- distintas técnicas como núcleo de su solución:
--  a) Listas por comprensión: extremosCumplenL
--  b) Recursión no final: extremosCumplenR
--  c) Recursión con acumulador: extremosCumplenR2
--  d) Funciones de orden superior distintas de plegado: extremosCumplenO
--  e) Plegado: extremosCumplenP

extremosCumplenL = undefined
extremosCumplenR = undefined
extremosCumplenR2 = undefined
extremosCumplenO = undefined
extremosCumplenP = undefined
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 2. [2 ptos]
-- --------------------------------------------------------------------------
-- 1. Defina, con sintaxis de registro, los tipos necesarios para almacenar
--    la información sobre personajes de Star Wars contenida en el
--    archivo personajes.json, ignorando aquellos datos que no son requeridos
--    para los apartados siguientes.
--
-- Nota: tenga en cuenta que los personajes se encuentran en un array dentro
--       de un campo de un objeto superior.
--
-- 2. Atribuya valores por defecto a los campos considerados.
--
-- 3. Realice un programa principal que:
--    a) Importe el archivo "personajes.json",
--    b) Indique el número total de personajes devuelto por la API
--    c) Para cada personaje, imprima por pantalla su nombre, altura, peso,
--       y número de películas en las que aparece
--    d) Indique el nombre y la estatura del personaje más alto devuelto.
-- -------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]
-- --------------------------------------------------------------------------
-- Un árbol binario se puede codificar asociando valores tan solo en las
-- hojas. Se pide,
--  a) Definir el tipo de dato algebraico del árbol binario polimórfico con
-- valores solo en las hojas. Este tipo de ser imprimible e igualable.

{--- Descomente este bloque una vez defina el tipo Arbol
ejar1 :: Arbol Int
ejar1 = (N (H 1) (N (N (H 1) (H 2)) (H 1)))
-}

--  b) Definir la función (elemNivel a x), tal que determine el nivel menos
--  profundo donde suceda x. La raíz cuenta como nivel 0, sus hijos el nivel 1,
--  etc. Esta función debe devolver un Maybe, ya que si el elemento no se
--  encuentra, devuelve Nothing. Si lo encuentra, devuelve simplemene el nivel.
--  Por ejemplo,
--    λ> ejar1
--       N (H 1) (N (N (H 1) (H 2)) (H 1))
--    λ> elemNivel ejar1 2
--       Just 3
--    λ> elemNivel ejar1 1
--       Just 1
--    λ> elemNivel ejar1 5
--       Nothing

elemNivel = undefined
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 4. [2 ptos]
-- --------------------------------------------------------------------------
-- En el problema de las torres de Hanoi se consideran tres varillas donde se
-- apilan discos de distinto tamaño. Una forma de codificar las varillas es
-- el emplear el tipo de dato abstracto pila con enteros, donde cada entero
-- indica el tamaño del disco (de menor a mayor)

ejp1, ejp2, ejp3 :: Pila Int
ejp1 = foldr apila vacia [4,5,7,10]
ejp2 = foldr apila vacia [1,2,3,6,8,9]
ejp3 = foldr apila vacia [4,5,1,2]

-- Se pide:
--  a) Definir la función (comprueba p), donde p es una pila que codifica una
--  varilla de discos, y compruebe si la varilla es correcta; es decir, que
--  ningún disco tenga por encima otro de mayor tamaño. Por ejemplo,
--   λ> comprueba ejp2
--      True
--   λ> comprueba ejp3
--      False

comprueba = undefined

--  b) Definir la función (transfiere n p1 p2), donde p1 y p2 son dos varillas
--  con discos, n es un número natural, y devuelva el resultado de apilar en p2
--  los primeros n discos de p1, manteniendo el orden; es decir, asume que se
--  puede coger más de un dico a la vez y moverlos a la segunda varilla. Por
--  ejemplo, 
-- λ> transfiere 3 ejp2 ejp1
-- 1|2|3|4|5|7|10|-
-- λ> transfiere 9 ejp2 ejp1
-- 1|2|3|6|8|9|4|5|7|10|-

transfiere = undefined
-- --------------------------------------------------------------------------


-- --------------------------------------------------------------------------
-- Ejercicio 5. [2 ptos]
-- --------------------------------------------------------------------------
-- Representamos el tablero del ajedrez con una matriz donde los elementos son
-- del tipo Pieza. Este tipo tiene como posibles valores: V, C, T, A, P, RY y
-- RA, que significan vacío, caballo, torre, alfil, peón, rey y reina,
-- respectivamente. Se pide:
--   a) Definir el nuevo tipo de datos Pieza para que los ejemplos ejm1 y ejm2
-- se puedan cargar correctamente una vez descomentados. Aplica las
-- derivaciones necesarias para poder utilizarlo en el resto de ejercicios.

{--- Descomente este bloque una vez defina el tipo Pieza
ejm1 :: Matrix Pieza
ejm1 = fromLists [[V, V, V, V],
                  [C, V, C, V],
                  [V, V, A, V],
                  [V,RY,RA, T]]

ejm2 :: Matrix Pieza
ejm2 = fromLists [[V, V, V, V],
                  [C, C, C,RY],
                  [V, V, P, C],
                  [V, V,RA, T]]
-}

--   b) Definir la función (jaquecaballo m), que compruebe si en el tablero
--   codificado en la matriz m existe algún caballo dando jaque al rey. Por
--   ejemplo,
--    λ> jaquecaballo ejm1
--       True
--    λ> jaquecaballo ejm2
--       False

jaquecaballo = undefined

-- --------------------------------------------------------------------------

