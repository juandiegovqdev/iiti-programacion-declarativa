-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Examen Septiembre                          12 de Septiembre de 2019
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · 1. Antes de continuar, cambie el nombre de este archivo por:
--                   Septiembre_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · 2. Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · 3. Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- · 4. Se recomienda entregar un fichero que cargue correctamente,
--   dejando comentado todo código con errores.
-- -------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import TADPila
import CodeWorld
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Text.CSV
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 ptos]
-- ---------------------------------------------------------------------
-- Se considera la función multFuncPrimerosNValidos
-- :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
-- tal que (multFuncPrimerosNValidos n f p xs) devuelve la suma de los
-- resultados de aplicar la función f a los primeros n elementos de xs
-- que cumplen el predicado p.
--
-- Por ejemplo:
--    multFuncPrimerosNValidos_1 2 (4+) even [1..7]  => 48
-- 
-- (Los dos primeros números pares de [1..7] son 2 y 4,
--  que al aplicarle (4+) quedan como 6 y 8, cuyo producto es 48)
--
-- Se pide definir la función:
-- 1. usando map y filter,
-- 2. por recursión,
-- 3. por recursión con acumulador,
-- 4. por plegado (a izquierda o derecha).
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. [1 pto]
-- ---------------------------------------------------------------------
-- Desarrolle una animación usando CodeWorld, de modo que la escena
-- incluya los ejes de coordenadas, un rectángulo inmóvil con grosor,
-- y un círculo relleno de otro color que vaya girando alrededor
-- del rectángulo estático.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]
-- ---------------------------------------------------------------------
-- Desarrolle un programa principal que lea del archivo pasado
-- como argumento (si no se le pasa ninguno, de "atp_players.csv"),
-- lo parsee y posteriormente realice lo siguiente con las filas válidas
-- delfichero:
--
-- a) Imprimir por pantalla el número de jugadores de que consta el archivo.
--    A continuación, imprimalos nombres de los campos contenidos en el
--    archivo, de la siguiente forma:
--
--    "ID" (campo 1)
--    "nombre" (campo 2)
--    ...
--
-- b) Procesar los registros contenidos, de forma quevaya seleccionando
--    aquellos que sean españoles (ESP), zurdos (L) y nacidos en los 80,
--    y para cada uno de ellos imprima el nombre, apellidos y
--    fecha de nacimiento.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. [1,5 ptos]
-- ---------------------------------------------------------------------
-- La sucesión generalizada de Fibonacci de grado N
-- (N >= 1) se construye comenzando con el número 1 y calculando el
-- resto de términos como la suma de los N términos anteriores (si
-- existen). Por ejemplo,
-- + la sucesión generalizada de Fibonacci de grado 2 es:
--   1, 1, 2, 3, 5, 8, 13, 21, 34, 55
-- + la sucesión generalizada de Fibonacci de grado 4 es:
--   1, 1, 2, 4, 8, 15, 29, 56, 108, 208
-- + la sucesión generalizada de Fibonacci de grado 6 es:
--   1, 1, 2, 4, 8, 16, 32, 63, 125, 248
-- Defina la función (fibPila n k), que devuelve una pila con los
-- términos de la sucesión de Fibonacci de grado n menores que k.
-- Se pide usar sólo el TAD de Pila (no se permite el uso de listas).
-- Por ejemplo:
-- λ> fibPila 6 100
--   63|32|16|8|4|2|1|1|-
-- λ> fibPila 6 200
--   125|63|32|16|8|4|2|1|1|-
-- λ> fibPila 4 200
--   108|56|29|15|8|4|2|1|1|-
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. [1,5 ptos]
-- ---------------------------------------------------------------------
-- Dada la siguiente definición de árbol mediante listas 
--
data Arbol = N Int [Arbol]
  deriving (Eq, Show)
--
-- defina la función (aumentaNiveles a), tal que expanda el árbol 'a'
-- añadiendo un nuevo hijo a cada nodo, cuyo valor es el resultado de
-- la suma de sus hermanos más el padre. A continuación se muestran
-- dos ejemplos, visualmente y evaluados en código:
--
-- Ej.1:    1         1          Ej.2:          1             1
--          |  ==>   / \                       / \    ==>    /|\
--          2       2   3                     2   3         2 3 6
--                  |                             |         | |\
--                  2                             4         2 4 7
--                                                            |
--                                                            4
-- Ej.1: λ> aumentaNiveles (N 1 [N 2 []])
--         N 1 [N 2 [N 2 []],N 3 []]
-- Ej.2: λ> aumentaNiveles (N 1 [N 2 [], N 3 [N 4 []]])
--         N 1 [N 2 [N 2 []],N 3 [N 4 [N 4 []],N 7 []],N 6 []]
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 6. [1 pto]
-- ---------------------------------------------------------------------
-- Definir las siguientes funciones y tipos de datos:
---   * Definir el tipo 'Binario', tal que nos permita representar un
--      número binario mediante enteros. Debe poder mostrarse por pantalla.
--      Ver los ejemplos de las funciones para conocer los constructores.
--    * Definir la función (int2binario n), por ejemplo:
--       λ> int2binario 1  ==   B 1 BFin
--       λ> int2binario 110 ==  B 1 (B 1 (B 0 BFin))
--       λ> int2binario 121 ==  *** Exception: El valor de entrada no es binario
--    * Definir la función (binario2int b), por ejemplo:
--       λ> binario2int (B 1 (B 1 (B 0 BFin))) == 110
--       λ> binario2int (B 0 (B 1 (B 0 BFin))) == 10
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 7. [1 pto]
-- ---------------------------------------------------------------------
-- Algunos algoritmos de compresión de imágenes hacen uso de los planos
-- de bits, o bitplanes. Sea una matriz m de números en binario, los
-- bitplanes son las matrices de bits correspondientes al bit n-ésimo de
-- cada elemento en m. Es decir, la matriz con el primer bit de todos los
-- elementos es el primer bitplane, la matriz con el segundo bit de todos
-- los elementos es el segundo bitplane, ... Supongamos una representación
-- little-endian (el bit menos significativo (en la posición 1) es el último,
-- el bit 2 es el antepenúltimo, etc.). Por ejemplo, los bitplanes de la matriz
--  ┌             ┐
--  │ 101   1  10 │
--  │   0  11 100 │
--  │  10 110   1 │
--  └             ┘
-- es la lista de matrices siguiente (del tercer bitplane al primero):
--  ┌       ┐   ┌       ┐   ┌       ┐
--  │ 1 0 0 │   │ 0 0 1 │   │ 1 1 0 │
--  │ 0 0 1 │   │ 0 1 0 │   │ 0 1 0 │
--  │ 0 1 0 │   │ 1 1 0 │   │ 0 0 1 │
--  └       ┘ , └       ┘ , └       ┘
--
-- Defina la función (bitplanes m), tal que recibe una matriz
-- de números en binario (por simplicidad de tipo Int, asuma que solo
-- contiene 0s y 1s), y devuelve una lista de matrices con los bitplanes
-- desde el más significativo (el '1' más significativo de todos los
-- elementos de la matriz) hasta el menos significativo. La siguiente
-- matriz de ejemplo corresponde al anterior.
matrizEj :: Matrix Int
matrizEj = fromLists [[101, 1, 10], [0, 11, 100], [10, 110, 1]]
-- ---------------------------------------------------------------------
