{-# LANGUAGE OverloadedStrings #-}

-- import TADPila
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Text.CSV
-- import Data.Matrix

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
-- ---------------------------------------------------------------------

-- 1. usando map y filter,
multFuncPrimerosNValidosMapFilter :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidosMapFilter n f p xs = sum (map f (take n (filter p xs)))
--    multFuncPrimerosNValidosMapFilter 2 (4+) even [1..7]  

-- 2. por recursión,
multFuncPrimerosNValidosRecursion :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidosRecursion n f p xs = multFuncPrimerosNValidosRecursionAux n f (multFuncPrimerosNValidosRecursionFiltro p xs)
--    multFuncPrimerosNValidosRecursion 2 (4+) even [1..7]

multFuncPrimerosNValidosRecursionAux :: (Num a, Num b) => Int -> (a -> b) -> [a] -> b
multFuncPrimerosNValidosRecursionAux n f (x:xs) = f x + multFuncPrimerosNValidosRecursionAux n f xs
multFuncPrimerosNValidosRecursionAux _ _ [] = 0

multFuncPrimerosNValidosRecursionFiltro :: (Num a) => (a -> Bool) -> [a]  -> [a]
multFuncPrimerosNValidosRecursionFiltro = filter

-- 3. por recursión con acumulador,
multFuncPrimerosNValidosAcumulador :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidosAcumulador n f p xs = undefined
--    multFuncPrimerosNValidosAcumulador 2 (4+) even [1..7] 

-- 4. por plegado (a izquierda o derecha).
multFuncPrimerosNValidosFoldr :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidosFoldr n f p xs = undefined
--    multFuncPrimerosNValidosFoldr 2 (4+) even [1..7] 

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]
-- ---------------------------------------------------------------------
-- Desarrolle un programa principal que lea del archivo pasado
-- como argumento (si no se le pasa ninguno, de "atp_players.csv"),
-- lo parsee y posteriormente realice lo siguiente con las filas válidas
-- del fichero:
--
-- a) Imprimir por pantalla el número de jugadores de que consta el archivo.
--    A continuación, imprimalos nombres de los campos contenidos en el
--    archivo, de la siguiente forma:
--
--    "ID" (campo 1)
--    "nombre" (campo 2)
--    ...
--
-- b) Procesar los registros contenidos, de forma que vaya seleccionando
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


