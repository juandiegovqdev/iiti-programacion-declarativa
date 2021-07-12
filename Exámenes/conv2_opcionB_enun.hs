-- PROGRAMACION DECLARATIVA. CURSO 2013-14
-- GRADO EN INGENIERÍA INFORMÁTICA. TECNOLOGÍAS INFORMÁTICAS.
-- EXAMEN DE LA CONVOCATORIA DE SEPTIEMBRE. 08/09/2014

import Data.List
-- NOTA: AL IMPORTAR ESTE MÓDULO, PODEMOS UTILIZAR FUNCIONES COMO sort o
-- nub, si son necesarias. 
-- ----------------------------------------------------------------------
-- EJERCICIO 1.
-- Definir por COMPRENSION Y RECURSION  una funcion (cuenta ls) tal que,
-- dada una lista ls, devuelve la lista que resulta de emparejar (SOLO
-- UNA VEZ) cada elemento con el numero de veces que aparece en ls. 

-- *Main> cuenta [1,2,3,4,3,2,1,2,3,4,3,2]
-- [(1,2),(2,4),(3,4),(4,2)]
-- *Main> cuenta "tratado"
-- [('t',2),('r',1),('a',2),('d',1),('o',1)]
-- POR COMPRESION
cuentaC :: (Eq a) => [a] -> [(a, Int)]
cuentaC = undefined

-- POR RECURSION
cuentaR :: (Eq a) => [a] -> [(a, Int)]
cuentaR = undefined

-- ----------------------------------------------------------------------
-- EJERCICIO 2
-- Define POR RECURSION Y COMPRENSION una funcion encaja tal que
-- (encaja x n xss) devuelve las listas de xss en las que x aparece en
-- la posicion n. Recordar que las posiciones empiezan desde 0. 
-- Ejemplos:
-- encaja 2 1 [] = []
-- encaja 2 1 [[0,1,2,3], [3,2,0], [2,2]] = [[3,2,0],[2,2]]
-- encaja 'a' 1 ["casa","perro", "bajo"]= ["casa","bajo"]
-- encaja 'o' 3 ["casa","perro", "bajo"]= ["bajo"]
-- encaja 'e' 8 ["casa","perro", "bajo"]= []

-- POR RECURSION
encaja :: (Eq a) => a -> Int -> [[a]]-> [[a]]
encaja = undefined
-- POR COMPRENSION
encajaC :: (Eq a) => a -> Int -> [[a]]-> [[a]]
encajaC = undefined

-- ----------------------------------------------------------------------
-- EJERCICIO 3.
-- Definir una función (rotacion n m) tal que, dados dos números
-- naturales n y m, determine si el segundo es una rotacion de las
-- cifras del primero.
-- Por ejemplo, las rotaciones del numero 4567 seran
-- [[5,6,7,4],[6,7,4,5],[7,4,5,6],[4,5,6,7]], por tanto, por ejemplo,
-- 6745 es una rotacion de 4567.
-- *Main> rotacion 4345 453
-- False
-- *Main> rotacion 4345 4543
-- True
rotacion :: Int -> Int -> Bool
rotacion = undefined

-- ----------------------------------------------------------------------
-- EJERCICIO 4.
-- Definir, utilizando PLEGADO, una función (repetidos n l) tal 
-- que, dados un número natural n y una lista de números naturales l,
-- devuelva los números de l que son rotaciones de las cifras de n.
-- Nota: se puede utilizar la funcion rotacion del ejercicio 3.

-- *Main> repetidos 1234 [3421,543,234,1243,3412,54,743, 2341]
-- [3412,2341]

repetidos :: Int -> [Int] -> [Int]
repetidos  =  undefined

-- ----------------------------------------------------------------------
-- EJERCICIO 5.
-- Para el tipo de dato Arbol binario siguiente: 
data Arbol = Hoja | Nodo Arbol Int Arbol 
                deriving Show 
-- Definir los arboles de los ejemplos segun ese tipo de dato, es decir,
-- los arboles ejArbol1 y ejArbol2.
-- A continuacion, definir la función 
-- recorta :: Arbol -> Arbol 
-- tal que, dado un árbol, elimina el nodo que está a mayor profundidad, 
-- es decir, aquellos nodos que terminan en dos Hojas como muestran los 
-- ejemplos: 
 
-- ejArbol1:                             recorta ejArbol1: 
--                  7                              7 
--                 /  \                           / \ 
--               /      \                        /   \ 
--              4       8                       4     H 
--             /  \    /  \                    /  \ 
--            H   5   H    H                  H    H 
--               / \ 
--              H   H 
ejArbol1 = undefined
 
-- ejArbol2:                           recorta ejArbol2: 
--               1                                 1 
--              /  \                             /   \ 
--             /    H                           / 
--            9                                9 
--          /   \                            /   \ 
--         7     2                          H     2 
--        / \   /  \                             /  \ 
--       H   H H    6                           H    6 
--                 /  \                             /  \ 
--                4    H                           H    H 
--               /  \ 
--              H    H 
ejArbol2 = undefined
 
recorta :: Arbol -> Arbol 
recorta  = undefined
-- ---------------------------------------------------------------------