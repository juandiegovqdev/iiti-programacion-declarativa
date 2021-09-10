import Data.List

-- NOTA: AL IMPORTAR ESTE MÓDULO, PODEMOS UTILIZAR FUNCIONES
-- COMO sort o nub, si son necesarias.
-- -----------------------------------------------------------------
-- EJERCICIO 1.(1,5 puntos)
--  Definir por COMPRENSIÓN Y RECURSIÓN la función (parejas l1 l2),
--  tal que, dadas dos listas l1 y l2, construya todas las posibles 
--  parejas que se pueden formar con los elementos de ambas. 

-- *Main> parejas [1,2,3] [4,5]
-- [(1,4),(1,5),(2,4),(2,5),(3,4),(3,5)]
-- *Main> parejas "casa" [4,5]
-- [('c',4),('c',5),('a',4),('a',5),('s',4),('s',5),('a',4),('a',5)]

parejaAux :: a -> [b] -> [(a, b)]
parejaAux _ [] = []
parejaAux a (x:xs) = (a, x) : parejaAux a xs

-- RECURSIÓN
parejasR :: [a] -> [b] -> [(a,b)]
parejasR [] _ = []
parejasR (x:xs) ys = parejaAux x ys ++ parejasR xs ys

-- COMPRENSIÓN
parejasC :: [a] -> [b] -> [(a,b)]
parejasC xs ys = [(x, y) | x <- xs, y <- ys]

-- -----------------------------------------------------------------
-- EJERCICIO 2.(1,5 puntos)
-- Definir POR RECURSIÓN Y COMPRENSIÓN una función (elimina l n) tal
-- que, dada una lista l y un número natural n(mayor que 1), elimine
-- de la lista todos los elementos cuyas posiciones sean múltiplos
-- de n. (Considerar que la primera posición es la 0).

-- *Main> elimina "caminata" 3
-- "amnaa"
-- *Main> elimina [0,1,2,3,4,5,6,7,8,9,10,11,12] 4
-- [1,2,3,5,6,7,9,10,11]

-- POR RECURSIÓN
eliminaR :: [a] -> Int -> [a] 
eliminaR = undefined

-- USANDO COMPRENSIÓN
eliminaC :: [a] -> Int -> [a]
eliminaC = undefined

-- -----------------------------------------------------------------
-- EJERCICIO 3.(1 punto)
-- Definir una función permuta que, dados dos números naturales,
-- determine si el segundo es una permutación de las cifras del
-- primero.

-- *Main> permuta 4345 453
-- False
-- *Main> permuta 4345 4534
-- True

permuta :: Int -> Int -> Bool
permuta = undefined

-- -----------------------------------------------------------------
-- EJERCICIO 4.(1,5 puntos)
-- Definir, utilizando PLEGADO, una función (repetidos n l) tal 
-- que, dados un número natural n y una lista de números naturales l,
-- devuelva los números de l que son permutaciones de las cifras de n.
-- NOTA: Se puede utilizar la funcion permuta del ejercicio 3.

-- *Main> repetidos 1234 [3421,543,234,1243,2314,54,743]
-- [3421,1243,2314]

repetidos :: Int -> [Int] -> [Int]
repetidos = undefined

-- -----------------------------------------------------------------
-- EJERCICIO 5. (2,5 puntos)
-- Definir un tipo de dato que permita representar árboles (no
-- necesariamente binarios) con datos enteros en los  nodos.

-- data Arbol a = undefined

-- Definir, utilizando esa representación,el siguiente árbol al que
-- llamaremos arbol8.

--                5     
--              / | \
--             7  1  3   
--               / \
--              2   3   
--                  |
--                  2

-- arbol8 :: Arbol Int
-- arbol8 = undefined

-- Definir la función ramaLarga que devuelva una lista con los datos
-- de la rama más larga.
-- *Main> ramaLarga arbol8
-- [5,1,3,2]

-- ramaLarga :: Arbol a -> [a]
-- ramaLarga = undefined
-- -----------------------------------------------------------------


