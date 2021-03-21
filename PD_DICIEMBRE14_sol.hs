-- ------------------------------------------------------------------
-- PD. TERCERA CONVOCATORIA 
-- 4 DICIEMBRE 2014
-- ------------------------------------------------------------------
-- NOMBRE Y APELLIDOS:
-- UVUS:
-- ------------------------------------------------------------------
--
-- Antes de comenzar, renombra estefichero: uvus_dic.hs
--
-- ------------------------------------------------------------------
import Data.List
-- ------------------------------------------------------------------
-- Ejercicio 1. (2 puntos)
--   Dada una lista de elementos de un tipo a, queremos obtener la longitud de
--   la secuencia de valores consecutivos de la lista iguales a uno dado.
--   Por ejemplo, dada la lista numerica:
ln = [2,3,3,5,5,5,5,2,3,5,5,5,4,5,2,2,2]
--   y el valor 5 observamos que la secuencia mas larga de valores iguales a 5
--   tiene longitud 4. Si el valor no aparece entonces debe devolver 0.
--      Por tanto:
--          secuenciaMasLarga ln 5 == 4
--          secuenciaMasLarga ln 2 == 3
--          secuenciaMasLarga ln 1 == 0
-- --------------------------------------------------------------------
-- SOLUCION:
secuencias :: (Eq a) => [a] -> a -> [[a]]
secuencias [] c = []
secuencias (x:xs) c | c == x = (x:takeWhile (==c) xs) 
                               : (secuencias (dropWhile (==c) xs) c)
                    | otherwise = secuencias xs c
                                  
longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes xss = map length xss

secuenciaMasLarga :: (Eq a) => [a] -> a -> Int
secuenciaMasLarga ls c |ls == [] = 0
                       |otherwise =  maximum longs     
                              where longs = longitudes (secuencias ls c)
-- --------------------------------------------------------------------
-- Ejercicio 2. (3 puntos)
-- Definir una funcion (sumInvImp n) que se verifique si n es un numero
-- que no termina en cero y
-- la suma de n con su inverso es un numero con todas las cifras
-- impares, considerando el inverso de n como el numero que resulta de
-- escribir n al reves.
-- Por ejemplo,
-- sumInvImp 36 == True  porque 36+63 = 99 tiene todas sus cifras impares
-- sumInvImp 243 == False, ya que 243+342=585, que tiene una cifra par.

-- Definir la funcion
--    invMenores :: Int -> Int
-- tal que (invMenores n) es la cantidad de numeros que cumplen
-- sumInvImp y son menores que n. Por ejemplo,
-- invMenores 10 == 0
-- invMenores 100 == 20
-- invMenores 1000 == 120
-- --------------------------------------------------------------------
-- SOLUCION:
invMenores :: Int -> Int
invMenores n = length [x| x<-[1 .. n-1], sumInvImp x]

sumInvImp :: Int -> Bool
sumInvImp n = rem n 10 /= 0 && impares (cifras (n+(inverso n)))

-- (cifras n) es la lista de las cifras del numero n. Por ejemplo,
-- cifras 3526 == [3,5,2,6]
cifras :: Int -> [Int]
cifras n = [read [x] | x<- show n]

-- (inverso n) es el numero obtenido escribiendo las cifras de n 
-- en orden inverso. Por ejemplo,
--     inverso 3526 == 6253
inverso :: Int -> Int
inverso n = read (reverse (show n))

-- (impares xs) se verifica si xs es una lista de numeros impares. 
-- Por ejemplo,
-- impares [3,5,2] == False
-- impares [3,5,1] == True
impares :: [Int] -> Bool
impares xs = and [odd x | x <- xs]
-- --------------------------------------------------------------------
-- Ejercicio 3. (2 puntos)
-- Definir una funcion (ramaAladerecha ar) que, dado un arbol del tipo
data Arbol a =  Nodo a [Arbol a] deriving Show
-- devuelva la rama mas a la derecha del arbol ar, es decir, la lista de
-- los nodos que estan mas a la derecha
-- Por ejemplo, para el arbol:
arbolito = Nodo 3 [Nodo 4 [],
                   Nodo 5 [Nodo 6 [] ],
                   Nodo 7 [Nodo 1 [],
                           Nodo 5 [Nodo 2 []],
                           Nodo 9 [Nodo 1 [],Nodo 8 []]]]
-- la función devolvera:
-- ramaAladerecha arbolito == [3,7,9,8]
-- La representacion de arbolito es:
--                    3
--                 /  |   \
--                4   5    7
--                    |  / |  \
--                    6 1  5   9
--                         |  / \
--                         2 1   8
ramaAladerecha :: Arbol a -> [a]
ramaAladerecha (Nodo x []) = [x]
ramaAladerecha (Nodo x ars)= x:ramaAladerecha (last ars)
