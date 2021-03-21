-- PD 2009-10: Examen 1a (20 de enero de 2010, grupos 2A y 2B)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import Data.List

-- Apellidos: 
-- Nombre:
-- Grupo:

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    duplica :: Eq a => a -> [a] -> [a]
-- tal que (duplica x xs) devuelve la lista obtenida al duplicar cada
-- ocurrencia de x en xs. Por ejemplo, 
--    duplica 3 [1..5] => [1,2,3,3,4,5]
--    duplica 'l' "Haskell" => "Haskellll" 
-- ---------------------------------------------------------------------

duplica :: Eq a => a -> [a] -> [a]
duplica _ [] = []
duplica x (y:ys) | x == y    = y:y:duplica x ys
                 | otherwise = y:duplica x ys

-- ----------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    listaconSuma :: Int -> [[Int]] 
-- que, dado un número natural n, devuelve todas las listas de enteros
-- positivos (esto es, enteros mayores o iguales que 1) cuya suma sea
-- n. Por ejemplo,
--    Main> listaconSuma 4
--    [[1,1,1,1],[1,1,2],[1,2,1],[1,3],[2,1,1],[2,2],[3,1],[4]]
-- ---------------------------------------------------------------------

listaconSuma :: Int -> [[Int]]
listaconSuma 0         = [[]]
listaconSuma n | n > 0 = [(x:xs) | x <- [1..n], xs <- listaconSuma (n-x)]
