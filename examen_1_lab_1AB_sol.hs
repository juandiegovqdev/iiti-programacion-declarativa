-- PD 2009-10: Examen 1a (20 de enero de 2010, grupos 1A y 1B)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import Data.List

-- Apellidos: 
-- Nombre:
-- Grupo:

-- ---------------------------------------------------------------------
-- Ejercicio 1. Supongamos que queremos convertir la sucesión ordenada
-- de las páginas en las que aparece un concepto en la entrada de dicho
-- concepto en el índice del libro. Por ejemplo, la sucesión 1, 3, 4, 6,
-- 7, 8, 9, 12, 13, 14 se transforma en 1, 3-4, 6-9, 12-14. Definir la
-- función 
--    indice :: [Int] -> [[Int]]
-- tal que (indice xs) la lista obtenida aplicando la transformación
-- anterior a la sucesión xs. Por ejemplo,
--    indice [1,3,4,6,7,8,9,12,13,14]  => [[1],[3,4],[6,9],[12,14]]
-- ---------------------------------------------------------------------

indice :: [Int] -> [[Int]]
indice []  = []
indice [x] = [[x]]
indice (x:y:xs) | x+1 == y  = [x,last ys]:(tail yss)
                | otherwise = [x]:yss
                where yss = indice (y:xs)
                      ys  = head yss

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función 
--    diferencias :: Num a => [a] -> [a]
-- tal que (diferencias xs) es la lista de las diferencias entre los
-- elementos consecutivos de xs. Por ejemplo, 
--    diferencias [5,3,8,7]  ==> [2,-5,1]
-- ---------------------------------------------------------------------

diferencias :: Num a => [a] -> [a]
diferencias [] = []
diferencias xs = [a-b | (a,b) <- zip xs (tail xs)]

