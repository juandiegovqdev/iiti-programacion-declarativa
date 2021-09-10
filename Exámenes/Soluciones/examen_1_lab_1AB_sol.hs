import Data.List

-- Apellidos: 
-- Nombre:
-- Grupo:

-- ---------------------------------------------------------------------
-- Ejercicio 1. Supongamos que queremos convertir la sucesi�n ordenada
-- de las p�ginas en las que aparece un concepto en la entrada de dicho
-- concepto en el �ndice del libro. Por ejemplo, la sucesi�n 1, 3, 4, 6,
-- 7, 8, 9, 12, 13, 14 se transforma en 1, 3-4, 6-9, 12-14. Definir la
-- funci�n 
--    indice :: [Int] -> [[Int]]
-- tal que (indice xs) la lista obtenida aplicando la transformaci�n
-- anterior a la sucesi�n xs. Por ejemplo,
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
-- Ejercicio 2. Definir la funci�n 
--    diferencias :: Num a => [a] -> [a]
-- tal que (diferencias xs) es la lista de las diferencias entre los
-- elementos consecutivos de xs. Por ejemplo, 
--    diferencias [5,3,8,7]  ==> [2,-5,1]
-- ---------------------------------------------------------------------

diferencias :: Num a => [a] -> [a]
diferencias [] = []
diferencias xs = [a-b | (a,b) <- zip xs (tail xs)]

