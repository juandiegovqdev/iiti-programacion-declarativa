import Data.List

-- Apellidos: 
-- Nombre:
-- Grupo:

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la funci�n
--    comprime :: Eq a => [a] -> [(a,Int)]
-- tal que (comprime xs) es la lista obtenida comprimiendo xs de forma
-- que represente los pares formados por los elementos de xs y sus
-- ocurrencias contiguas. Por ejemplo,
--    comprime "aaabbaccc"  ==>  [('a',3),('b',2),('a',1),('c',3)]
-- ---------------------------------------------------------------------

-- 1� definici�n 
comprime :: Eq a => [a] -> [(a,Int)]
comprime [] = []
comprime [x] = [(x,1)]
comprime (x:y:xs) | x == y    = (x,n+1) : tail ys
                  | otherwise = (x,1)   : ys
                  where ys = comprime (y:xs)
                        n  = snd (head ys)

-- 2� definici�n (usando group)
comprime_2 :: Eq a => [a] -> [(a,Int)]
comprime_2 xs = [(head ys, length ys) | ys <- group xs]

-- 3� definici�n (usando takeWile y dropWhile)
comprime_3 :: Eq a => [a] -> [(a,Int)]
comprime_3 [] = []
comprime_3 (x:xs) = (x,1 + length (takeWhile (==x) xs)) : 
                    comprime_3 (dropWhile (==x) xs)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funci�n
--    producto :: [[a]] -> [[a]]
-- tal que (producto xss) es el producto cartesiano de los conjuntos
-- xss. Por ejemplo,
--    *Main> producto [[1,3],[2,5]]
--    [[1,2],[1,5],[3,2],[3,5]]
--    *Main> producto [[1,3],[2,5],[6,4]]
--    [[1,2,6],[1,2,4],[1,5,6],[1,5,4],[3,2,6],[3,2,4],[3,5,6],[3,5,4]]
--    *Main> producto [[1,3,5],[2,4]]
--    [[1,2],[1,4],[3,2],[3,4],[5,2],[5,4]]
--    *Main> producto []
--    [[]]
-- ---------------------------------------------------------------------

producto :: [[a]] -> [[a]]
producto []       = [[]]
producto (xs:xss) = [x:ys | x <- xs, ys <- producto xss]

