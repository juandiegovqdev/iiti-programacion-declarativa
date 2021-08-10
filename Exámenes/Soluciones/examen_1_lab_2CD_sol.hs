-- PD 2009-10: Examen 1a (20 de enero de 2010, grupos 2C y 2D)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- Apellidos: 
-- Nombre:
-- Grupo:

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir el predicado
--    comprueba :: [[Int]] -> Bool
-- tal que tal que (comprueba xss) se verifica si cada elemento de la
-- lista de listas xss contiene algún número par. Por ejemplo, 
--    comprueba [[1,2],[3,4,5],[8]]  ==>  True
--    comprueba [[1,2],[3,5]]        ==>  False
-- ---------------------------------------------------------------------

comprueba:: [[Int]] -> Bool
comprueba xss = and [or [par x | x <- xs] | xs <- xss]
                where par x = x `mod` 2 == 0

-- ---------------------------------------------------------------------
-- Ejercicio 2. Se define el tipo de datos Grafo a
--    data Grafo a = G [a] [(a,a)]
-- donde (G xs ys) representa el grafo no dirigido cuyos vértices son
-- los elementos de xs y cuyas aristas son los elementos de ys. Por
-- ejemplo, 
--    g1 = G [1,2,3,4] [(1,2),(3,1),(2,3)]
-- 
-- Define en Haskell la función
--    grado :: Eq a => Grafo a -> a -> Int
-- tal que (grado g x) devuelve el grado del vértice x en el grafo g (es
-- decir, el número de aristas de g que inciden en x). Si x no es un
-- vértice de g, se devolverá (-1). Por ejemplo,
--    grado g1 1  ==>  2
--    grado g1 5  ==>  -1
-- ---------------------------------------------------------------------

grado :: Eq a => Grafo a -> a -> Int
grado (G xs ys) x | notElem x xs = -1
                  | otherwise = length [x | (a,b)<-ys, a==x||b==x]
