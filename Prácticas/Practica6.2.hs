-- PD 2019-20: Practica 6.2
-- Funciones de orden superior y definiciones por plegados (II)
-- ----------------------------------------------------------------------------

import Data.Char
import Data.List
import Data.Numbers.Primes

-- ----------------------------------------------------------------------------
-- Ejercicio 1. Se considera la función
--      resultadoPos :: (a -> Integer) -> [a] -> [a]
-- tal que (resultadoPos f xs) es la lista de los elementos de la lista
-- xs tales que el valor de la función f sobre ellos es positivo. Por ejemplo,
--   resultadoPos head [[-1,2],[-9,4],[2,3]]       ==  [[2,3]]
--   resultadoPos sum [[1,2],[9],[-8,3],[],[3,5]]  ==  [[1,2],[9],[3,5]]
--
-- Define esta función
-- -----------------------------------------------------------------------------
-- 1) por comprensión
resultadoPosC :: (a -> Integer) -> [a] -> [a]
resultadoPosC f xss = [x | x <- xss, f x > 0]

-- 2) por orden superior (map, filter, ...)
resultadoPosOS :: (a -> Integer) -> [a] -> [a]
resultadoPosOS f xss = filter (\x -> f x > 0) xss

-- 3) por recursión
resultadoPosR :: (a -> Integer) -> [a] -> [a]
resultadoPosR _ [] = []
resultadoPosR f (x:xs) 
    | f x > 0   = [x] ++ resultadoPosR f xs
    | otherwise = resultadoPosR f xs

-- 4) por plegado (con 'foldr')


-- ----------------------------------------------------------------------------
-- Ejercicio 2. Se considera la función
--     intercala :: Int -> [Int] -> [Int]
-- tal que (intercala y xs) es la lista que resulta de intercalar el elemento
-- y delante de todos los elementos de la lista xs que sean menores que y.
-- Por ejemplo,
--   intercala 5 [1,2,6,3,7,9]  ==  [5,1,5,2,6,5,3,7,9]
--   intercala 5 [6,7,9,8]      ==  [6,7,9,8]
--
-- Define esta función
-- ----------------------------------------------------------------------------
-- 1) por comprensión,
{--
intercalaC :: Int -> [Int] -> [Int]
intercalaC y xs = [b x y | x <- xs]
    where b x y = if x < y then x y else x
--}

-- 2) por orden superior (map, filter, ...)


-- 3) por recursión,
intercalaR :: Int -> [Int] -> [Int]
intercalaR _ [] = []
intercalaR y (x:xs)
    | x < y = y : x : intercalaR y xs
    | otherwise = x : intercalaR y xs

-- 4) por plegado (con 'foldr').


-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la función
--    dec2ent :: [Integer] -> Integer
-- tal que (dec2ent xs) es el número entero cuyas cifras ordenadas son los
-- elementos de la lista xs. Por ejemplo,
--   dec2ent [2,3,4,5]  ==  2345
--   dec2ent [1..9]     ==  123456789
--
-- Define esta función
-- ----------------------------------------------------------------------------
-- 1) por comprensión,


-- 2) por orden superior (map, filter, ...)


-- 3) por recursión,


-- 4) por plegado (con 'foldr').


-- ----------------------------------------------------------------------------
-- Ejercicio 4. Se considera la función
--     diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, el conjunto de los elementos de la lista xs que no se
-- encuentran en la lista ys. Por ejemplo,
--   diferenciaOS [2,3,5,6] [5,2,7]  ==  [3,6]
--   diferencia [1,3,5,7] [2,4,6]  ==  [1,3,5,7]
--   diferencia [1,3] [1..9]       ==  []
--
-- Define esta función
-- ----------------------------------------------------------------------------

contieneElemento :: Eq a => a -> [a] -> Bool
contieneElemento _ [] = False
contieneElemento a (x:xs)
    | a == x = True
    | otherwise = contieneElemento a xs

-- 1) por comprensión,
diferenciaC :: Eq a => [a] -> [a] -> [a]
diferenciaC xs ys = [xs!!x | x <- [0..length xs - 1], not (contieneElemento (xs!!x) ys)]

-- 2) por orden superior (map, filter, ...)
diferenciaOS :: Eq a => [a] -> [a] -> [a]
diferenciaOS xs ys = filter (\ x -> not (contieneElemento x ys)) xs 

-- 3) por recursión,
diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR (x:xs) ys 
    | not (contieneElemento x ys) = x : diferenciaR xs ys
    | otherwise = diferenciaR xs ys
diferenciaR _ ys = []

-- 4) por plegado (con 'foldr').
diferenciaF :: Eq a => [a] -> [a] -> [a]
diferenciaF = undefined
-- diferenciaF xs ys = foldr (\ x ys -> not (contieneElemento x ys)) []

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Se considera la función
--   primerosYultimos :: [[a]] -> ([a],[a])
-- tal que (primerosYultimos xss) es el par formado por la lista de los
-- primeros elementos de las listas no vacías de xss y la lista de los
-- últimos elementos de las listas no vacías de xss. Por ejemplo,
--   primerosYultimos [[1,2],[5,3,4],[],[9]]  ==  ([1,5,9],[2,4,9])
--   primerosYultimos [[1,2],[1,2,3],[1..4]]  ==  ([1,1,1],[2,3,4])

-- Define esta función
-- ----------------------------------------------------------------------------
eliminarListaVacia :: [[a]] -> [[a]]
eliminarListaVacia [] = []
eliminarListaVacia (xs:xss)
    | null xs = eliminarListaVacia xss
    | otherwise = xs : eliminarListaVacia xss

-- 1) por comprensión,
primerosYultimosC :: [[a]] -> ([a],[a])
primerosYultimosC xss = ([head xs |xs <- xss, not (null xs)], [last xs |xs <- xss, not (null xs)])

-- 2) por orden superior (map, filter, ...)
primerosYultimosOS :: [[a]] -> ([a],[a])
primerosYultimosOS xss = (map head (eliminarListaVacia xss), map last (eliminarListaVacia xss))

-- 3) por recursión,
primerosYultimosR :: [[a]] -> ([a],[a])
primerosYultimosR xss = (primeros xss, ultimos xss)

primeros :: [[a]] -> [a]
primeros [] = []
primeros (xs:xss)
    | not (null xs) = head xs : primeros xss 
    | otherwise = primeros xss

ultimos :: [[a]] -> [a]
ultimos [] = []
ultimos (xs:xss)
    | not (null xs) = last xs : ultimos xss 
    | otherwise = ultimos xss

-- 4) por plegado (con 'foldr').


-- ----------------------------------------------------------------------------
-- Ejercicio 6. Una lista hermanada es una lista de números estrictamente
-- positivos en la que cada elemento tiene algún factor primo en común con el
-- siguiente, en caso de que exista, o alguno de los dos es un 1. Por ejemplo,
-- [2,6,3,9,1,5] es una lista hermanada.

-- Se considera la función
--    hermanada :: [Int] -> Bool
-- tal que (hermanada xs) comprueba que la lista xs es hermanada según la
-- definición anterior. Por ejemplo,
--    hermanada [2,6,3,9,1,5]  ==  True
--    hermanada [2,3,5]        ==  False
--
-- Se pide definir esta función
-- ----------------------------------------------------------------------------
-- Nota: Usa la función 'gcd'
-- ----------------------------------------------------------------------------
-- 1) por comprensión,


-- 2) por orden superior (map, filter, ...)


-- 3) por recursión,


-- 4) por plegado (con 'foldr').


-- ----------------------------------------------------------------------------
-- Ejercicio 7. Un elemento de una lista es permanente si ninguno de los que
-- vienen a continuación en la lista es mayor que él. Consideramos la función
--   permanentes :: [Int] -> [Int]
-- tal que (permanentes xs) es la lista de los elementos permanentes de la
-- lista xs. Por ejemplo,
--   permanentes [80,1,7,8,4]  ==  [80,8,4]

-- Se pide definir esta función
-- ---------------------------------------------------------------------------
-- Nota: Usa la función 'tails' de Data.List.
-- ----------------------------------------------------------------------------
esPermanente :: Int -> [Int] -> Bool
esPermanente a [] = True
esPermanente a (x:xs)
    | a >= x = esPermanente a xs
    | otherwise = False

-- 1) por comprensión,
permanentesC :: [Int] -> [Int]
permanentesC xs = [xs!!x | x <- [0..length xs-1], esPermanente (xs!!x) (drop (x+1) xs)]

-- 2) por orden superior (map, filter, ...)
permanentesF :: [Int] -> [Int]
permanentesF (x:xs) = undefined

-- 3) por recursión,
permanentesR :: [Int] -> [Int]
permanentesR [] = []
permanentesR (x:xs)
    | esPermanente x xs = x : permanentesR xs
    | otherwise = permanentesR xs

-- 4) por plegado (con 'foldr').


-- ---------------------------------------------------------------------
-- Ejercicio 8. Un número entero positivo n es muy primo si es n primo
-- y todos los números que resultan de ir suprimimiendo la última cifra
-- también son primos. Por ejemplo, 7193 es muy primo pues los números
-- 7193, 719, 71 y 7 son todos primos. 
-- 
-- Define la función 
--    muyPrimo :: Integer -> Bool
-- que (muyPrimo n) se verifica si n es muy primo. Por ejemplo,
--    muyPrimo 7193  == True
--    muyPrimo 71932 == False
-- --------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- ¿Cuántos números de cinco cifras son muy primos?
-- ---------------------------------------------------------------------

-- El cálculo es

-- ---------------------------------------------------------------------