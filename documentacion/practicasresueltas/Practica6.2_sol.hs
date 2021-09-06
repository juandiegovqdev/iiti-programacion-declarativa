-- PD Practica 6.2 Solución
-- Funciones de orden superior y definiciones por plegados (II)
-- Departamento de Ciencias de la Computación e Inteligencia Artificial
-- Universidad de Sevilla
-- ============================================================================

-- ============================================================================
-- Librerías auxiliares
-- ============================================================================
import Data.Char
import Data.List
-- cabal install primes
import Data.Numbers.Primes
import Test.QuickCheck


-- ----------------------------------------------------------------------------
-- Ejercicio 1. Se considera la función
--      resultadoPos :: (a -> Integer) -> [a] -> [a]
-- tal que (resultadoPos f xs) es la lista de los elementos de la lista
-- xs tales que el valor de la función f sobre ellos es positivo. Por ejemplo,
--   resultadoPos head [[-1,2],[-9,4],[2,3]]       ==  [[2,3]]
--   resultadoPos sum [[1,2],[9],[-8,3],[],[3,5]]  ==  [[1,2],[9],[3,5]]
--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...),
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- -----------------------------------------------------------------------------

resultadoPosC :: (a -> Integer) -> [a] -> [a]
resultadoPosC f xs = [x | x <- xs, f x > 0]

resultadoPosS :: (a -> Integer) -> [a] -> [a]
resultadoPosS f xs = filter (\x -> f x > 0) xs

resultadoPosS' f xs = filter lp (zip (map f xs) xs)
    where lp (a, b) = a > 0


resultadoPosR :: (a -> Integer) -> [a] -> [a]
resultadoPosR _ [] = []
resultadoPosR f (x:xs) 
    | f x > 0   = x: resultadoPosR f xs
    | otherwise = resultadoPosR f xs


resultadoPosP :: (a -> Integer) -> [a] -> [a]
resultadoPosP f = foldr (\x acc -> if f x > 0 then x:acc else acc) []

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
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

intercalaC :: Int -> [Int] -> [Int]
intercalaC y xs =  [x | xs <- xss, x <- xs]
    where xss = [if x < y then [y, x] else [x] | x <- xs]

intercalaS :: Int -> [Int] -> [Int]
intercalaS y xs = concat (map (\x -> if x < y then [y, x] else [x]) xs)

intercalaS' y xs = concatMap (\ x -> if y > x then [y, x] else [x]) xs

intercalaR :: Int -> [Int] -> [Int]
intercalaR _ [] = []
intercalaR y (x:xs)
    | x < y         = [y,x] ++ intercalaR y xs
    | otherwise     = x:intercalaR y xs
    

intercalaP :: Int -> [Int] -> [Int]
intercalaP y = foldr (\x acc -> if x < y then [y, x] ++ acc else x:acc) []

-- ----------------------------------------------------------------------------
-- Ejercicio 3. Se considera la función
--    dec2ent :: [Integer] -> Integer
-- tal que (dec2ent xs) es el número entero cuyas cifras ordenadas son los
-- elementos de la lista xs. Por ejemplo,
--   dec2ent [2,3,4,5]  ==  2345
--   dec2ent [1..9]     ==  123456789
--
-- Defie esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------
   
dec2entC :: [Integer] -> Integer
dec2entC xs = sum (reverse [x * (10^y) | (x, y) <- zip (reverse xs) [0,1..] ])

dec2entS :: [Integer] -> Integer
dec2entS xs = read (map intToDigit  (map fromIntegral xs)) :: Integer

dec2entS' xs = sum (map (\ (x,n) -> x*10^n) (zip xs [n,(n-1)..]) )
  where n = length xs - 1

dec2entR xs = dec2entRaux (reverse xs)
dec2entRaux [] = 0
dec2entRaux (x:xs) = x + 10*dec2entRaux xs

-- tambien con acululador

dec2entP xs = foldr (\ x y -> 10*y + x) 0 (reverse xs)

-- ----------------------------------------------------------------------------
-- Ejercicio 4. Se considera la función
--     diferencia :: Eq a => [a] -> [a] -> [a]
-- tal que (diferencia xs ys) es la diferencia entre los conjuntos xs e
-- ys; es decir, el conjunto de los elementos de la lista xs que no se
-- encuentran en la lista ys. Por ejemplo,
--   diferencia [2,3,5,6] [5,2,7]  ==  [3,6]
--   diferencia [1,3,5,7] [2,4,6]  ==  [1,3,5,7]
--   diferencia [1,3] [1..9]       ==  []
--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- Por comprensión
diferenciaC xs ys = [x | x <- xs, not (elem x ys)]

-- Por orden superior
diferenciaOS xs ys = filter (\ x -> not (elem x ys)) xs


-- ESTOS DOS YA LO HICIMOS EN EL 6
diferenciaR :: Eq a => [a] -> [a] -> [a]
diferenciaR [] _ = []
diferenciaR xs [] = xs
diferenciaR (x:xs) ys
    | elem x ys         = diferenciaR xs ys
    | otherwise         = x:diferenciaR xs ys

diferenciaP :: Eq a => [a] -> [a] -> [a]
diferenciaP xs ys = foldr (\x acc -> if elem x ys then acc else x:acc) [] xs

-- ----------------------------------------------------------------------------
-- Ejercicio 5. Se considera la función
--   primerosYultimos :: [[a]] -> ([a],[a])
-- tal que (primerosYultimos xss) es el par formado por la lista de los
-- primeros elementos de las listas no vacías de xss y la lista de los
-- últimos elementos de las listas no vacías de xss. Por ejemplo,
--   primerosYultimos [[1,2],[5,3,4],[],[9]]  ==  ([1,5,9],[2,4,9])
--   primerosYultimos [[1,2],[1,2,3],[1..4]]  ==  ([1,1,1],[2,3,4])

--
-- Define esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------

-- Por comprensión
primerosYultimosC xss = unzip [(head xs, last xs) | xs <- xss, xs /= []]

-- Por orden superior
primerosYultimosS xss = unzip (map (\ x -> (head x, last x)) (filter (/= []) xss))

-- Por recursión
primerosYultimosR xss = unzip (primerosYultimosR' xss)
primerosYultimosR' [] = []
primerosYultimosR' (xs:xss)
  | xs == []  = primerosYultimosR' xss
  | otherwise = (head xs, last xs) : primerosYultimosR' xss

-- Por plegado
primerosYultimosP xss = unzip (foldr (\ x y -> (head x, last x) : y) [] (filter (/= []) xss))

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
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ----------------------------------------------------------------------------
-- Nota: Usa la función 'gcd'
-- ----------------------------------------------------------------------------

hermanos x y = x == 1 || y == 1 || (gcd x y /= 1)

-- Por comprensión
hermanadaC xs = and [hermanos x y | (x,y) <- zip xs (tail xs)]

-- Por orden superior
hermanadaOS xs = all (\ (x,y) -> hermanos x y) (zip xs (tail xs))

-- Por recursión
hermanadaR :: [Int] -> Bool
hermanadaR [] = True
hermanadaR (x:[]) = True
hermanadaR (x:y:xs) = (hermanos x y) && (hermanadaR (y:xs))

-- Por plegado
hermanadaP xs = foldr (\ (x,y) z -> (hermanos x y) && z) True (zip xs (tail xs))

-- ----------------------------------------------------------------------------
-- Ejercicio 7. Un elemento de una lista es permanente si ninguno de los que
-- vienen a continuación en la lista es mayor que él. Consideramos la función
--   permanentes :: [Int] -> [Int]
-- tal que (permanentes xs) es la lista de los elementos permanentes de la
-- lista xs. Por ejemplo,
--   permanentes [80,1,7,8,4]  ==  [80,8,4]

-- Se pide definir esta función
-- 1) por comprensión,
-- 2) por orden superior (map, filter, ...)
-- 3) por recursión,
-- 4) por plegado (con 'foldr').
-- ---------------------------------------------------------------------------
-- Nota: Usa la función 'tails' de Data.List.
-- ----------------------------------------------------------------------------

head_permanente (y:[]) = True
head_permanente (y:ys) = (maximum ys < y)

permanentesC :: [Int] -> [Int]
permanentesC xs = [y | (y:ys) <- tails xs, head_permanente (y:ys)]

-- Por orden superior               
permanentesS :: [Int] -> [Int]
permanentesS xs = map head (filter head_permanente (init (tails xs)))

-- Por composición de funciones
permanentesS' :: [Int] -> [Int]
permanentesS' = (map head) .  (filter head_permanente) .  init . tails

-- Por recursión
permanentesR :: [Int] -> [Int]
permanentesR [] = []
permanentesR (x:xs)
  | head_permanente (x:xs) = x:permanentesR xs
  | otherwise   = permanentesR xs

-- Por plegado
permanentesP xs = foldr (\xs acc -> if head_permanente xs then (head xs):acc else acc) [] (init (tails xs))


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

muyPrimo 0 = True
muyPrimo n = isPrime n && (muyPrimo (div n 10))

-- ---------------------------------------------------------------------
-- ¿Cuántos números de cinco cifras son muy primos?
-- ---------------------------------------------------------------------

-- El cálculo es
-- sum [if muyPrimo x then 1 else 0 | x <- [10^4..(10^5-1)]]
-- > 15
-- ---------------------------------------------------------------------

