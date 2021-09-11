-- ==================================================================
import Test.QuickCheck
import Data.List -- Se puede utilizar la funcion nub
-- =================================================================
-- Ejercicio 1 
-- (a) Define usando listas por comprension la funcion
--      mulPosC :: Int ->  [Int] -> [Int]
-- tal que (mulPosC x xs) devuelve los elementos de xs que son 
-- multiplos positivos de x. Por ejemplo,
--    mulPosC 3 [1,6,-5,-9,33] == [6,33]
-- (b) Da una definicion recursiva de la funcion anterior:
--       mulPosR :: Int -> [Int] -> [Int]
-- ==================================================================
mulPosC :: Int -> [Int] -> [Int]
mulPosC x xs =  [y | y <- xs, y > 0 && rem y x == 0]  

mulPosR :: Int -> [Int] -> [Int]
mulPosR  x [] = []
mulPosR x (y:ys) | y > 0 && rem y x == 0 = y:mulPosR x ys
                 | otherwise = mulPosR x ys

-- ==================================================================
-- Ejercicio 2 
-- Diremos que una lista numerica es muy creciente si cada elemento es
-- mayor estricto que el doble del anterior. 
-- (a) Define el predicado 
--   muyCreciente :: (Ord a,Num a) => [a] -> Bool
-- tal que (muyCreciente xs) se verifica si xs es una lista muy
-- creciente. Por ejemplo,
--    muyCreciente [3,7,100,220] == True
--    muyCreciente [1,5,7,1000] == False
-- (b) Para generar listas muy crecientes, consideramos la funcion
--     f :: Integer -> Integer -> Integer
-- dada por las ecuaciones recursivas:
--     f(x,0) = x
--     f(x,n) = 2*f(x,n-1) + 1, n > 0
--  Define la funcion 
--      lista :: Int -> Integer -> [Integer]
-- tal que (lista n x) devuelve la lista [f(x,0),f(x,1),...,f(x,n)]
-- Por ejemplo,
--   lista 5 4 == [4,9,19,39,79]
-- ==================================================================
muyCreciente :: (Ord a,Num a) => [a] -> Bool
muyCreciente xs = and [y > 2*x | (x,y) <- zip xs (tail xs)]

f :: Integer -> Integer -> Integer
f x 0 = x
f x n = 2*f x (n-1) + 1

lista :: Int -> Integer -> [Integer]
lista n x = take n [f x i | i <- [0..]]
-- ===================================================================
-- Ejercicio 3.  
-- Decimos que los numeros m y n estan relacionados si
-- tienen los mismos divisores primos.
-- (a) Definir la funcion 
--     relacionados:: Integer -> Integer -> Bool
-- tal que (relacionados m n) se verifica si m y n estan relacionados.
-- Por ejemplo,
--     relacionados 24 32 == False ( divisores primos de 24 son el 2 y
--     el 3; divisores primos de 32 es el 3) 
--     relacionados 24 12 == True (divisores primos de 12 son el 2 y 3.
--     relacionados 24 2  == False
--     relacionados 18 12 == True
-- ---------------------------------------------------------------------
relacionados:: Integer -> Integer -> Bool
relacionados m n = 
    divisoresPrimos n == divisoresPrimos  m

divisores n = 
    [x | x <- [1..n], n `rem` x == 0]

esPrimo n = divisores n == [1,n]

divisoresPrimos n = 
    [x | x <- [1..n], n `rem` x == 0, esPrimo x]

-- ----------------------------------------------------------------------------
-- Ejercicio 4. El nivel de una lista es el numero de
-- elementos mayores que 1 menos el numero de elementos menores que 1.
-- Definir POR COMPRENSION, POR RECURSION Y PLEGADO la funcion 
--    nivel :: [Integer] -> Integer
-- tal que '(nivel xs)' es el nivel de la lista 'xs'. Por ejemplo,
--    nivel [1,2,0,-1]     ==  -1
--    nivel [1,0,2,0,3]    ==  0
--    nivel [1,0,-2,0,3]   ==  -2
--    nivel [1,0,-2,0,-3]  ==  -4
--    nivel [1,0,-2,2,-3]  ==  -2
-- ----------------------------------------------------------------------------
nivelC :: [Integer] -> Integer
nivelC xs = length [x|x<-xs, x>1]- length [x|x<-xs, x<1]

nivelR :: [Integer] -> Integer
nivelR [] = 0
nivelR (x:xs)| x>1 = 1 + nivelR xs 
             | x<1 = nivelR xs -1
             |otherwise = nivelR xs
nivelP :: [Integer] -> Integer
nivelP = foldr f 0
    where f u v | u>1 = 1 + v
                | u<1 = v -1
                | otherwise = v
