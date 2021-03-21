-- ------------------------------------------------------------------
-- PROGRAMACIÓN DECLARATIVA (3ºTI). 
-- EXAMEN DE LA TERCERA CONVOCATORIA
-- 2 DICIEMBRE 2015
-- ------------------------------------------------------------------
-- NOMBRE Y APELLIDOS:
-- UVUS:
-- ------------------------------------------------------------------
-- Antes de comenzar, renombra este fichero: uvus_1.hs
-- Todos los ejercicios valen lo mismo.
-- --------------------------------------------------------------------
-- Ejercicio 1. Consideremos la secuencia infinita de todos los numeros
-- naturales. 
--    [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,...]
-- La lista de sus digitos será una lista infinita:
--    [1,2,3,4,5,6,7,8,9,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,2,,...]
-- 
-- Definir la funcion 
--    secuenciaDigitosNaturales :: [Int]
-- tal que su valor es la secuencia infinita de los digitos de todos los
-- elementos de la secuencia de numeros naturales. Por ejemplo,
--    take 11 secuenciaDigitosNaturales  ==  [1,2,3,4,5,6,7,8,9,1,0]
-- ---------------------------------------------------------------------
secuenciaDigitosNaturales :: [Int]
secuenciaDigitosNaturales = [read [c] | n <- [1..], c <- show n]
-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la funcion
--     siembra :: [a] -> [[a]] -> [[a]]
-- tal que (siembra xs yss) es la lista obtenida introduciendo cada uno
-- de los elementos de xs en la lista Y lugar correspondiente de yss; es decir,
-- el primer elemento de xs ira el primero en la primera lista de yss,
-- el segundo elemento de xs ira segundo en la segunda lista de yss,
-- etc. OBSERVA LOS EJEMPLOS: 
--    siembra [1,2,3] [[4,7],[6],[9,5,8]]  ==  [[1,4,7],[6,2],[9,5,3,8]]
--    siembra [1,2] [[4,7],[6],[9,5,8]]    ==  [[1,4,7],[6,2],[9,5,8]]
--    siembra [1,2,3] [[4,7],[6]]          ==  [[1,4,7],[6,2]]
-- ---------------------------------------------------------------------

siembra :: [a] -> [[a]] -> [[a]]
siembra [] yss          = yss
siembra xs []           = []
siembra xs yss = [poner (xs!!(n-1)) n ys | (ys,n)<- zip yss [1 .. m]]
                 ++ drop m yss
                     where m = length xs

poner x 1 xs = x:xs
poner x n [] = []
poner x n (y:ys) = y:(poner x (n-1) ys)
-- ---------------------------------------------------------------------
-- Ejercicio 3. El siguiente tipo de dato representa expresiones
-- construidas con variables, sumas y productos
--    data Expr = Var String
--              | S Expr Expr
--              | P Expr Expr
--              deriving (Eq, Show)
-- Por ejemplo, x*(y+z) se representa por (P (V "x") (S (V "y") (V "z"))) 
-- 
-- Definir la funcion 
--    normal :: Expr -> Expr
-- tal que (normal e) es la forma normal de la expresion e obtenida
-- aplicando, mientras que sea posible, las propiedades distributivas:
--    (a+b)*c = a*c+b*c
--    c*(a+b) = c*a+c*b
-- Las expresiones x*(y*z) y x+(y*z) estan en forma normal; pero x*(y+z) y
-- (x+y)*(x+z) no. La idea es que las sumas sean exteriores.
-- Por ejemplo,
--    ghci> normal (P (S (V "x") (V "y")) (V "z"))
--    S (P (V "x") (V "z")) (P (V "y") (V "z"))
--    ghci> normal (P (V "z") (S (V "x") (V "y")))
--    S (P (V "z") (V "x")) (P (V "z") (V "y"))
--    ghci> normal (P (S (V "x") (V "y")) (S (V "u") (V "v")))
--    S (S (P (V "x") (V "u")) (P (V "x") (V "v"))) 
--      (S (P (V "y") (V "u")) (P (V "y") (V "v")))
--    ghci> normal (S (P (V "x") (V "y")) (V "z"))
--    S (P (V "x") (V "y")) (V "z")
--    ghci> normal (V "x")
--    V "x"
-- ---------------------------------------------------------------------

data Expr = V String
          | S Expr Expr
          | P Expr Expr
          deriving (Eq, Show)

normal :: Expr -> Expr
normal (V v)   = V v
normal (S a b) = S (normal a) (normal b)
normal (P a b) = p (normal a) (normal b)
    where p (S a b) c = S (p a c) (p b c)
          p a (S b c) = S (p a b) (p a c)
          p a b       = P a b
-- ---------------------------------------------------------------------
-- Ejercicio 4. Un elemento de una lista es permanente si ninguno de
-- los siguientes es mayor que el.
-- Definir, por RECURSION  Y PLEGADO la funcion 
--    permanentesR :: [Int] -> [Int]
-- tal que (permanentesR xs) es la lista de los elementos permanentes de
-- xs. Por ejemplo,
--    permanentesR [80,1,7,8,4]  ==  [80,8,4]
-- ---------------------------------------------------------------------

-- 1a definicion RECURSION:
permanentesR :: [Int] -> [Int]
permanentesR [] = []
permanentesR (x:xs) | x == maximum (x:xs) = x:permanentesR xs
                    | otherwise           = permanentesR xs

-- 2a definicion RECURSION (sin usar maximum):
permanentesR2 :: [Int] -> [Int]
permanentesR2 [] = []
permanentesR2 (x:xs) | and [x>=y|y<-xs] = x:permanentesR2 xs
                     | otherwise        = permanentesR2 xs

-- Nota: Comparacion de eficiencia
--    ghci> let xs = [1..1000] in last (permanentesR (xs ++ reverse xs))
--    1
--    (0.22 secs, 41105812 bytes)
--    ghci> let xs = [1..1000] in last (permanentesR2 (xs ++ reverse xs))
--    1
--    (0.96 secs, 31421308 bytes)

-- ---------------------------------------------------------------------
-- 3a definicion PLEGADO:
permanentesP :: [Int] -> [Int]
permanentesP = foldr f []
    where f x ys | x == maximum (x:ys) = x:ys
                 | otherwise           = ys

-- 4a definicion PLEGADO:
permanentesP2 :: [Int] -> [Int]
permanentesP2 xs = foldl f [] (reverse xs)
    where f ac x | x == maximum (x:ac) = x:ac
                 | otherwise           = ac
-- Nota: Comparacion de eficiencia
--    ghci> let xs = [1..1000] in last (permanentesP (xs ++ reverse xs))
--    1
--    (0.22 secs, 52622056 bytes)
--    ghci> let xs = [1..1000] in last (permanentesP2 (xs ++ reverse xs))
--    1
--    (0.23 secs, 52918324 bytes)
-- ---------------------------------------------------------------------