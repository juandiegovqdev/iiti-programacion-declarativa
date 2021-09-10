-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursión, la función
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

digitosR :: Integer -> [Integer]
digitosR n = reverse (digitosR' n)

digitosR' n
    | n < 10    = [n]
    | otherwise = (n `rem` 10) : digitosR' (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por comprensión, la función
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_digitos n =
    n >= 0 ==> 
    digitosR n == digitosC n
  
-- La comprobación es
--    *Main> quickCheck prop_digitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por recursión, la función 
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosR :: Integer -> Integer
sumaDigitosR n
    | n < 10    = n
    | otherwise = n `rem` 10 + sumaDigitosR (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, sin usar recursión, la función 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = sum (digitosC n)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaDigitos n =
    n >= 0 ==>
    sumaDigitosR n == sumaDigitosNR n

-- La comprobación es
--    *Main> quickCheck prop_sumaDigitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función 
--    esDigito :: Integer -> Integer -> Bool
-- tal que (esDigito x n) se verifica si x es un dígito de n. Por
-- ejemplo, 
--    esDigito 4 1041  ==  True
--    esDigito 3 1041  ==  False
-- ---------------------------------------------------------------------

esDigito :: Integer -> Integer -> Bool
esDigito x n = x `elem` digitosC n

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    numeroDeDigitos :: Integer -> Integer
-- tal que (numeroDeDigitos x) es el número de dígitos de x. Por ejemplo,
--    numeroDeDigitos 34047  ==  5
-- ---------------------------------------------------------------------

numeroDeDigitos :: Integer -> Int
numeroDeDigitos x = length (digitosC x)

-- ---------------------------------------------------------------------
-- Ejercicio 8.1 Definir, por recursión, la función 
--    listaNumeroR :: [Integer] -> Integer
-- tal que (listaNumeroR xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroR [5]        == 5
--    listaNumeroR [1,3,4,7]  == 1347
--    listaNumeroR [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroR :: [Integer] -> Integer
listaNumeroR xs = listaNumeroR' (reverse xs)

listaNumeroR' :: [Integer] -> Integer
listaNumeroR' [x]    = x
listaNumeroR' (x:xs) = x + 10 * (listaNumeroR' xs)

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Definir, por comprensión, la función 
--    listaNumeroC :: [Integer] -> Integer
-- tal que (listaNumeroC xs) es el número formado por los dígitos xs. Por
-- ejemplo, 
--    listaNumeroC [5]        == 5
--    listaNumeroC [1,3,4,7]  == 1347
--    listaNumeroC [0,0,1]    == 1
-- ---------------------------------------------------------------------

listaNumeroC :: [Integer] -> Integer
listaNumeroC xs = sum [y*10^n | (y,n) <- zip (reverse xs) [0..]]

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir, por recursión, la función 
--    pegaNumerosR :: Integer -> Integer -> Integer
-- tal que (pegaNumerosR x y) es el número resultante de "pegar" los
-- números x e y. Por ejemplo, 
--    pegaNumerosR 12 987   ==  12987
--    pegaNumerosR 1204 7   ==  12047
--    pegaNumerosR 100 100  ==  100100
-- ---------------------------------------------------------------------

pegaNumerosR :: Integer -> Integer -> Integer
pegaNumerosR x y
    | y < 10    = 10*x+y
    | otherwise = 10 * pegaNumerosR x (y `div`10) + (y `mod` 10)  

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir, sin usar recursión, la función 
--    pegaNumerosNR :: Integer -> Integer -> Integer
-- tal que (pegaNumerosNR x y) es el número resultante de "pegar" los
-- números x e y. Por ejemplo, 
--    pegaNumerosNR 12 987   ==  12987
--    pegaNumerosNR 1204 7   ==  12047
--    pegaNumerosNR 100 100  ==  100100
-- ---------------------------------------------------------------------

pegaNumerosNR :: Integer -> Integer -> Integer
pegaNumerosNR x y = listaNumeroC (digitosC x ++ digitosC y)

-- Otra definición es
pegaNumerosNR2 :: Integer -> Integer -> Integer
pegaNumerosNR2 x y = (x * (10^(numeroDeDigitos y))) + y

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Comprobar con QuickCheck que las funciones
-- pegaNumerosR y pegaNumerosNR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_pegaNumeros x y =
    x >= 0 && y >= 0 ==>
    pegaNumerosR x y == pegaNumerosNR x y

-- La comprobción es
--    *Main> quickCheck prop_pegaNumeros
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir, por recursión, la función 
--    primerDigitoR :: Integer -> Integer
-- tal que (primerDigitoR n) es el primer dígito de n. Por ejemplo, 
--    primerDigitoR 425  ==  4
-- ---------------------------------------------------------------------

primerDigitoR :: Integer -> Integer
primerDigitoR n 
    | n < 10    = n
    | otherwise = primerDigitoR (n `div` 10)
  
-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Definir, sin usar recursión, la función 
--    primerDigitoNR :: Integer -> Integer
-- tal que (primerDigitoNR n) es la primera digito de n. Por ejemplo, 
--    primerDigitoNR 425  ==  4
-- ---------------------------------------------------------------------

primerDigitoNR :: Integer -> Integer
primerDigitoNR n = head (digitosC n)

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Comprobar con QuickCheck que las funciones
-- primerDigitoR y primerDigitoNR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_primerDigito x =
    x >= 0 ==>
    primerDigitoR x == primerDigitoNR x

-- La comprobación es
--    *Main> quickCheck prop_primerDigito
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función 
--    ultimoDigito :: Integer -> Integer 
-- tal que (ultimoDigito n) es el último dígito de n. Por ejemplo, 
--    ultimoDigito 425  ==  5
-- ---------------------------------------------------------------------

ultimoDigito :: Integer -> Integer 
ultimoDigito n = n `rem` 10

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. Definir la función 
--    inverso :: Integer -> Integer
-- tal que (inverso n) es el número obtenido escribiendo los dígitos de n
-- en orden inverso. Por ejemplo, 
--    inverso 42578  ==  87524
--    inverso 203    ==    302
-- ---------------------------------------------------------------------

inverso :: Integer -> Integer
inverso n = listaNumeroC (reverse (digitosC n))

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir, usando show y read, la función 
--    inverso' :: Integer -> Integer
-- tal que (inverso' n) es el número obtenido escribiendo los dígitos de n
-- en orden inverso'. Por ejemplo, 
--    inverso' 42578  ==  87524
--    inverso' 203    ==    302
-- ---------------------------------------------------------------------

inverso' :: Integer -> Integer
inverso' n = read (reverse (show n))

-- ---------------------------------------------------------------------
-- Ejercicio 12.3. Comprobar con QuickCheck que las funciones
-- inverso e inverso' son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_inverso n =
    n >= 0 ==>
    inverso n == inverso' n

-- La comprobación es
--    *Main> quickCheck prop_inverso
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función 
--    capicua :: Integer -> Bool
-- tal que (capicua n) se verifica si si los dígitos que n son las mismas
-- de izquierda a derecha que de derecha a izquierda. Por ejemplo,
--    capicua 1234  =  False
--    capicua 1221  =  True
--    capicua 4     =  True
-- ---------------------------------------------------------------------

capicua :: Integer -> Bool
capicua n = n == inverso n

-- Informática (1º del Grado en Matemáticas)
-- Examen de la 3º convocatoria (20 de noviembre de 2012)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Definir la función
--    mayorProducto :: Int -> [Int] -> Int
-- tal que (mayorProducto n xs) es el mayor producto de una sublista de
-- xs de longitud n. Por ejemplo,
--    mayorProducto 3 [3,2,0,5,4,9,1,3,7]  ==  180
-- ya que de todas las sublistas de longitud 3 de [3,2,0,5,4,9,1,3,7] la
-- que tiene mayor producto es la [5,4,9] cuyo producto es 180.
-- ---------------------------------------------------------------------          
          
mayorProducto :: Int -> [Int] -> Int
mayorProducto n cs 
    | length cs < n = 1
    | otherwise     = maximum [product xs | xs <- segmentos n cs]
   where segmentos n cs = [take n xs | xs <- tails cs]

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    sinDobleCero :: Int -> [[Int]]
-- tal que (sinDobleCero n) es la lista de las listas de longitud n
-- formadas por el 0 y el 1 tales que no contiene dos ceros
-- consecutivos. Por ejemplo,
--    ghci> sinDobleCero 2
--    [[1,0],[1,1],[0,1]]
--    ghci> sinDobleCero 3
--    [[1,1,0],[1,1,1],[1,0,1],[0,1,0],[0,1,1]]
--    ghci> sinDobleCero 4
--    [[1,1,1,0],[1,1,1,1],[1,1,0,1],[1,0,1,0],[1,0,1,1],
--     [0,1,1,0],[0,1,1,1],[0,1,0,1]]
-- ---------------------------------------------------------------------

sinDobleCero :: Int -> [[Int]]
sinDobleCero 0 = [[]]
sinDobleCero 1 = [[0],[1]]
sinDobleCero n = [1:xs | xs <- sinDobleCero (n-1)] ++
                 [0:1:ys | ys <- sinDobleCero (n-2)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] La sucesión A046034 de la OEIS (The On-Line
-- Encyclopedia of Integer Sequences) está formada por los números tales
-- que todos sus dígitos son primos. Los primeros términos de A046034
-- son 
--    2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223
-- 
-- Definir la constante
--    numerosDigitosPrimos :: [Int]
-- cuyos elementos son los términos de la sucesión A046034. Por ejemplo,
--    ghci> take 22 numerosDigitosPrimos
--    [2,3,5,7,22,23,25,27,32,33,35,37,52,53,55,57,72,73,75,77,222,223]
-- ¿Cuántos elementos hay en la sucesión menores que 2013?
-- ---------------------------------------------------------------------

numerosDigitosPrimos :: [Int]
numerosDigitosPrimos = 
    [n | n <- [2..], digitosPrimos n]

-- (digitosPrimos n) se verifica si todos los dígitos de n son
-- primos. Por ejemplo,
--    digitosPrimos 352  ==  True
--    digitosPrimos 362  ==  False
digitosPrimos :: Int -> Bool
digitosPrimos n = all (`elem` "2357") (show n)

-- 2ª definición de digitosPrimos:
digitosPrimos2 :: Int -> Bool
digitosPrimos2 n = subconjunto (cifras n) [2,3,5,7]

-- (cifras n) es la lista de las cifras de n. Por ejemplo,
cifras :: Int -> [Int]
cifras n = [read [x] | x <-show n]

-- (subconjunto xs ys) se verifica si xs es un subconjunto de ys. Por
-- ejemplo, 
subconjunto :: Eq a => [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x <- xs]

-- El cálculo es
--    ghci> length (takeWhile (<2013) numerosDigitosPrimos)
--    84

-- ----------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Entre dos matrices de la misma dimensión se
-- puede aplicar distintas operaciones binarias entre los elementos en
-- la misma posición. Por ejemplo, si a y b son las matrices
--    |3 4 6|     |1 4 2|
--    |5 6 7|     |2 1 2|
-- entonces a+b y a-b son, respectivamente
--    |4 8 8|     |2 0 4|
--    |7 7 9|     |3 5 5|
-- 
-- Las matrices enteras se pueden representar mediante tablas con
-- índices enteros: 
--    type Matriz = Array (Int,Int) Int
-- y las matrices anteriores se definen por
--    a, b :: Matriz
--    a = listArray ((1,1),(2,3)) [3,4,6,5,6,7]
--    b = listArray ((1,1),(2,3)) [1,4,2,2,1,2]
-- 
-- Definir la función
--    opMatriz :: (Int -> Int -> Int) -> Matriz -> Matriz -> Matriz
-- tal que (opMatriz f p q) es la matriz obtenida aplicando la operación
-- f entre los elementos de p y q de la misma posición. Por ejemplo,
--    ghci> opMatriz (+) a b
--    array ((1,1),(2,3)) [((1,1),4),((1,2),8),((1,3),8),
--                         ((2,1),7),((2,2),7),((2,3),9)]
--    ghci> opMatriz (-) a b
--    array ((1,1),(2,3)) [((1,1),2),((1,2),0),((1,3),4),
--                         ((2,1),3),((2,2),5),((2,3),5)]
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

a, b :: Matriz
a = listArray ((1,1),(2,3)) [3,4,6,5,6,7]
b = listArray ((1,1),(2,3)) [1,4,2,2,1,2]

-- 1ª definición
opMatriz :: (Int -> Int -> Int) -> Matriz -> Matriz -> Matriz
opMatriz f p q = 
    array ((1,1),(m,n)) [((i,j), f (p!(i,j)) (q!(i,j)))
                      | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p

-- 2ª definición
opMatriz2 :: (Int -> Int -> Int) -> Matriz -> Matriz -> Matriz
opMatriz2 f p q = 
    listArray (bounds p) [f x y | (x,y) <- zip (elems p) (elems q)]

-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Las expresiones aritméticas se pueden definir
-- usando el siguiente tipo de datos
--    data Expr = N Int 
--              | X 
--              | S Expr Expr 
--              | R Expr Expr 
--              | P Expr Expr 
--              | E Expr Int
--              deriving (Eq, Show)
-- Por ejemplo, la expresión 
--    3*x - (x+2)^7
-- se puede definir por
--    R (P (N 3) X) (E (S X (N 2)) 7)
-- 
-- Definir la función  
--    maximo :: Expr -> [Int] -> (Int,[Int])
-- tal que (maximo e xs) es el par formado por el máximo valor de la
-- expresión e para los puntos de xs y en qué puntos alcanza el
-- máximo. Por ejemplo, 
--    ghci> maximo (E (S (N 10) (P (R (N 1) X) X)) 2) [-3..3]
--    (100,[0,1])
-- ---------------------------------------------------------------------

data Expr = N Int 
          | X 
          | S Expr Expr 
          | R Expr Expr 
          | P Expr Expr 
          | E Expr Int
          deriving (Eq, Show)

maximo :: Expr -> [Int] -> (Int,[Int])
maximo e ns = (m,[n | n <- ns, valor e n == m])  
    where m = maximum [valor e n | n <- ns]

valor :: Expr -> Int -> Int
valor (N x) _ = x
valor X     n = n
valor (S e1 e2) n = (valor e1 n) + (valor e2 n)
valor (R e1 e2) n = (valor e1 n) - (valor e2 n)
valor (P e1 e2) n = (valor e1 n) * (valor e2 n)
valor (E e  m ) n = (valor e  n)^m

-- Informática (1º del Grado en Matemáticas)
-- Examen de la 2ª convocatoria (13 de septiembre de 2013)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. [1 punto] Las notas se pueden agrupar de distinta
-- formas. Una es por la puntuación; por ejemplo,
--    [(4,["juan","ana"]),(9,["rosa","luis","mar"])]
-- Otra es por nombre; por ejemplo,
--    [("ana",4),("juan",4),("luis",9),("mar",9),("rosa",9)]
-- 
-- Definir la función
--    transformaPaN :: [(Int,[String])] -> [(String,Int)]
-- tal que (transformaPaN xs) es la agrupación de notas por nombre
-- correspondiente a la agrupación de notas por puntuación xs. Por
-- ejemplo, 
--    > transformaPaN [(4,["juan","ana"]),(9,["rosa","luis","mar"])]
--    [("ana",4),("juan",4),("luis",9),("mar",9),("rosa",9)]
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
transformaPaN :: [(Int,[String])] -> [(String,Int)]
transformaPaN xs = sort [(a,n) | (n,as) <- xs, a <- as]

-- 2ª definición (por recursión):
transformaPaN2 :: [(Int,[String])] -> [(String,Int)]
transformaPaN2 []          = []
transformaPaN2 ((n,xs):ys) = [(x,n)|x<-xs] ++ transformaPaN2 ys

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. [1 punto] Definir la función
--    transformaNaP :: [(String,Int)] -> [(Int,[String])] 
-- tal que (transformaPaN xs) es la agrupación de notas por puntuación
-- correspondiente a la agrupación de notas por nombre xs. Por
-- ejemplo, 
--    > transformaNaP [("ana",4),("juan",4),("luis",9),("mar",9),("rosa",9)]
--    [(4,["ana","juan"]),(9,["luis","mar","rosa"])]
-- ---------------------------------------------------------------------

transformaNaP :: [(String,Int)] -> [(Int,[String])] 
transformaNaP xs = [(n, [a | (a,n') <- xs, n' == n]) | n <- notas]
    where notas = sort (nub [n | (_,n) <- xs]) 

-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función
--    multiplosCon9 :: Integer -> [Integer]
-- tal que (multiplosCon9 n) es la lista de los múltiplos de n cuya
-- única cifra es 9. Por ejemplo,
--    take 3 (multiplosCon9 3)  ==  [9,99,999]
--    take 3 (multiplosCon9 7)  ==  [999999,999999999999,999999999999999999]
-- Calcular el menor múltiplo de 2013 formado sólo por nueves.
-- ---------------------------------------------------------------------
          
multiplosCon9 :: Integer -> [Integer]
multiplosCon9 n = [x | x <- numerosCon9, rem x n == 0]

-- numerosCon9 es la lista de los número cuyas cifras son todas iguales
-- a 9. Por ejemplo,
--    take 5 numerosCon9  ==  [9,99,999,9999,99999]
numerosCon9 :: [Integer]
numerosCon9 = [10^n-1 | n <- [1..]]

-- 2ª definición (por recursión):
numerosCon9R :: [Integer]
numerosCon9R = 9 : sig 9
    where sig x = (10*x+9) : sig (10*x+9)

-- El cálculo es
--    ghci> head (multiplosCon9 2013)
--    999999999999999999999999999999999999999999999999999999999999

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Una sucesión es suave si valor absoluto de la
-- diferencia de sus términos consecutivos es 1. Definir la función 
--    suaves :: Int -> [[Int]]
-- tal que (suaves n) es la lista de las sucesiones suaves de longitud n
-- cuyo último término es 0. Por ejemplo,
--    suaves 2  ==  [[1,0],[-1,0]]
--    suaves 3  ==  [[2,1,0],[0,1,0],[0,-1,0],[-2,-1,0]]
-- ---------------------------------------------------------------------

suaves :: Int -> [[Int]]
suaves 0 = []
suaves 1 = [[0]]
suaves n = concat [[x+1:x:xs,x-1:x:xs] | (x:xs) <- suaves (n-1)] 

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Los árboles binarios se pueden representar
-- mediante el tipo Arbol definido por 
--    data Arbol a = H a 
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, el árbol
--          1
--         / \ 
--        /   \
--       4     6
--      / \   / \
--     0   7 4   3
-- se puede definir por 
--    ej1 :: Arbol Int
--    ej1 = N 1 (N 4 (H 0) (H 7)) (N 6 (H 4) (H 3))
--
-- Definir la función
--    algunoArbol :: Arbol t -> (t -> Bool) -> Bool
-- tal que (algunoArbol a p) se verifica si algún elemento del árbol a
-- cumple la propiedad p. Por ejemplo,
--    algunoArbol ej1 (>9)  ==  False
--    algunoArbol ej1 (>5)  ==  True
-- ---------------------------------------------------------------------

data Arbol a = H a 
             | N a (Arbol a) (Arbol a)
             deriving Show

ej1 :: Arbol Int
ej1 = N 1 (N 4 (H 0) (H 7)) (N 6 (H 4) (H 3))

algunoArbol :: Arbol a -> (a -> Bool) -> Bool
algunoArbol (H x) p     = p x
algunoArbol (N x i d) p = p x || algunoArbol i p || algunoArbol d p

-- ----------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Las matrices enteras se pueden representar
-- mediante tablas con índices enteros:
--    type Matriz = Array (Int,Int) Int
-- 
-- Definir la función
--    matrizPorBloques :: Matriz -> Matriz -> Matriz -> Matriz -> Matriz
-- tal que (matrizPorBloques p1 p2 p3 p4) es la matriz cuadrada de orden
-- 2nx2n construida con las matrices cuadradas de orden nxn p1, p2 p3 y
-- p4 de forma que p1 es su bloque superior izquierda, p2 es su bloque
-- superior derecha, p3 es su bloque inferior izquierda y p4 es su bloque
-- inferior derecha. Por ejemplo, si p1, p2, p3 y p4 son las matrices
-- definidas por
--    p1, p2, p3, p4 :: Matriz
--    p1 = listArray ((1,1),(2,2)) [1,2,3,4]
--    p2 = listArray ((1,1),(2,2)) [6,5,7,8]
--    p3 = listArray ((1,1),(2,2)) [0,6,7,1]
--    p4 = listArray ((1,1),(2,2)) [5,2,8,3]
-- entonces
--    ghci> matrizPorBloques p1 p2 p3 p4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),2),((1,3),6),((1,4),5),
--                         ((2,1),3),((2,2),4),((2,3),7),((2,4),8),
--                         ((3,1),0),((3,2),6),((3,3),5),((3,4),2),
--                         ((4,1),7),((4,2),1),((4,3),8),((4,4),3)]
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

p1, p2, p3, p4 :: Matriz
p1 = listArray ((1,1),(2,2)) [1,2,3,4]
p2 = listArray ((1,1),(2,2)) [6,5,7,8]
p3 = listArray ((1,1),(2,2)) [0,6,7,1]
p4 = listArray ((1,1),(2,2)) [5,2,8,3]

matrizPorBloques :: Matriz -> Matriz -> Matriz -> Matriz -> Matriz
matrizPorBloques p1 p2 p3 p4 =
    array ((1,1),(m,m)) [((i,j), f i j) | i <- [1..m], j <- [1..m]]
    where ((_,_),(n,_)) = bounds p1
          m = 2*n
          f i j | i <= n && j <= n = p1!(i,j)
                | i <= n && j >  n = p2!(i,j-n)
                | i >  n && j <= n = p3!(i-n,j)
                | i >  n && j >  n = p4!(i-n,j-n)

-- Informática (1º del Grado en Matemáticas)
-- Examen de la 1ª convocatoria del curso (3 de julio de 2013)
-- ---------------------------------------------------------------------

import Data.List
import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Dos listas son cíclicamente iguales si tienen
-- el mismo número de elementos en el mismo orden. Por ejemplo, son
-- cíclicamente iguales los siguientes pares de listas
--    [1,2,3,4,5] y [3,4,5,1,2],
--    [1,1,1,2,2] y [2,1,1,1,2],
--    [1,1,1,1,1] y [1,1,1,1,1]
-- pero no lo son
--    [1,2,3,4] y [1,2,3,5],
--    [1,1,1,1] y [1,1,1],
--    [1,2,2,1] y [2,2,1,2]
-- Definir la función 
--    iguales :: Eq a => [a] -> [a] -> Bool
-- tal que (iguales xs ys) se verifica si xs es ys son cíclicamente
-- iguales. Por ejemplo,
--    iguales [1,2,3,4,5] [3,4,5,1,2]  ==  True
--    iguales [1,1,1,2,2] [2,1,1,1,2]  ==  True
--    iguales [1,1,1,1,1] [1,1,1,1,1]  ==  True
--    iguales [1,2,3,4] [1,2,3,5]      ==  False
--    iguales [1,1,1,1] [1,1,1]        ==  False
--    iguales [1,2,2,1] [2,2,1,2]      ==  False
-- ---------------------------------------------------------------------

-- 1ª solución
-- ===========

iguales1 :: Ord a => [a] -> [a] -> Bool
iguales1 xs ys = 
    permutacionApares xs == permutacionApares ys

-- (permutacionApares xs) es la lista ordenada de los pares de elementos
-- consecutivos de elementos de xs. Por ejemplo,
--    permutacionApares [2,1,3,5,4]  ==  [(1,3),(2,1),(3,5),(4,2),(5,4)]
permutacionApares :: Ord a => [a] -> [(a, a)]
permutacionApares xs = 
    sort (zip xs (tail xs) ++ [(last xs, head xs)])

-- 2ª solucion
-- ===========

-- (iguales2 xs ys) se verifica si las listas xs e ys son cíclicamente
-- iguales. Por ejemplo,
iguales2 :: Eq a => [a] -> [a] -> Bool
iguales2 xs ys = 
    elem ys (ciclos xs)

-- (ciclo xs) es la lista obtenida pasando el último elemento de xs al
-- principio. Por ejemplo,
--    ciclo [2,1,3,5,4]  ==  [4,2,1,3,5]
ciclo :: [a] -> [a]
ciclo xs = (last xs): (init xs)

-- (kciclo k xs) es la lista obtenida pasando los k últimos elementos de
-- xs al principio. Por ejemplo, 
--    kciclo 2 [2,1,3,5,4]  ==  [5,4,2,1,3]
kciclo :: (Eq a, Num a) => a -> [a1] -> [a1]
kciclo 1 xs = ciclo xs
kciclo k xs = kciclo (k-1) (ciclo xs)

-- (ciclos xs) es la lista de las listas cíclicamente iguales a xs. Por
-- ejemplo, 
--    ghci> ciclos [2,1,3,5,4]
--    [[4,2,1,3,5],[5,4,2,1,3],[3,5,4,2,1],[1,3,5,4,2],[2,1,3,5,4]]
ciclos :: [a] -> [[a]]
ciclos xs = [kciclo k xs | k <- [1..length xs]]

-- 3º solución
-- ===========

iguales3 :: Eq a => [a] -> [a] -> Bool
iguales3 xs ys = 
    length xs == length ys && isInfixOf xs (ys ++ ys)

-- ---------------------------------------------------------------------
-- Ejercicio ?. Un número natural n es casero respecto de f si las
-- cifras de f(n) es una sublista de las de n. Por ejemplo,
-- * 1234 es casero repecto de resto de dividir por 173, ya que el resto
--   de dividir 1234 entre 173 es 23 que es una sublista de 1234; 
-- * 1148 es casero respecto de la suma de cifras, ya que la suma de las
--   cifras de 1148 es 14 que es una sublista de 1148.
-- Definir la función
--    esCasero :: (Integer -> Integer) -> Integer -> Bool
-- tal que (esCasero f x) se verifica si x es casero respecto de f. Por
-- ejemplo,
--    esCasero (\x -> rem x 173) 1234  ==  True
--    esCasero (\x -> rem x 173) 1148  ==  False
--    esCasero sumaCifras 1148         ==  True
--    esCasero sumaCifras 1234         ==  False
-- donde (sumaCifras n) es la suma de las cifras de n.
--
-- ¿Cuál es el menor número casero respecto de la suma de cifras mayor
-- que 2013? 
-- ---------------------------------------------------------------------

esCasero :: (Integer -> Integer) -> Integer -> Bool
esCasero f x =
    esSublista2 (cifras (f x)) (cifras x)

-- (esSublista xs ys) se verifica si xs es una sublista de ys; es decir,
-- si existen dos listas as y bs tales que 
--    ys = as ++ xs ++ bs
esSublista :: Eq a => [a] -> [a] -> Bool
esSublista = isInfixOf

-- Se puede definir por
esSublista2 :: Eq a => [a] -> [a] -> Bool
esSublista2 xs ys = 
    or [esPrefijo xs zs | zs <- sufijos ys]

-- (esPrefijo xs ys) se verifica si xs es un prefijo de ys. Por 
-- ejemplo,
--    esPrefijo "ab" "abc"  ==  True
--    esPrefijo "ac" "abc"  ==  False
--    esPrefijo "bc" "abc"  ==  False
esPrefijo :: Eq a => [a] -> [a] -> Bool
esPrefijo [] _          = True
esPrefijo _  []         = False
esPrefijo (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- (sufijos xs) es la lista de sufijos de xs. Por ejemplo,
--    sufijos "abc"  ==  ["abc","bc","c",""]
sufijos :: [a] -> [[a]]
sufijos xs = [drop i xs | i <- [0..length xs]]

-- (cifras x) es la lista de las cifras de x. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras x = [read [d] | d <- show x]

-- (sumaCifras x) es la suma de las cifras de x. Por ejemplo,
--    sumaCifras 325  ==  10
sumaCifras :: Integer -> Integer
sumaCifras = sum . cifras

-- El cálculo del menor número casero respecto de la suma mayor que 2013
-- es 
--    ghci> head [n | n <- [2014..], esCasero sumaCifras n]
--    2099

-- ---------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Definir la función
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion xs ys) es la intersección de las dos listas,
-- posiblemente infinitas, ordenadas de menor a mayor xs e ys. Por ejemplo,
--    take 5 (interseccion [2,4..] [3,6..])  ==  [6,12,18,24,30]
-- ---------------------------------------------------------------------

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion (x:xs) (y:ys)
    | x == y    = x : interseccion xs ys
    | x < y     = interseccion (dropWhile (<y) xs) (y:ys)
    | otherwise = interseccion (x:xs) (dropWhile (<x) ys)

-- ---------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Los árboles binarios se pueden representar
-- mediante el tipo Arbol definido por 
--    data Arbol = H Int 
--               | N Int Arbol Arbol
-- Por ejemplo, el árbol
--         1
--        / \ 
--       /   \
--      2     5
--     / \   / \
--    3   4 6   7
-- se puede representar por 
--    N 1 (N 2 (H 3) (H 4)) (N 5 (H 6) (H 7))
-- Definir la función
--    esSubarbol :: Arbol -> Arbol -> Bool
-- tal que (esSubarbol a1 a2) se verifica si a1 es un subárbol de
-- a2. Por ejemplo,
--    esSubarbol (H 2) (N 2 (H 2) (H 4))              ==  True
--    esSubarbol (H 5) (N 2 (H 2) (H 4))              ==  False
--    esSubarbol (N 2 (H 2) (H 4)) (N 2 (H 2) (H 4))  ==  True
--    esSubarbol (N 2 (H 4) (H 2)) (N 2 (H 2) (H 4))  ==  False
-- ---------------------------------------------------------------------

data Arbol= H Int
          | N Int Arbol Arbol

esSubarbol :: Arbol -> Arbol -> Bool
esSubarbol (H x) (H y) = x == y
esSubarbol a@(H x) (N y i d) = esSubarbol a i || esSubarbol a d
esSubarbol (N _ _ _) (H _) = False
esSubarbol a@(N r1 i1 d1) (N r2 i2 d2) 
    | r1 == r2  = (igualArbol i1 i2 && igualArbol d1 d2) ||
                  esSubarbol a i2 || esSubarbol a d2
    | otherwise = esSubarbol a i2 || esSubarbol a d2

-- (igualArbol a1 a2) se verifica si los árboles a1 y a2 son iguales.
igualArbol :: Arbol -> Arbol -> Bool
igualArbol (H x) (H y) = x == y
igualArbol (N r1 i1 d1) (N r2 i2 d2) = 
    r1 == r2 && igualArbol i1 i2 && igualArbol d1 d2
igualArbol _ _ = False


-- ---------------------------------------------------------------------
-- Ejercicio 5. [2 puntos] Las matrices enteras se pueden representar
-- mediante tablas con índices enteros:
--    type Matriz = Array (Int,Int) Int
-- Por ejemplo, las matrices          
--    | 1  2  3  4  5 |    | 1 2 3 |
--    | 2  6  8  9  4 |    | 2 6 8 |
--    | 3  8  0  8  3 |    | 3 8 0 |
--    | 4  9  8  6  2 |
--    | 5  4  3  2  1 |
-- se puede definir por
--    ejM1, ejM2 :: Matriz
--    ejM1 = listArray ((1,1),(5,5)) [1,2,3,4,5,
--                                    2,6,8,9,4,
--                                    3,8,0,8,3,
--                                    4,9,8,6,2,
--                                    5,4,3,2,1]
--    
--    ejM2 = listArray ((1,1),(3,3)) [1,2,3,
--                                    2,6,8,
--                                    3,8,0]
-- Una matriz cuadrada es bisimétrica si es simétrica respecto de su
-- diagonal principal y de su diagonal secundaria. Definir la función
--    esBisimetrica :: Matriz -> Bool
-- tal que (esBisimetrica p) se verifica si p es bisimétrica. Por
-- ejemplo,           
--    esBisimetrica ejM1  ==  True
--    esBisimetrica ejM2  ==  False
-- ---------------------------------------------------------------------

type Matriz = Array (Int,Int) Int

ejM1, ejM2 :: Matriz
ejM1 = listArray ((1,1),(5,5)) [1,2,3,4,5,
                                2,6,8,9,4,
                                3,8,0,8,3,
                                4,9,8,6,2,
                                5,4,3,2,1]

ejM2 = listArray ((1,1),(3,3)) [1,2,3,
                                2,6,8,
                                3,8,0]

-- 1ª definición:
esBisimetrica :: Matriz -> Bool
esBisimetrica p =
    and [p!(i,j) == p!(j,i) | i <- [1..n], j <- [1..n]] &&
    and [p!(i,j) == p!(n+1-j,n+1-i) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- 2ª definición:
esBisimetrica2 :: Matriz -> Bool
esBisimetrica2 p = p == simetrica p && p == simetricaS p
        
-- (simetrica p) es la simétrica de la matriz p respecto de la diagonal
-- principal. Por ejemplo,
--    ghci> simetrica (listArray ((1,1),(4,4)) [1..16])
--    array ((1,1),(4,4)) [((1,1),1),((1,2),5),((1,3), 9),((1,4),13),
--                         ((2,1),2),((2,2),6),((2,3),10),((2,4),14),
--                         ((3,1),3),((3,2),7),((3,3),11),((3,4),15),
--                         ((4,1),4),((4,2),8),((4,3),12),((4,4),16)]
simetrica :: Matriz -> Matriz
simetrica p =
    array ((1,1),(n,n)) [((i,j),p!(j,i)) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

-- (simetricaS p) es la simétrica de la matriz p respecto de la diagonal
-- secundaria. Por ejemplo,
--    ghci> simetricaS (listArray ((1,1),(4,4)) [1..16])
--    array ((1,1),(4,4)) [((1,1),16),((1,2),12),((1,3),8),((1,4),4),
--                         ((2,1),15),((2,2),11),((2,3),7),((2,4),3),
--                         ((3,1),14),((3,2),10),((3,3),6),((3,4),2),
--                         ((4,1),13),((4,2), 9),((4,3),5),((4,4),1)]
simetricaS :: Matriz -> Matriz
simetricaS p =
    array ((1,1),(n,n)) [((i,j),p!(n+1-j,n+1-i)) | i <- [1..n], j <- [1..n]]
    where ((_,_),(n,_)) = bounds p

import Data.Array

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 puntos] Un número es creciente si cada una de sus
-- cifras es mayor o igual que su anterior. Definir la función
--    numerosCrecientes :: [Integer] -> [Integer]
-- tal que (numerosCrecientes xs) es la lista de los números crecientes
-- de xs. Por ejemplo,
--    ghci> numerosCrecientes [21..50]
--    [22,23,24,25,26,27,28,29,33,34,35,36,37,38,39,44,45,46,47,48,49]
-- Usando la definición de numerosCrecientes calcular la cantidad de
-- números crecientes de 3 cifras.
-- ---------------------------------------------------------------------

-- 1ª definición (por comprensión):
numerosCrecientes :: [Integer] -> [Integer]
numerosCrecientes xs = [n | n <- xs, esCreciente (cifras n)]

-- (esCreciente xs) se verifica si xs es una sucesión cerciente. Por
-- ejemplo, 
--    esCreciente [3,5,5,12]  ==  True
--    esCreciente [3,5,4,12]  ==  False
esCreciente :: Ord a => [a] -> Bool
esCreciente (x:y:zs) = x <= y && esCreciente (y:zs) 
esCreciente _        = True

-- (cifras x) es la lista de las cifras del número x. Por ejemplo,
--    cifras 325  ==  [3,2,5]
cifras :: Integer -> [Integer]
cifras x = [read [d] | d <- show x]         

-- El cálculo es
--    ghci> length (numerosCrecientes [100..999])
--    165

-- 2ª definición (por filtrado):  
numerosCrecientes2 :: [Integer] -> [Integer]
numerosCrecientes2 = filter (\n -> esCreciente (cifras n))

-- 3ª definición (por recursión):
numerosCrecientes3 :: [Integer] -> [Integer]
numerosCrecientes3 [] = []
numerosCrecientes3 (n:ns)
  | esCreciente (cifras n) = n : numerosCrecientes3 ns
  | otherwise              = numerosCrecientes3 ns

-- 4ª definición (por plegado):
numerosCrecientes4 :: [Integer] -> [Integer]
numerosCrecientes4 = foldr f []
  where f n ns | esCreciente (cifras n) = n : ns
               | otherwise              = ns
          
-- ---------------------------------------------------------------------
-- Ejercicio 2. [2 puntos] Definir la función
--    sublistasIguales :: Eq a => [a] -> [[a]]
-- tal que (sublistasIguales xs) es la listas de elementos consecutivos
-- de xs que son iguales. Por ejemplo,
--    ghci> sublistasIguales [1,5,5,10,7,7,7,2,3,7] 
--    [[1],[5,5],[10],[7,7,7],[2],[3],[7]]
-- ---------------------------------------------------------------------

-- 1ª definición:
sublistasIguales :: Eq a => [a] -> [[a]]
sublistasIguales [] = []
sublistasIguales (x:xs) =
  (x : takeWhile (==x) xs) : sublistasIguales (dropWhile (==x) xs)

-- 2ª definición:
sublistasIguales2 :: Eq a => [a] -> [[a]]
sublistasIguales2 []     = []
sublistasIguales2 [x]    = [[x]]
sublistasIguales2 (x:y:zs)
  | x == u    = (x:u:us):vss
  | otherwise = [x]:((u:us):vss)           
  where ((u:us):vss) = sublistasIguales2 (y:zs)

-- ----------------------------------------------------------------------
-- Ejercicio 3. [2 puntos] Los árboles binarios se pueden representar
-- con el de dato algebraico
--    data Arbol a = H
--                 | N a (Arbol a) (Arbol a)
--                 deriving Show
-- Por ejemplo, los árboles 
--         9                9                
--        / \              / \    
--       /   \            /   \   
--      8     6          8     6  
--     / \   / \        / \   / \ 
--    3   2 4   5      3   2 4   7
-- se pueden representar por
--    ej1, ej2:: Arbol Int
--    ej1 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 5 H H))
--    ej2 = N 9 (N 8 (N 3 H H) (N 2 H H)) (N 6 (N 4 H H) (N 7 H H))
-- Un árbol binario ordenado es un árbol binario (ABO) en el que los
-- valores de cada nodo es mayor o igual que los valores de sus
-- hijos. Por ejemplo, ej1 es un ABO, pero ej2 no lo es. 
-- 
-- Definir la función esABO
--    esABO :: Ord t => Arbol t -> Bool
-- tal que (esABO a) se verifica si a es un árbol binario ordenado. Por
-- ejemplo. 
--    esABO ej1 == True
--    esABO ej2 == False    
-- ---------------------------------------------------------------------

data Arbol a = H
             | N a (Arbol a) (Arbol a)
             deriving Show

ej1, ej2 :: Arbol Int
ej1 = N 9 (N 8 (N 3 H H) (N 2 H H))
          (N 6 (N 4 H H) (N 5 H H))

ej2 = N 9 (N 8 (N 3 H H) (N 2 H H))
          (N 6 (N 4 H H) (N 7 H H))

-- 1ª definición
esABO :: Ord a => Arbol a -> Bool
esABO H                       = True
esABO (N x H H)               = True
esABO (N x m1@(N x1 a1 b1) H) = x >= x1 && esABO m1
esABO (N x H m2@(N x2 a2 b2)) = x >= x2 && esABO m2
esABO (N x m1@(N x1 a1 b1) m2@(N x2 a2 b2)) = 
      x >= x1 && esABO m1 && x >= x2 && esABO m2

-- 2ª definición
esABO2 :: Ord a => Arbol a -> Bool
esABO2 H         = True
esABO2 (N x i d) = mayor x i && mayor x d && esABO2 i && esABO2 d 
       where  mayor x H         = True
              mayor x (N y _ _) = x >= y
    
-- ----------------------------------------------------------------------
-- Ejercicio 4. [2 puntos] Definir la función 
--    paresEspecialesDePrimos :: Integer -> [(Integer,Integer)]
-- tal que (paresEspecialesDePrimos n) es la lista de los pares de
-- primos (p,q) tales que p < q y q-p es divisible por n. Por ejemplo,
--    ghci> take 9 (paresEspecialesDePrimos 2)
--    [(3,5),(3,7),(5,7),(3,11),(5,11),(7,11),(3,13),(5,13),(7,13)]
--    ghci> take 9 (paresEspecialesDePrimos 3)
--    [(2,5),(2,11),(5,11),(7,13),(2,17),(5,17),(11,17),(7,19),(13,19)]
-- ---------------------------------------------------------------------

paresEspecialesDePrimos :: Integer -> [(Integer,Integer)]
paresEspecialesDePrimos n =
  [(p,q) | (p,q) <- paresPrimos, rem (q-p) n == 0]

-- paresPrimos es la lista de los pares de primos (p,q) tales que p < q. 
-- Por ejemplo,
--    ghci> take 9 paresPrimos
--    [(2,3),(2,5),(3,5),(2,7),(3,7),(5,7),(2,11),(3,11),(5,11)]
paresPrimos :: [(Integer,Integer)]
paresPrimos = [(p,q) | q <- primos, p <- takeWhile (<q) primos]

-- primos es la lista de primos. Por ejemplo,
--    take 9 primos  ==  [2,3,5,7,11,13,17,19,23]
primos :: [Integer]
primos = criba [2..]

criba :: [Integer] -> [Integer]
criba (p:xs) = p : criba [x | x <- xs, x `mod` p /= 0]

-- ----------------------------------------------------------------------
-- Ejercicio 5. Las matrices enteras se pueden representar mediante
-- tablas con índices enteros:
--    type Matriz = Array (Int,Int) Int
-- 
-- Definir la función
--    ampliaColumnas :: Matriz -> Matriz -> Matriz
-- tal que (ampliaColumnas p q) es la matriz construida añadiendo las
-- columnas de la matriz q a continuación de las de p (se supone que
-- tienen el mismo número de filas). Por ejemplo, si p y q representa
-- las dos primeras matrices, entonces (ampliaColumnas p q) es la
-- tercera  
--    |0 1|    |4 5 6|    |0 1 4 5 6| 
--    |2 3|    |7 8 9|    |2 3 7 8 9|
-- --------------------------------------------------------------------- 

type Matriz = Array (Int,Int) Int

ampliaColumnas :: Matriz -> Matriz -> Matriz
ampliaColumnas p1 p2 =
  array ((1,1),(m,n1+n2)) [((i,j), f i j) | i <- [1..m], j <- [1..n1+n2]]
    where ((_,_),(m,n1)) = bounds p1
          ((_,_),(_,n2)) = bounds p2
          f i j | j <= n1   = p1!(i,j)
                | otherwise = p2!(i,j-n1) 

-- Ejemplo
--    ghci> let p = listArray ((1,1),(2,2)) [0..3] :: Matriz
--    ghci> let q = listArray ((1,1),(2,3)) [4..9] :: Matriz
--    ghci> ampliaColumnas p q
--    array ((1,1),(2,5)) 
--          [((1,1),0),((1,2),1),((1,3),4),((1,4),5),((1,5),6),
--           ((2,1),2),((2,2),3),((2,3),7),((2,4),8),((2,5),9)]

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de los grafos, utilizando las implementaciones estudiadas
-- en el tema 22 que se pueden descargar desde 
--    http://www.cs.us.es/~jalonso/cursos/i1m-12/codigos.zip
--
-- Las transparencias del tema 22 se encuentran en
--    http://www.cs.us.es/~jalonso/cursos/i1m-12/temas/tema-22.pdf

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Array
import Data.List (nub)
import Test.QuickCheck

-- Hay que seleccionar una implementación del TAD de los grafos
import GrafoConVectorDeAdyacencia 
-- import GrafoConMatrizDeAdyacencia 
-- import Rel_29_sol

-- ---------------------------------------------------------------------
-- Ejemplos                                                           --
-- ---------------------------------------------------------------------

-- Para los ejemplos se usarán los siguientes grafos.
g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11 :: Grafo Int Int
g1 = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                         (2,4,55),(2,5,32),
                         (3,4,61),(3,5,44),
                         (4,5,93)]
g2 = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                        (2,4,55),(2,5,32),
                        (4,3,61),(4,5,93)]
g3 = creaGrafo D (1,3) [(1,2,0),(2,2,0),(3,1,0),(3,2,0)]
g4 = creaGrafo D (1,4) [(1,2,3),(2,1,5)]
g5 = creaGrafo D (1,1) [(1,1,0)]
g6 = creaGrafo D (1,4) [(1,3,0),(3,1,0),(3,3,0),(4,2,0)]
g7 = creaGrafo ND (1,4) [(1,3,0)]
g8 = creaGrafo D (1,5) [(1,1,0),(1,2,0),(1,3,0),(2,4,0),(3,1,0),
                        (4,1,0),(4,2,0),(4,4,0),(4,5,0)]
g9 = creaGrafo D (1,5) [(4,1,1),(4,3,2),(5,1,0)]
g10 = creaGrafo ND (1,3) [(1,2,1),(1,3,1),(2,3,1),(3,3,1)]
g11 = creaGrafo D (1,3) [(1,2,1),(1,3,1),(2,3,1),(3,3,1)]

-- ---------------------------------------------------------------------
-- Ejercicio 1. El grafo completo de orden n, K(n), es un grafo no
-- dirigido cuyos conjunto de vértices es {1,..n} y tiene una arista
-- entre par de vértices distintos. Definir la función,
--    completo :: Int -> Grafo Int Int
-- tal que (completo n) es el grafo completo de orden n. Por ejemplo,
--    ghci> completo 4
--    G ND (array (1,4) [(1,[(2,0),(3,0),(4,0)]),
--                       (2,[(1,0),(3,0),(4,0)]),
--                       (3,[(1,0),(2,0),(4,0)]),
--                       (4,[(1,0),(2,0),(3,0)])])
-- ---------------------------------------------------------------------

completo :: Int -> Grafo Int Int
completo n = creaGrafo ND (1,n) xs
    where xs = [(x,y,0) | x <- [1..n], y <- [1..n], x < y]

completo' :: Int -> Grafo Int Int
completo' n = creaGrafo ND (1,n) [(a,b,0)|a<-[1..n],b<-[1..a-1]]
 
-- ---------------------------------------------------------------------
-- Ejercicio 2. El ciclo de orden n, C(n), es un grafo no dirigido
-- cuyo conjunto de vértices es {1,...,n} y las aristas son
--    (1,2), (2,3), ..., (n-1,n), (n,1)
-- Definir la función
--    grafoCiclo :: Int -> Grafo Int Int
-- tal que (grafoCiclo n) es el grafo ciclo de orden n. Por ejemplo,
--    ghci> grafoCiclo 3
--    G ND (array (1,3) [(1,[(3,0),(2,0)]),(2,[(1,0),(3,0)]),(3,[(2,0),(1,0)])])
-- ---------------------------------------------------------------------

grafoCiclo :: Int -> Grafo Int Int
grafoCiclo n = creaGrafo ND (1,n) xs
    where xs = [(x,x+1,0) | x <- [1..n-1]] ++ [(n,1,0)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    nVertices :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nVertices g) es el número de vértices del grafo g. Por
-- ejemplo, 
--    nVertices (completo 4)  ==  4
--    nVertices (completo 5)  ==  5
-- ---------------------------------------------------------------------

nVertices :: (Ix v,Num p) => Grafo v p ->  Int
nVertices = length . nodos

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    noDirigido :: (Ix v,Num p) => Grafo v p ->  Bool
-- tal que (noDirigido g) se verifica si el grafo g es no dirigido. Por
-- ejemplo, 
--    noDirigido g1            ==  True
--    noDirigido g2            ==  False
--    noDirigido (completo 4)  ==  True
-- ---------------------------------------------------------------------

noDirigido :: (Ix v,Num p) => Grafo v p ->  Bool
noDirigido = not . dirigido

-- ---------------------------------------------------------------------
-- Ejercicio 5. En un un grafo g, los incidentes de un vértice v es el
-- conjuntos de vértices x de g para los que hay un arco (o una arista)
-- de x a v; es decir, que v es adyacente a x. Definir la función
--    incidentes :: (Ix v,Num p) => (Grafo v p) -> v -> [v]
-- tal que (incidentes g v) es la lista de los vértices incidentes en el
-- vértice v. Por ejemplo,
--    incidentes g2 5  ==  [1,2,4]
--    adyacentes g2 5  ==  []
--    incidentes g1 5  ==  [1,2,3,4]
--    adyacentes g1 5  ==  [1,2,3,4]
-- --------------------------------------------------------------------- 

incidentes :: (Ix v,Num p) => Grafo v p -> v -> [v]
incidentes g v = [x | x <- nodos g, v `elem` adyacentes g x]

-- ---------------------------------------------------------------------
-- Ejercicio 6. En un un grafo g, los contiguos de un vértice v es el
-- conjuntos de vértices x de g tales que x es adyacente o incidente con
-- v. Definir la función
--    contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
-- tal que (contiguos g v) es el conjunto de los vértices de g contiguos
-- con el vértice v. Por ejemplo,
--    contiguos g2 5  ==  [1,2,4]
--    contiguos g1 5  ==  [1,2,3,4]
-- ---------------------------------------------------------------------

contiguos :: (Ix v,Num p) => Grafo v p -> v -> [v]
contiguos g v = nub (adyacentes g v ++ incidentes g v)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
-- tal que (lazos g) es el conjunto de los lazos (es decir, aristas
-- cuyos extremos son iguales) del grafo g. Por ejemplo, 
--    ghci> lazos g3
--    [(2,2)]
--    ghci> lazos g2
--    []
-- ---------------------------------------------------------------------

lazos :: (Ix v,Num p) => Grafo v p -> [(v,v)]
lazos g = [(x,x) | x <- nodos g, aristaEn g (x,x)]

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    nLazos :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nLazos g) es el número de lazos del grafo g. Por
-- ejemplo, 
--    nLazos g3  ==  1
--    nLazos g2  ==  0
-- ---------------------------------------------------------------------

nLazos :: (Ix v,Num p) => Grafo v p ->  Int
nLazos = length . lazos

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    nAristas :: (Ix v,Num p) => Grafo v p ->  Int
-- tal que (nAristas g) es el número de aristas del grafo g. Si g es no
-- dirigido, las aristas de v1 a v2 y de v2 a v1 sólo se cuentan una
-- vez y los lazos se cuentan dos veces. Por ejemplo, 
--    nAristas g1            ==  8
--    nAristas g2            ==  7
--    nAristas g10           ==  4
--    nAristas (completo 4)  ==  6
--    nAristas (completo 5)  ==  10
-- ---------------------------------------------------------------------

nAristas :: (Ix v,Num p) => Grafo v p ->  Int
nAristas g 
    | dirigido g = length (aristas g)
    | otherwise  = (length (aristas g) `div` 2) + nLazos g
                          
-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    prop_nAristasCompleto :: Int -> Bool
-- tal que (prop_nAristasCompleto n) se verifica si el número de aristas
-- del grafo completo de orden n es n*(n-1)/2 y, usando la función,
-- comprobar que la propiedad se cumple para n de 1 a 20.
-- ---------------------------------------------------------------------

prop_nAristasCompleto :: Int -> Bool
prop_nAristasCompleto n =
    nAristas (completo n) == n*(n-1) `div` 2

-- La comprobación es
--    ghci> and [prop_nAristasCompleto n | n <- [1..20]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 11. El grado positivo de un vértice v de un grafo dirigido
-- g, es el número de vértices de g adyacentes con v. Definir la función  
--    gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (gradoPos g v) es el grado positivo del vértice v en el grafo
-- g. Por ejemplo,
--    gradoPos g1 5  ==  4
--    gradoPos g2 5  ==  0
--    gradoPos g2 1  ==  3
-- ---------------------------------------------------------------------

gradoPos :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoPos g v = length (adyacentes g v)

-- ---------------------------------------------------------------------
-- Ejercicio 12. El grado negativo de un vértice v de un grafo dirigido
-- g, es el número de vértices de g incidentes con v. Definir la función  
--    gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (gradoNeg g v) es el grado negativo del vértice v en el grafo
-- g. Por ejemplo,
--    gradoNeg g1 5  ==  4
--    gradoNeg g2 5  ==  3
--    gradoNeg g2 1  ==  0
-- ---------------------------------------------------------------------

gradoNeg :: (Ix v,Num p) => Grafo v p -> v -> Int
gradoNeg g v = length (incidentes g v)

-- ---------------------------------------------------------------------
-- Ejercicio 13. El grado de un vértice v de un grafo dirigido g, es el
-- número de aristas de g que contiene a v. Si g es no dirigido, el
-- grado de un vértice v es el número de aristas incidentes en v, teniendo
-- en cuenta que los lazos se cuentan dos veces. Definir la función    
--    grado :: (Ix v,Num p) => Grafo v p -> v -> Int
-- tal que (grado g v) es el grado del vértice v en el grafo g. Por
-- ejemplo, 
--    grado g1 5  ==  4
--    grado g2 5  ==  3
--    grado g2 1  ==  3 
--    grado g3 2  ==  4
--    grado g3 1  ==  2
--    grado g3 3  ==  2
--    grado g5 1  ==  3
--    grado g10 3 == 4
--    grado g11 3 == 4
-- ---------------------------------------------------------------------

grado :: (Ix v,Num p) => Grafo v p -> v -> Int
grado g v | dirigido g           = gradoNeg g v + gradoPos g v
          | (v,v) `elem` lazos g = length (incidentes g v) + 1 
          | otherwise            = length (incidentes g v)

-- ---------------------------------------------------------------------
-- Ejercicio 14. Comprobar con QuickCheck que para cualquier grafo g, la
-- suma de los grados positivos de los vértices de g es igual que la
-- suma de los grados negativos de los vértices de g.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaGrados:: Grafo Int Int -> Bool
prop_sumaGrados g = 
    sum [gradoPos g v | v <- vs] == sum [gradoNeg g v | v <- vs] 
    where vs = nodos g

-- La comprobación es
--    ghci> quickCheck prop_sumaGrados
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 15. En la teoría de grafos, se conoce como "Lema del
-- apretón de manos" la siguiente propiedad: la suma de los grados de
-- los vértices de g es el doble del número de aristas de g. 
-- Comprobar con QuickCheck que para cualquier grafo g, se verifica
-- dicha propiedad.
-- ---------------------------------------------------------------------

prop_apretonManos:: Grafo Int Int -> Bool
prop_apretonManos g = 
    sum [grado g v | v <- nodos g] == 2 * nAristas g 

-- La comprobación es
--    ghci> quickCheck prop_apretonManos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 16. Comprobar con QuickCheck que en todo grafo, el número
-- de nodos de grado impar es par. 
-- ---------------------------------------------------------------------

prop_numNodosGradoImpar :: Grafo Int Int -> Bool
prop_numNodosGradoImpar g = even m
    where vs = nodos g
          m = length [v | v <- vs, odd(grado g v)]

-- La comprobación es
--    ghci> quickCheck prop_numNodosGradoImpar
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la propiedad
--   prop_GradoCompleto :: Int -> Bool
-- tal que (prop_GradoCompleto n) se verifica si todos los vértices del
-- grafo completo K(n) tienen grado n-1. Usarla para comprobar que dicha
-- propiedad se verifica para los grafos completos de grados 1 hasta 30.
-- ---------------------------------------------------------------------

prop_GradoCompleto :: Int -> Bool
prop_GradoCompleto n = 
    and [grado g v == (n-1) | v <- nodos g]
        where g = completo n

-- La comprobación es                       
--    ghci> and [prop_GradoCompleto n | n <- [1..30]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 18. Un grafo es regular si todos sus vértices tienen el
-- mismo grado. Definir la función
--    regular :: (Ix v,Num p) => Grafo v p -> Bool
-- tal que (regular g) se verifica si todos los nodos de g tienen el
-- mismo grado. 
--    regular g1            ==  False
--    regular g2            ==  False
--    regular (completo 4)  ==  True
-- ---------------------------------------------------------------------

regular :: (Ix v,Num p) => Grafo v p -> Bool
regular g = and [grado g v == k | v <- vs]
    where vs = nodos g
          k  = grado g (head vs)

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la propiedad
--    prop_CompletoRegular :: Int -> Int -> Bool
-- tal que (prop_CompletoRegular m n) se verifica si todos los grafos
-- completos desde el de orden m hasta el de orden m son regulares y
-- usarla para comprobar que todos los grafos completo desde el de orden
-- 1 hasta el de orden 30 son regulares.
-- --------------------------------------------------------------------- 

prop_CompletoRegular :: Int -> Int -> Bool
prop_CompletoRegular m n = and [regular (completo x) | x <- [m..n]]

-- La comprobación es
--    ghci> prop_CompletoRegular 1 30
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 20. Un grafo es k-regular si todos sus vértices son de
-- grado k. Definir la función
--    regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
-- tal que (regularidad g) es la regularidad de g. Por ejemplo,
--    regularidad g1              ==  Nothing
--    regularidad (completo 4)    ==  Just 3
--    regularidad (completo 5)    ==  Just 4
--    regularidad (grafoCiclo 4)  ==  Just 2
--    regularidad (grafoCiclo 5)  ==  Just 2
-- ---------------------------------------------------------------------

regularidad :: (Ix v,Num p) => Grafo v p -> Maybe Int
regularidad g | regular g = Just (grado g (head (nodos g)))
              | otherwise = Nothing    

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la propiedad
--    prop_completoRegular :: Int -> Bool
-- tal que (prop_completoRegular n) se verifica si el grafo completo de
-- orden n es (n-1)-regular. Por ejemplo,
--    prop_completoRegular 5  ==  True
-- y usarla para comprobar que la cumplen todos los grafos completos
-- desde orden 1 hasta 20.
-- ---------------------------------------------------------------------

prop_completoRegular :: Int -> Bool
prop_completoRegular n = 
   regularidad (completo n) == Just (n-1)

-- La comprobación es                       
--    ghci> and [prop_completoRegular n | n <- [1..20]]
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la propiedad
--    prop_cicloRegular :: Int -> Bool
-- tal que (prop_cicloRegular n) se verifica si el grafo ciclo de orden
-- n es 2-regular. Por ejemplo, 
--    prop_cicloRegular 2  ==  True
-- y usarla para comprobar que la cumplen todos los grafos ciclos
-- desde orden 3 hasta 20.
-- ---------------------------------------------------------------------

prop_cicloRegular :: Int -> Bool
prop_cicloRegular n = 
   regularidad (grafoCiclo n) == Just 2

-- La comprobación es                       
--    ghci> and [prop_cicloRegular n | n <- [3..20]]
--    True

-- ---------------------------------------------------------------------
-- § Generador de grafos                                              --
-- ---------------------------------------------------------------------

-- (generaGND n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    ghci> generaGND 3 [4,2,5]
--    (ND,array (1,3) [(1,[(2,4),(3,2)]),
--                     (2,[(1,4),(3,5)]),
--                      3,[(1,2),(2,5)])])
--    ghci> generaGND 3 [4,-2,5]
--    (ND,array (1,3) [(1,[(2,4)]),(2,[(1,4),(3,5)]),(3,[(2,5)])])
generaGND :: Int -> [Int] -> Grafo Int Int
generaGND n ps  = creaGrafo ND (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n], x < y]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- (generaGD n ps) es el grafo completo de orden n tal que los pesos
-- están determinados por ps. Por ejemplo,
--    ghci> generaGD 3 [4,2,5]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[]),
--                    (3,[])])
--    ghci> generaGD 3 [4,2,5,3,7,9,8,6]
--    (D,array (1,3) [(1,[(1,4),(2,2),(3,5)]),
--                    (2,[(1,3),(2,7),(3,9)]),
--                    (3,[(1,8),(2,6)])])
generaGD :: Int -> [Int] -> Grafo Int Int
generaGD n ps = creaGrafo D (1,n) l3
    where l1 = [(x,y) | x <- [1..n], y <- [1..n]]
          l2 = zip l1 ps
          l3 = [(x,y,z) | ((x,y),z) <- l2, z > 0]

-- genGD es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGD
--    (D,array (1,4) [(1,[(1,1)]),(2,[(3,1)]),(3,[(2,1),(4,1)]),(4,[(4,1)])])
--    (D,array (1,2) [(1,[(1,6)]),(2,[])])
--    ...
genGD :: Gen (Grafo Int Int)
genGD = do n <- choose (1,10)
           xs <- vectorOf (n*n) arbitrary
           return (generaGD n xs)

-- genGND es un generador de grafos dirigidos. Por ejemplo,
--    ghci> sample genGND
--    (ND,array (1,1) [(1,[])])
--    (ND,array (1,3) [(1,[(2,3),(3,13)]),(2,[(1,3)]),(3,[(1,13)])])
--    ...
genGND :: Gen (Grafo Int Int)
genGND = do n <- choose (1,10)
            xs <- vectorOf (n*n) arbitrary
            return (generaGND n xs)

-- genG es un generador de grafos. Por ejemplo,
--    ghci> sample genG
--    (D,array (1,3) [(1,[(2,1)]),(2,[(1,1),(2,1)]),(3,[(3,1)])])
--    (ND,array (1,3) [(1,[(2,2)]),(2,[(1,2)]),(3,[])])
--    ...
genG :: Gen (Grafo Int Int)
genG = do d <- choose (True,False)
          n <- choose (1,10)
          xs <- vectorOf (n*n) arbitrary
          if d then return (generaGD n xs)
               else return (generaGND n xs)

-- Los grafos está contenido en la clase de los objetos generables
-- aleatoriamente. 
instance Arbitrary (Grafo Int Int) where
    arbitrary = genG

-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Control.Monad

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir por recursión la función
--    sumaImpares :: Int -> Int
-- tal que (sumaImpares n) es la suma de los n primeros números
-- impares. Por ejemplo,
--    sumaImpares 5  ==  25
-- ---------------------------------------------------------------------

sumaImpares :: Int -> Int
sumaImpares 0 = 0
sumaImpares n = sumaImpares (n-1) + (2*n-1) 


sumaImpares2 :: Int -> Int
sumaImpares2 0 = 0
sumaImpares2 n = 2*n+1 + sumaImpares2 (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir, sin usar recursión, la función
--    sumaImpares' :: Int -> Int
-- tal que (sumaImpares' n) es la suma de los n primeros números
-- impares. Por ejemplo,
--    ghci> sumaImpares' 5  ==  25
-- ---------------------------------------------------------------------

sumaImpares' :: Int -> Int
sumaImpares' n = sum [1,3..(2*n-1)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir la función
--    sumaImparesIguales :: Int -> Int -> Bool
-- tal que (sumaImparesIguales m n) se verifica si para todo x entre m y
-- n se tiene que (sumaImpares x) y (sumaImpares' x) son iguales.
-- 
-- Comprobar que (sumaImpares x) y (sumaImpares' x) son iguales para
-- todos los números x entre 1 y 100.
-- ---------------------------------------------------------------------

-- La definición es
sumaImparesIguales :: Int -> Int -> Bool
sumaImparesIguales m n = 
    and [sumaImpares x == sumaImpares' x | x <- [m..n]]

-- La comprobación es
--    ghci>  sumaImparesIguales 1 100
--    True

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Definir la función 
--    grafoSumaImpares :: Int -> Int -> [(Int,Int)]
-- tal que (grafoSumaImpares m n) es la lista formadas por los números x
-- entre m y n y los valores de (sumaImpares x).
--
-- Calcular (grafoSumaImpares 1 9).
-- ---------------------------------------------------------------------

-- La definición es
grafoSumaImpares :: Int -> Int -> [(Int,Int)]
grafoSumaImpares m n =
    [(x,sumaImpares x) | x <- [m..n]]

-- El cálculo es
--    ghci> grafoSumaImpares 1 9
--    [(1,1),(2,4),(3,9),(4,16),(5,25),(6,36),(7,49),(8,64),(9,81)]

-- ---------------------------------------------------------------------
-- Ejercicio 1.5. Demostrar por inducción que para todo n, 
-- (sumaImpares n) es igual a n^2.
-- ---------------------------------------------------------------------

{-
 Caso base: Hay que demostrar que
    sumaImpares 0 = 0^2 
 En efecto,
    sumaImpares 0   [por hipótesis]  
    = 0               [por sumaImpares.1]
    = 0^2             [por aritmética]
 
  Caso inductivo: Se supone la hipótesis de inducción (H.I.)
     sumaImpares n = n^2
  Hay que demostrar que
     sumaImpares (n+1) = (n+1)^2
  En efecto,
     sumaImpares (n+1) = 
     = (sumaImpares n) + (2*n+1)        [por sumaImpares.2]
     = n^2 + (2*n+1)                    [por H.I.]
     = (n+1)^2                          [por álgebra]
-} 

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir por recursión la función
--    sumaPotenciasDeDosMasUno :: Int -> Int
-- tal que 
--    sumaPotenciasDeDosMasUno n = 1 + 2^0 + 2^1 + 2^2 + ... + 2^n. 
-- Por ejemplo, 
--    sumaPotenciasDeDosMasUno 3 == 16
-- ---------------------------------------------------------------------

sumaPotenciasDeDosMasUno :: Int -> Int
sumaPotenciasDeDosMasUno 0 = 2
sumaPotenciasDeDosMasUno n = sumaPotenciasDeDosMasUno (n-1) + 2^n

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir por comprensión la función
--    sumaPotenciasDeDosMasUno' :: Int -> Int
-- tal que 
--    (sumaPotenciasDeDosMasUno' n) = 1 + 2^0 + 2^1 + 2^2 + ... + 2^n. 
-- Por ejemplo, 
--    sumaPotenciasDeDosMasUno' 3  ==  16
-- ---------------------------------------------------------------------

sumaPotenciasDeDosMasUno' :: Int -> Int
sumaPotenciasDeDosMasUno' n = 1 + sum [2^x | x <- [0..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Demostrar por inducción que
--    sumaPotenciasDeDosMasUno n = 2^(n+1)
-- ---------------------------------------------------------------------

{-
  Caso base: Hay que demostrar que 
     sumaPotenciasDeDosMasUno 0 = 2^(0+1)
  En efecto,
       sumaPotenciasDeDosMasUno 0 
     = 2                              [por sumaPotenciasDeDosMasUno.1]
     = 2^(0+1)                        [por aritmética]

  Caso inductivo: Se supone la hipótesis de inducción (H.I.)
     sumaPotenciasDeDosMasUno n = 2^(n+1)
  Hay que demostrar que 
     sumaPotenciasDeDosMasUno (n+1) = 2^((n+1)+1)
  En efecto, 
       sumaPotenciasDeDosMasUno (n+1)
     = (sumaPotenciasDeDosMasUno n) + 2^(n+1)  [por sumaPotenciasDeDosMasUno.2]
     = 2^(n+1) + 2^(n+1)                       [por H.I.]
     = 2^((n+1)+1)                             [por aritmética]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir por recursión la función
--    copia :: Int -> a -> [a]
-- tal que (copia n x) es la lista formado por n copias del elemento
-- x. Por ejemplo, 
--    copia 3 2  ==  [2,2,2]
-- ---------------------------------------------------------------------
 
copia :: Int -> a -> [a]
copia 0 _ = []                  -- copia.1
copia n x = x : copia (n-1) x   -- copia.2

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir por recursión la función 
--    todos :: (a -> Bool) -> [a] -> Bool
-- tal que (todos p xs) se verifica si todos los elementos de xs cumplen
-- la propiedad p. Por ejemplo,
--    todos even [2,6,4]  ==  True
--    todos even [2,5,4]  ==  False
-- ---------------------------------------------------------------------

todos :: (a -> Bool) -> [a] -> Bool
todos p []       = True                -- todos.1
todos p (x : xs) = p x && todos p xs   -- todos.2

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Comprobar con QuickCheck que todos los elementos de 
-- (copia n x) son iguales a x.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_copia :: Eq a => Int -> a -> Bool
prop_copia n x =
    todos (==x) (copia n' x)
    where n' = abs n

-- La comprobación es
--    ghci> quickCheck prop_copia
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.4. Demostrar, por inducción en n, que todos los elementos
-- de (copia n x) son iguales a x.
-- ---------------------------------------------------------------------

{-
  Hay que demostrar que para todo n y todo x,
     todos (==x) (copia n x)

  Caso base: Hay que demostrar que
     todos (==x) (copia 0 x) = True 
  En efecto, 
       todos (== x) (copia 0 x)
     = todos (== x) []            [por copia.1] 
     = True                       [por todos.1] 

  Caso inductivo: Se supone la hipótesis de inducción (H.I.)
     todos (==x) (copia n x) = True
  Hay que demostrar que
     todos (==x) (copia (n+1) x) = True
  En efecto, 
       todos (==x) (copia (n+1) x)
     = todos (==x) (x : copia n x )         [por copia.2]
     = x == x && todos (==x) (copia n x )   [por todos.2] 
     = True && todos (==x) (copia n x )     [por def. de ==] 
     = todos (==x) (copia n x )             [por def. de &&] 
     = True                                 [por H.I.]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir por recursión la función 
--   factR :: Integer -> Integer
-- tal que (factR n) es el factorial de n. Por ejemplo,
--   factR 4  ==  24
-- ---------------------------------------------------------------------

factR :: Integer -> Integer
factR 0 = 1
factR n = n * factR (n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir por comprensión la función 
--   factC :: Integer -> Integer
-- tal que (factR n) es el factorial de n. Por ejemplo,
--   factC 4  ==  24
-- ---------------------------------------------------------------------

factC :: Integer -> Integer
factC n = product [1..n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las funciones factR y
-- factC son equivalentes sobre los números naturales.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_factR_factC :: Integer -> Bool
prop_factR_factC n = 
    factR n' == factC n'
    where n' = abs n

-- La comprobación es
--    ghci> quickCheck prop_factR_factC
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.4. Comprobar con QuickCheck si las funciones factR y
-- factC son equivalentes sobre los números enteros.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_factR_factC_Int :: Integer -> Bool
prop_factR_factC_Int n = 
    factR n == factC n

-- La comprobación es
--    ghci> quickCheck prop_factR_factC_Int
--    *** Exception: Non-exhaustive patterns in function factR

-- No son iguales ya que factR no está definida para los números
-- negativos y factC de cualquier número negativo es 0.

-- ---------------------------------------------------------------------
-- Ejercicio 4.5. Se considera la siguiente definición iterativa de la
-- función factorial 
--    factI :: Integer -> Integer
--    factI n = factI' n 1
--    
--    factI' :: Integer -> Integer -> Integer
--    factI' 0 x = x                  -- factI'.1
--    factI' n x = factI' (n-1) n*x   -- factI'.2
-- Comprobar con QuickCheck que factI y factR son equivalentes sobre los
-- números naturales.
-- ---------------------------------------------------------------------

factI :: Integer -> Integer
factI n = factI' n 1

factI' :: Integer -> Integer -> Integer
factI' 0 x = x
factI' n x = factI' (n-1) n*x 

-- La propiedad es
prop_factI_factR n = 
    factI n' == factR n'
    where n' = abs n

-- La comprobación es
--    ghci> quickCheck prop_factI_factR
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.6. Comprobar con QuickCheck que para todo número natural
-- n, (factI' n x) es igual al producto de x y (factR n).
-- --------------------------------------------------------------------- 

-- La propiedad es
prop_factI' :: Integer -> Integer -> Bool
prop_factI' n  x =
    factI' n' x == x * factR n'
    where n' = abs n

-- La comprobación es
--    ghci> quickCheck prop_factI'
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 4.7. Demostrar por inducción que para todo número natural
-- n, (factI' n x) es igual x*n!
-- --------------------------------------------------------------------- 

{-
  Demostración (por inducción en n)

  Caso base: Hay que demostrar que factI' 0 x = x*0!
  En efecto,
     factI' 0 x 
     = x           [por factI'.1]
     = x*0!        [por álgebra]

  Caso inductivo: Se supone la hipótesis de inducción: para todo x,
     factI' n x = x*n!
  hay que demostrar que para todo x
     factI' (n+1) x = x*(n+1)!
  En efecto,
     factI' (n+1) x
     = factI' n (n+1)*x    [por factI'.2]
     = (n+1)*x*n!          [por hipótesis de inducción]
     = x*(n+1)!            [por álgebra]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, recursivamente y sin usar (++). la función
--    amplia :: [a] -> a -> [a]
-- tal que (amplia xs y) es la lista obtenida añadiendo el elemento y al
-- final de la lista xs. Por ejemplo,
--    amplia [2,5] 3  ==  [2,5,3]
-- ---------------------------------------------------------------------

amplia :: [a] -> a -> [a]
amplia []     y = [y]               -- amplia.1
amplia (x:xs) y = x : amplia xs y   -- amplia.2

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, mediante plegado. la función
--    ampliaF :: [a] -> a -> [a]
-- tal que (ampliaF xs y) es la lista obtenida añadiendo el elemento y al
-- final de la lista xs. Por ejemplo,
--    ampliaF [2,5] 3  ==  [2,5,3]
-- ---------------------------------------------------------------------

ampliaF :: [a] -> a -> [a]
ampliaF xs y = foldr (:) [y] xs

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que amplia y ampliaF son
-- equivalentes. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_amplia_ampliaF :: Eq a => [a] -> a -> Bool
prop_amplia_ampliaF xs y =
    amplia xs y == ampliaF xs y

-- La comprobación es
--    ghci> quickCheck prop_amplia_ampliaF
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.4. Comprobar con QuickCheck que
--    amplia xs y = xs ++ [y]
-- ---------------------------------------------------------------------

-- La propiedad es
prop_amplia :: Eq a => [a] -> a -> Bool
prop_amplia xs y =
    amplia xs y == xs ++ [y]

-- La comprobación es
--    ghci> quickCheck prop_amplia
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.5. Demostrar por inducción que
--    amplia xs y = xs ++ [y]
-- ---------------------------------------------------------------------

{-
  Demostración: Por inducción en xs.

  Caso base: Hay que demostrar que 
     amplia [] y = [] ++ [y]
  En efecto,
     amplia [] y 
     = [y]         [por amplia.1]
     = [] ++ [y]   [por (++).1]

  Caso inductivo: Se supone la hipótesis de inducción
     amplia xs y = xs ++ [y]
  Hay que demostrar que
     amplia (x:xs) y = (x:xs) ++ [y]
  En efecto,
     amplia (x:xs) y
     = x : amplia xs y    [por amplia.2]
     = x : (xs ++ [y])    [por hipótesis de inducción]
     = (x:xs) ++ [y]      [por (++).2]
-}

-- ----------------------------------------------------------------------
-- Ejercicio 6.1. Definir la función 
--    listaConSuma :: Int -> [[Int]] 
-- que, dado un número natural n, devuelve todas las listas de enteros
-- positivos (esto es, enteros mayores o iguales que 1) cuya suma sea
-- n. Por ejemplo,
--    Main> listaConSuma 4
--    [[1,1,1,1],[1,1,2],[1,2,1],[1,3],[2,1,1],[2,2],[3,1],[4]]
-- ---------------------------------------------------------------------

listaConSuma :: Int -> [[Int]]
listaConSuma 0 = [[]]
listaConSuma n = [x:xs | x <- [1..n], xs <- listaConSuma (n-x)]

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la función
--    numeroDeListasConSuma :: Int -> Int
-- tal que (numeroDeListasConSuma n) es el número de elementos de
-- (listaConSuma n). Por ejemplo,
--    numeroDeListasConSuma 10  =  512
-- ---------------------------------------------------------------------

numeroDeListasConSuma :: Int -> Int
numeroDeListasConSuma = length . listaConSuma

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Definir la constante
--    numerosDeListasConSuma :: [(Int,Int)]
-- tal que numerosDeListasConSuma es la lista de los pares formado por un
-- número natural n mayor que 0 y el número de elementos de 
-- (listaConSuma n). 
--
-- Calcular el valor de
--    take 10 numerosDeListasConSuma
-- ---------------------------------------------------------------------

-- La constante es
numerosDeListasConSuma :: [(Int,Int)]
numerosDeListasConSuma = [(n,numeroDeListasConSuma n) | n <- [1..]] 

-- El cálculo es
--    ghci> take 10 numerosDeListasConSuma
--    [(1,1),(2,2),(3,4),(4,8),(5,16),(6,32),(7,64),(8,128),(9,256),(10,512)]

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. A partir del ejercicio anterior, encontrar una fórmula
-- para calcular el valor de (numeroDeListasConSuma n) paras los
-- números n mayores que 0. 
-- 
-- Demostrar dicha fórmula por inducción fuerte.
-- ---------------------------------------------------------------------

{-
 La fórmula es
    numeroDeListasConSuma n = 2^(n-1)
 La demostración, por inducción fuerte en n, es la siguiente:

 Caso base (n=1):
    numeroDeListasConSuma 1 
    = length (listaConSuma 1)    
         [por numeroDeListasConSuma]
    = length [[x:xs | x <- [1..1], xs <- listaConSuma [[]]]
         [por listaConSuma.2]
    = length [[1]]
         [por def. de listas de comprensión]
    = 1
         [por def. de length] 
    = 2^(1-1)
         [por aritmética] 

 Paso de inducción: Se supone que
    para todo x en [1..n-1], 
       numeroDeListasConSuma x = 2^(x-1)
 Hay que demostrar que
    numeroDeListasConSuma n = 2^(n-1)
 En efecto,
    numeroDeListasConSuma n
    = length (listaConSuma n)   
         [por numeroDeListasConSuma]
    = length [x:xs | x <- [1..n], xs <- listaConSuma (n-x)]
         [por listaConSuma.2]
    = sum [numeroDeListasConSuma (n-x) | x <- [1..n]]
         [por length y listas de comprensión]
    = sum [2^(n-x-1) | x <- [1..n-1]] + 1
         [por hip. de inducción y numeroDeListasConSuma]
    = 2^(n-2) + 2^(n-3) + ... + 2^1 + 2^0 + 1
    = 2^(n-1)
         [por el ejercicio 2c de la relación 15]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 6.4. A partir del ejercicio anterior, definir de manera más
-- eficiente la función numeroDeListasConSuma.
-- ---------------------------------------------------------------------

numeroDeListasConSuma' :: Int -> Int
numeroDeListasConSuma' 0 = 1
numeroDeListasConSuma' n = 2^(n-1)

-- ---------------------------------------------------------------------
-- Ejercicio 6.5. Comparar la eficiencia de las dos definiciones
-- comparando el tiempo y el espacio usado para calcular 
-- (numeroDeListasConSuma 20) y (numeroDeListasConSuma' 20).
-- ---------------------------------------------------------------------

-- La comparación es
--    ghci> :set +s
--    ghci> numeroDeListasConSuma 20
--    524288
--    (9.99 secs, 519419824 bytes)
--    ghci> numeroDeListasConSuma' 20
--    524288
--    (0.01 secs, 0 bytes)

-- ---------------------------------------------------------------------
-- Ejercicio 7.0. La sucesión de Fibonacci 
--    0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, ...
-- puede definirse por recursión como
--    fib :: Int -> Int
--    fib 0 = 0                           -- fib.1
--    fib 1 = 1                           -- fib.2
--    fib n = (fib (n-11)) + fib (n-2)    -- fib.3
-- También puede definirse por recursición iterativa como
--    fibIt :: Int -> Int
--    fibIt n = fibItAux n 0 1
-- donde la función auxiliar se define por
--    fibItAux :: Int -> Int -> Int -> Int
--    fibItAux 0 a b = a                        -- fibItAux.1
--    fibItAux n a b = fibItAux (n-1) b (a+b)   -- fibItAux.2 
-- ---------------------------------------------------------------------

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 

fibIt :: Int -> Int
fibIt n = fibItAux n 0 1

fibItAux :: Int -> Int -> Int -> Int
fibItAux 0 a b = a
fibItAux n a b = fibItAux (n-1) b (a+b) 

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Comprobar con QuickCheck que para todo número natural
-- n tal que n <= 20, se tiene que
--    fib n = fibIt n
-- ---------------------------------------------------------------------

-- La propiedad es
prop_fib :: Int -> Property
prop_fib n =
    n >= 0 && n <= 20 ==> fib n == fibIt n

-- La comprobación es
--    ghci> quickCheck prop_fib
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Sea f la función definida por
--    f :: Int -> Int -> Int
--    f n k = fibItAux n (fib k) (fib (k+1))
-- Definir la función 
--    grafoDeF :: Int -> [(Int,Int)]
-- tal que (grafoDeF n)
--    ghci> take 7 (grafoDeF 3)
--    [(1,3),(2,5),(3,8),(4,13),(5,21),(6,34),(7,55)]
--    ghci> take 7 (grafoDeF 5)
--    [(1,8),(2,13),(3,21),(4,34),(5,55),(6,89),(7,144)]
-- ---------------------------------------------------------------------

f :: Int -> Int -> Int
f n k = fibItAux n (fib k) (fib (k+1))

grafoDeF :: Int -> [(Int,Int)]
grafoDeF n = [(k, f n k) | k <- [1..]]

-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que para todo par de números
-- naturales n, k tales que n+k <= 20, se tiene que
--    fibItAux n (fib k) (fib (k+1)) = fib (k+n)
-- ---------------------------------------------------------------------

-- La propiedad es
prop_fibItAux :: Int -> Int -> Property
prop_fibItAux n k =
    n >= 0 && k >= 0 && n+k <= 20 ==>
    fibItAux n (fib k) (fib (k+1)) == fib (k+n)

-- La comprobación es
--    ghci> quickCheck prop_fibItAux
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 7.4. Demostrar por inducción que para todo n y todo k,
--    fibItAux n (fib k) (fib (k+1)) = fib (k+n)
-- ---------------------------------------------------------------------

{-
 Demostración: Por inducción en n se prueba que
    para todo k, fibItAux n (fib k) (fib (k+1)) = fib (k+n)

 Caso base (n=0): Hay que demostrar que
    para todo k, fibItAux 0 (fib k) (fib (k+1)) = fib k
 En efecto, sea k un número natural. Se tiene
    fibItAux 0 (fib k) (fib (k+1))
    = fib k                          [por fibItAux.1]

 Paso de inducción: Se supone la hipótesis de inducción
    para todo k, fibItAux n (fib k) (fib (k+1)) = fib (k+n)
 Hay que demostrar que
    para todo k, fibItAux (n+1) (fib k) (fib (k+1)) = fib (k+n+1)
 En efecto. Sea k un número natural,
    fibItAux (n+1) (fib k) (fib (k+1))
    = fibItAux n (fib (k+1)) ((fib k) + (fib (k+1)))
         [por fibItAux.2]
    = fibItAux n (fib (k+1)) (fib (k+2))
         [por fib.3]
    = fib (n+k+1)
         [por hipótesis de inducción]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 7.5. Demostrar que para todo n,
--    fibIt n = fib n
-- ---------------------------------------------------------------------

{-
 Demostración
    fibIt n 
    = fibItAux n 0 1               [por fibIt]
    = fibItAux n (fib 0) (fib 1)   [por fib.1 y fib.2]
    = fib n                        [por ejercicio 5.4]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 8.1. La función potencia puede definirse por
--    potencia :: Int -> Int -> Int
--    potencia x 0 = 1
--    potencia x n | even n    = potencia (x*x) (div n 2)
--                 | otherwise = x * potencia (x*x) (div n 2)
-- Comprobar con QuickCheck que para todo número natural n y todo
-- número entero x, (potencia x n) es x^n.
-- ---------------------------------------------------------------------

potencia :: Integer -> Integer -> Integer
potencia x 0 = 1
potencia x n | even n    = potencia (x*x) (div n 2)
             | otherwise = x * potencia (x*x) (div n 2)

-- La propiedad es
prop_potencia :: Integer -> Integer -> Property
prop_potencia x n =
    n >= 0 ==> potencia x n == x^n

-- La comprobación es
--    ghci> quickCheck prop_potencia
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Demostrar por inducción que que para todo número
-- natural n y todo número entero x, (potencia x n) es x^n
-- ---------------------------------------------------------------------

{-
 Demostración: Por inducción en n.

 Caso base: Hay que demostrar que 
    para todo x, potencia x 0 = 2^0
 Sea x un número entero, entonces
    potencia x 0 
    = 1              [por potencia.1]
    = 2^0            [por aritmética]

 Paso de inducción: Se supone que n>0 y la hipótesis de inducción: 
    para todo m<n y para todo x, potencia x (n-1) = x^(n-1)
 Tenemos que demostrar que 
    para todo x, potencia x n = x^n
 Lo haremos distinguiendo casos según la paridad de n.

 Caso 1: Supongamos que n es par. Entonces, existe un k tal que 
    n = 2*k.                    (1)
 Por tanto, 
    potencia  n 
    = potencia (x*x) (div n 2)  [por potencia.2]
    = potencia (x*x) k          [por (1)]
    = (x*x)^k                   [por hip. de inducción]
    = x^(2*k)                   [por aritmética]
    = x^n                       [por (1)]

 Caso 2: Supongamos que n es impar. Entonces, existe un k tal que 
    n = 2*k+1.                       (2)
 Por tanto, 
    potencia  n 
    = x * potencia (x*x) (div n 2)   [por potencia.3]
    = x * potencia (x*x) k           [por (1)]
    = x * (x*x)^k                    [por hip. de inducción]
    = x^(2*k+1)                      [por aritmética]
    = x^n                            [por (1)]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Comprobar con QuickCheck que para todo par de listas
-- xs, ys se tiene que
--    reverse (xs ++ ys) == reverse ys ++ reverse xs
-- ---------------------------------------------------------------------

-- La propiedad es
prop_reverse_conc :: [Int] -> [Int] -> Bool
prop_reverse_conc xs ys =
    reverse (xs ++ ys) == reverse ys ++ reverse xs

-- La comprobación es
--    ghci> quickCheck prop_reverse_conc
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Demostrar por inducción que para todo par de listas
-- xs, ys se tiene que
--    reverse (xs ++ ys) == reverse ys ++ reverse xs
-- 
-- Las definiciones de reverse y (++) son
--    reverse [] = []                      -- reverse.1
--    reverse (x:xs) = reverse xs ++ [x]   -- reverse.2
-- 
--    [] ++ ys     = ys                    -- ++.1
--    (x:xs) ++ ys = x : (xs ++ ys)        -- ++.2
-- ---------------------------------------------------------------------

{-
 Demostración por inducción en xs.

 Caso base: Hay que demostrar que para toda ys,
    reverse ([] ++ ys) == reverse ys ++ reverse []
 En efecto,
    reverse ([] ++ ys)
    = reverse ys                  [por ++.1] 
    = reverse ys ++ []            [por propiedad de ++]
    = reverse ys ++ reverse []    [por reverse.1]

 Paso de inducción: Se supone que para todo ys,
    reverse (xs ++ ys) == reverse ys ++ reverse xs
 Hay que demostrar que para todo ys,   
    reverse ((x:xs) ++ ys) == reverse ys ++ reverse (x:xs)
 En efecto,
    reverse ((x:xs) ++ ys)
    = reverse (x:(xs ++ ys))               [por ++.2]
    = reverse (xs ++ ys) ++ [x]            [por reverse.2]
    = (reverse ys ++ reverse xs) ++ [x]    [por hip. de inducción]
    = reverse ys ++ (reverse xs ++ [x])    [por asociativa de ++]
    = reverse ys ++ reverse (x:xs)         [por reverse.2]  
-}

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Demostrar por inducción que para toda lista xs,
--    reverse (reverse xs) = xs
-- ---------------------------------------------------------------------

{-
 Demostración por inducción en xs.

 Caso Base: Hay que demostrar que 
    reverse (reverse []) = []
 En efecto, 
    reverse (reverse [])
    = reverse []           [por reverse.1]
    = []                   [por reverse.1]

 Paso de inducción: Se supone que
    reverse (reverse xs) = xs
 Hay que demostrar que
    reverse (reverse (x:xs)) = x:xs
 En efecto,
    reverse (reverse (x:xs))
    = reverse (reverse xs ++ [x])           [por reverse.2]
    = reverse [x] ++ reverse (reverse xs)   [por ejercicio 7.2]
    = [x] ++ reverse (reverse xs)           [por reverse]
    = [x] ++ xs                             [por hip. de inducción]
    = x:xs                                  [por ++.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.0. En los siguientes ejercicios se demostrarán
-- propiedades de los árboles binarios definidos como sigue
--    data Arbol a = Hoja 
--                 | Nodo a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- En los ejemplos se usará el siguiente árbol
--    arbol = Nodo 9
--                   (Nodo 3 
--                         (Nodo 2 Hoja Hoja) 
--                         (Nodo 4 Hoja Hoja)) 
--                   (Nodo 7 Hoja Hoja)
-- ---------------------------------------------------------------------

data Arbol a = Hoja 
             | Nodo a (Arbol a) (Arbol a)
             deriving (Show, Eq)

arbol = Nodo 9
               (Nodo 3 
                     (Nodo 2 Hoja Hoja) 
                     (Nodo 4 Hoja Hoja)) 
               (Nodo 7 Hoja Hoja)

-- ---------------------------------------------------------------------
-- Nota. Para comprobar propiedades de árboles con QuickCheck se
-- utilizará el siguiente generador.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbol
    where
      arbol 0       = return Hoja 
      arbol n | n>0 = oneof [return Hoja,
                             liftM3 Nodo arbitrary subarbol subarbol]
                      where subarbol = arbol (div n 2)
  -- coarbitrary = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10.1. Definir la función
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del árbol x. Por ejemplo,
--    ghci> espejo arbol
--    Nodo 9 
--         (Nodo 7 Hoja Hoja) 
--         (Nodo 3 
--               (Nodo 4 Hoja Hoja) 
--               (Nodo 2 Hoja Hoja))
-- ---------------------------------------------------------------------

espejo :: Arbol a -> Arbol a
espejo Hoja         = Hoja
espejo (Nodo x i d) = Nodo x (espejo d) (espejo i)

-- ---------------------------------------------------------------------
-- Ejercicio 10.2. Comprobar con QuickCheck que para todo árbol x,
--    espejo (espejo x) = x
-- ---------------------------------------------------------------------

prop_espejo :: Arbol Int -> Bool
prop_espejo x =
    espejo (espejo x) == x

-- ---------------------------------------------------------------------
-- Ejercicio 10.3. Demostrar por inducción que para todo árbol x,
--    espejo (espejo x) = x
-- ---------------------------------------------------------------------

{-
 Demostración por inducción en x

 Caso base: Hay que demostrar que
    espejo (espejo Hoja) = Hoja
 En efecto,
    espejo (espejo Hoja)
    = espejo Hoja          [por espejo.1]
    = Hoja                 [por espejo.1]

 Paso de inducción: Se supone la hipótesis de inducción
    espejo (espejo i) = i
    espejo (espejo d) = d
 Hay que demostrar que
    espejo (espejo (Nodo x i d)) = Nodo x i d
 En efecto,
    espejo (espejo (Nodo x i d))
    = espejo (Nodo x (espejo d) (espejo i))             [por espejo.2]
    = Nodo x (espejo (espejo i)) (espejo (espejo d))    [por espejo.2]
    = Nodo x i d                                        [por hip. inducción]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.4. Definir la función
--    preorden :: Arbol a -> [a]
-- tal que (preorden x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    ghci> arbol
--    Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
--    ghci> preorden arbol
--    [9,3,2,4,7]
-- ---------------------------------------------------------------------

preorden :: Arbol a -> [a]
preorden Hoja         = []
preorden (Nodo x i d) = x : (preorden i ++ preorden d)

-- ---------------------------------------------------------------------
-- Ejercicio 10.5. Definir la función
--    postorden :: Arbol a -> [a]
-- tal que (postorden x) es la lista correspondiente al recorrido
-- postorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación el subárbol derecho y, finalmente, la raíz
-- del árbol. Por ejemplo,
--    ghci> arbol
--    Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
--    ghci> postorden arbol
--    [2,4,3,7,9]
-- ---------------------------------------------------------------------

postorden :: Arbol a -> [a]
postorden Hoja         = []
postorden (Nodo x i d) = postorden i ++ postorden d ++ [x]

-- ---------------------------------------------------------------------
-- Ejercicio 10.6. Comprobar con QuickCheck que para todo árbol x,
--    postorden (espejo x) = reverse (preorden x)
-- ---------------------------------------------------------------------

-- La propiedad es
prop_recorrido :: Arbol Int -> Bool
prop_recorrido x =
   postorden (espejo x) == reverse (preorden x)

-- La comprobación es
--    ghci> quickCheck prop_recorrido
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.7. Demostrar por inducción que para todo árbol x,
--    postorden (espejo x) = reverse (preorden x)
-- ---------------------------------------------------------------------

{-
 Demostración por inducción en x.

 Caso base: Hay que demostrar que 
    postorden (espejo Hoja) = reverse (preorden Hoja)
 En efecto,
    postorden (espejo Hoja)
    = postorden Hoja           [por espejo.1]
    = []                       [por postorden.1]
    = reverse []               [por reverse.1]
    = reverse (preorden Hoja)  [por preorden.1]

 Paso de inducción: Se supone la hipótesis de inducción
    postorden (espejo i) = reverse (preorden i)
    postorden (espejo d) = reverse (preorden d)
 Hay que demostrar que
    postorden (espejo (Nodo x i d)) = reverse (preorden (Nodo x i d))
 En efecto,
    postorden (espejo (Nodo x i d))
    = postorden (Nodo x (espejo d) (espejo i))   [por espejo.2]
    = postorden (espejo d) ++ postorden (espejo i) ++ [x]                
                                                 [por postorden.2]
    = reverse (preorden d) ++ reverse (preorden i) ++ [x]
                                                 [por hip. inducción]
    = reverse ([x] ++ preorden (espejo i) ++ preorden (espejo d))      
                                                 [por ejercicio 7.1]
    = reverse (preorden (Nodo x i d))            [por preorden.1]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.8. Comprobar con QuickCheck que para todo árbol binario
-- x, se tiene que
--    reverse (preorden (espejo x)) = postorden x
-- ---------------------------------------------------------------------

-- La propiedad es
prop_reverse_preorden_espejo :: Arbol Int -> Bool
prop_reverse_preorden_espejo x =
   reverse (preorden (espejo x)) == postorden x

-- La comprobación es
--    ghci> quickCheck prop_reverse_preorden_espejo
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.9. Demostrar que para todo árbol binario x, se tiene que
--    reverse (preorden (espejo x)) = preorden x
-- ---------------------------------------------------------------------

{-
 Demostración:
    reverse (preorden (espejo x))
    = postorden (espejo (espejo x))    [por ejercicio 8.7]
    = postorden x                      [por ejercicio 8.3]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.10. Definir la función
--    nNodos :: Arbol a -> Int
-- tal que (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--    ghci> arbol
--    Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
--    ghci> nNodos arbol
--    5
-- ---------------------------------------------------------------------

nNodos :: Arbol a -> Int
nNodos Hoja         = 0
nNodos (Nodo x i d) = 1 + nNodos i + nNodos d

-- ---------------------------------------------------------------------
-- Ejercicio 10.11. Comprobar con QuickCheck que el número de nodos de la
-- imagen especular de un árbol es el mismo que el número de nodos del
-- árbol. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nNodos_espejo :: Arbol Int -> Bool
prop_nNodos_espejo x =
   nNodos (espejo x) == nNodos x

-- La comprobación es
--    ghci> quickCheck prop_nNodos_espejo
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.12. Demostrar por inducción que el número de nodos de la
-- imagen especular de un árbol es el mismo que el número de nodos del
-- árbol. 
-- ---------------------------------------------------------------------

{-
 Demostración: Hay que demostrar, por inducción en x, que 
    nNodos (espejo x) == nNodos x
 
 Caso base: Hay que demostrar que
    nNodos (espejo Hoja) == nNodos Hoja
 En efecto,
    nNodos (espejo Hoja)
    = nNodos Hoja          [por espejo.1]

 Paso de inducción: Se supone la hipótesis de inducción
    nNodos (espejo i) == nNodos i
    nNodos (espejo d) == nNodos d
 Hay que demostrar que
    nNodos (espejo (Nodo x i d)) == nNodos (Nodo x i d)
 En esfecto,
    nNodos (espejo (Nodo x i d))
    = nNodos (Nodo x (espejo d) (espejo i))       [por espejo.2]
    = 1 + nNodos (espejo d) + nNodos (espejo i)   [por nNodos.2]
    = 1 + nNodos d + nNodos i                     [por hip.de inducción]
    = 1 + nNodos i + nNodos d                     [por aritmética]
    = nNodos (Nodo x i d)                         [por nNodos.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.13. Comprobar con QuickCheck que la longitud de la lista
-- obtenida recorriendo un árbol en sentido preorden es igual al número
-- de nodos del árbol.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_length_preorden :: Arbol Int -> Bool
prop_length_preorden x =
   length (preorden x) == nNodos x

-- La comprobación es
--    ghci> quickCheck prop_length_preorden
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.14. Demostrar por inducción que la longitud de la lista
-- obtenida recorriendo un árbol en sentido preorden es igual al número
-- de nodos del árbol.
-- ---------------------------------------------------------------------

{-
 Demostración: Por inducción en x, hay que demostrar que
    length (preorden x) == nNodos x
  
 Caso base: Hay que demostrar que 
    length (preorden Hoja) = nNodos Hoja
 En efecto,
    length (preorden Hoja)
    = length []              [por preorden.1]
    = 0                      [por length.1]
    = nNodos Hoja            [por nNodos.1]

 Paso de inducción: Se supone la hipótesis de inducción
    length (preorden i) == nNodos i
    length (preorden d) == nNodos d
 Hay que demostrar que
    length (preorden (Nodo x i d)) == nNodos (Nodo x i d)
 En efecto,
    length (preorden (Nodo x i d))
    = length ([x] ++ (peorden i) ++ (preorden d))   
         [por preorden.2]
    = length [x] + length (preorden i) + length (preorden d) 
         [propiedad de length: length (xs++ys) = length xs + length ys]
    = 1 + length (preorden i) + length (preorden d) 
         [por def. de length]
    = 1 + nNodos i + nNodos d
         [por hip. de inducción]
    = nNodos (x i d)
         [por nNodos.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.15. Definir la función
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
--    ghci> arbol
--    Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
--    ghci> profundidad arbol
--    3
-- ---------------------------------------------------------------------

profundidad :: Arbol a -> Int
profundidad Hoja = 0
profundidad (Nodo x i d) = 1 + max (profundidad i) (profundidad d)

-- ---------------------------------------------------------------------
-- Ejercicio 10.16. Comprobar con QuickCheck que para todo árbol biario
-- x, se tiene que
--    nNodos x <= 2^(profundidad x) - 1
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nNodosProfundidad :: Arbol Int -> Bool
prop_nNodosProfundidad x =
   nNodos x <= 2^(profundidad x) - 1

-- La comprobación es
--    ghci> quickCheck prop_nNodosProfundidad
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.17. Demostrar por inducción que para todo árbol binario
-- x, se tiene que
--    nNodos x <= 2^(profundidad x) - 1
-- ---------------------------------------------------------------------

{-
 Demostración por inducción en x
 
 Caso base: Hay que demostrar que 
    nNodos Hoja <= 2^(profundidad Hoja) - 1
 En efecto,
    nNodos Hoja
    = 0                            [por nNodos.1]
    = 2^0 - 1                      [por aritmética]
    = 2^(profundidad Hoja) - 1     [por profundidad.1]

 Paso de inducción: Se supone la hipótesis de inducción
    nNodos i <= 2^(profundidad i) - 1    
    nNodos d <= 2^(profundidad d) - 1    
 Hay que demostrar que 
    nNodos (Nodo x i d) <= 2^(profundidad (Nodo x i d)) - 1    
 En efecto,
    nNodos (Nodo x i d)
    =  1 + nNodos i + nNodos d    
          [por nNodos.1]
    <= 1 + (2^(profundidad i) - 1) + (2^(profundidad d) - 1)
          [por hip. de inducción]
    =  2^(profundidad i) + 2^(profundidad d) - 1   
          [por aritmética]
    <= 2^máx(profundidad i,profundidad d)+2^máx(profundidad i,profundidad d)-1
          [por aritmética]
    =  2*2^máx(profundidad i,profundidad d) - 1
          [por aritmética]
    =  2^(1+máx(profundidad i,profundidad d)) - 1
          [por aritmética]
    =  2^profundidad(Nodo x i d) - 1
          [por profundidad.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.18. Definir la función
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    ghci> arbol
--    Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
--    ghci> nHojas arbol
--    6
-- ---------------------------------------------------------------------

nHojas :: Arbol a -> Int
nHojas Hoja         = 1
nHojas (Nodo x i d) = nHojas i + nHojas d

-- ---------------------------------------------------------------------
-- Ejercicio 10.19. Comprobar con QuickCheck que en todo árbol binario el
-- número de sus hojas es igual al número de sus nodos más uno.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_nHojas :: Arbol Int -> Bool
prop_nHojas x =
    nHojas x == nNodos x + 1

-- La comprobación es
--    ghci> quickCheck prop_nHojas
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.20. Demostrar por inducción que en todo árbol binario el
-- número de sus hojas es igual al número de sus nodos más uno.
-- ---------------------------------------------------------------------

{-
 Demostración: Hay que demostrar, por inducción en x, que
    nHojas x = nNodos x + 1

 Caso base: Hay que demotrar que
    nHojas Hoja = nNodos Hoja + 1
 En efecto, 
    nHojas Hoja
    = 1                 [por nHojas.1]
    = 0 + 1             [por aritmética]
    = nNodos Hoja + 1   [por nNodos.1]

 Paso de inducción: Se supone la hipótesis de inducción
    nHojas i = nNodos i + 1
    nHojas d = nNodos d + 1
 Hay que demostrar que
    nHojas (Nodo x i d) = nNodos (Nodo x i d) + 1
 En efecto,
    nHojas (Nodo x i d)
    = nHojas i + nHojas d               [por nHojas.2]
    = (nNodos i + 1) + (nNodos d +1)    [por hip. de inducción]
    = (1 + nNodos i + nNodos d) + 1     [por aritmética]
    = nNodos (Nodo x i d) + 1           [por nNodos.2]
-}

-- ---------------------------------------------------------------------
-- Ejercicio 10.21. Definir, usando un acumulador, la función
--    preordenIt :: Arbol a -> [a]
-- tal que (preordenIt x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    ghci> arbol
--    Nodo 9 (Nodo 3 (Nodo 2 Hoja Hoja) (Nodo 4 Hoja Hoja)) (Nodo 7 Hoja Hoja)
--    ghci> preordenIt arbol
--    [9,3,2,4,7]
-- Nota: No usar (++) en la definición
-- ---------------------------------------------------------------------

preordenIt :: Arbol a -> [a]
preordenIt x = preordenItAux x []

preordenItAux :: Arbol a -> [a] -> [a]
preordenItAux Hoja xs         = xs
preordenItAux (Nodo x i d) xs = x : preordenItAux i (preordenItAux d xs)

-- ---------------------------------------------------------------------
-- Ejercicio 10.22. Comprobar con QuickCheck que preordenIt es
-- equivalente a preorden.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_preordenIt :: Arbol Int -> Bool
prop_preordenIt x =
    preordenIt x == preorden x

-- La comprobación es
--    ghci> quickCheck prop_preordenIt
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 10.22. Demostrar que preordenIt es equivalente a preorden.
-- ---------------------------------------------------------------------

prop_preordenItAux :: Arbol Int -> [Int] -> Bool
prop_preordenItAux x ys =
   preordenItAux x ys == preorden x ++ ys

{-
 Demostración: La propiedad es consecuencia del siguiente lema:
 
 Lema: Para todo árbol binario x, se tiene que 
    para toda ys, preordenItAux x ys = preorden x ++ ys

 Demostración de la propiedad usando el lema:
    preordenIt x
    = preordenItAux x []    [por preordnIt]
    = preorden x ++ []      [por el lema]
    = preorden x            [propiedad de ++]

 Demostración del lema: Por inducción en x.

 Caso base: Hay que demotrar que
    para toda ys, preordenItAux Hoja ys = preorden Hoja ++ ys
 En efecto, 
    preordenItAux Hoja ys
    = ys                     [por preordenItAux.1]
    = [] ++ ys               [por propiedad de ++]
    = preorden Hoja ++ ys    [por preorden.1]
    
 Paso de inducción: Se supone la hipótesis de inducción
    para toda ys, preordenItAux i ys = preorden i ++ ys
    para toda ys, preordenItAux d ys = preorden d ++ ys
 Hay que demostrar que
    para toda ys, preordenItAux (Nodo x i d) ys = preorden (Nodo x i d) ++ ys
 En efecto,
    preordenItAux (Nodo x i d) ys
    = x : (preordenItAux i (preordenItAux d ys))   [por preordenItAux.2]
    = x : (preordenItAux i (preorden d ++ ys))     [por hip. de inducción]
    = x : (preorden i ++ (preorden d ++ ys))       [por hip. de inducción]
    = ([x] ++ preorden i ++ preorden d) ++ ys      [por prop. de listas]
    = preorden (Nodo x i d) ++ ys                  [por preorden.2]
-}

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- El objetivo de esta relación es implementar el TAD de los grafos
-- mediante listas, de manera análoga a las implementaciones estudiadas
-- en el tema 22 que se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-12/temas/tema-22.pdf
-- y usando la mismas signatura.

-- ---------------------------------------------------------------------
-- Signatura                                                          --
-- ---------------------------------------------------------------------

module Rel_29_sol
    (Orientacion (..),
     Grafo,
     creaGrafo,  -- (Ix v,Num p) => Orientacion -> (v,v) -> [(v,v,p)] -> 
                 --                 Grafo v p
     dirigido,   -- (Ix v,Num p) => (Grafo v p) -> Bool
     adyacentes, -- (Ix v,Num p) => (Grafo v p) -> v -> [v]
     nodos,      -- (Ix v,Num p) => (Grafo v p) -> [v]
     aristas,    -- (Ix v,Num p) => (Grafo v p) -> [(v,v,p)]
     aristaEn,   -- (Ix v,Num p) => (Grafo v p) -> (v,v) -> Bool
     peso        -- (Ix v,Num p) => v -> v -> (Grafo v p) -> p
    ) where

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

-- ---------------------------------------------------------------------
-- Representación de los grafos mediante listas                       --
-- ---------------------------------------------------------------------

-- Orientacion es D (dirigida) ó ND (no dirigida).
data Orientacion = D | ND
                   deriving (Eq, Show)

-- (Grafo v p) es un grafo con vértices de tipo v y pesos de tipo p.
data Grafo v p = G Orientacion ([v],[((v,v),p)])
                 deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Ejercicios                                                         --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función
--    creaGrafo :: (Ix v, Num p) => Bool -> (v,v) -> [(v,v,p)] -> Grafo v p
-- tal que (creaGrafo d cs as) es un grafo (dirigido o no, según el
-- valor de o), con el par de cotas cs y listas de aristas as (cada
-- arista es un trío formado por los dos vértices y su peso). Por
-- ejemplo, 
--    ghci> creaGrafo ND (1,3) [(1,2,12),(1,3,34)]
--    G ND ([1,2,3],[((1,2),12),((1,3),34),((2,1),12),((3,1),34)])
--    ghci> creaGrafo D (1,3) [(1,2,12),(1,3,34)]
--    G D ([1,2,3],[((1,2),12),((1,3),34)])
--    ghci> creaGrafo D (1,4) [(1,2,12),(1,3,34)]
--    G D ([1,2,3,4],[((1,2),12),((1,3),34)])
-- ---------------------------------------------------------------------

creaGrafo :: (Ix v, Num p) => 
             Orientacion -> (v,v) -> [(v,v,p)] -> Grafo v p
creaGrafo o cs as = 
    G o (range cs, [((x1,x2),w) | (x1,x2,w) <- as] ++ 
                    if o == D then []
                    else [((x2,x1),w) | (x1,x2,w) <- as, x1 /= x2])

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir, con creaGrafo, la constante 
--    ejGrafoND :: Grafo Int Int 
-- para representar el siguiente grafo no dirigido
--             12
--        1 -------- 2
--        | \78     /|
--        |  \   32/ |
--        |   \   /  |
--      34|     5    |55
--        |   /   \  |
--        |  /44   \ |
--        | /     93\|
--        3 -------- 4
--             61
--    ghci> ejGrafoND
--    G ND ([1,2,3,4,5],
--          [((1,2),12),((1,3),34),((1,5),78),((2,4),55),((2,5),32),
--           ((3,4),61),((3,5),44),((4,5),93),((2,1),12),((3,1),34),
--           ((5,1),78),((4,2),55),((5,2),32),((4,3),61),((5,3),44),
--           ((5,4),93)])
-- ---------------------------------------------------------------------

ejGrafoND :: Grafo Int Int 
ejGrafoND = creaGrafo ND (1,5) [(1,2,12),(1,3,34),(1,5,78),
                                (2,4,55),(2,5,32),
                                (3,4,61),(3,5,44),
                                (4,5,93)]

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir, con creaGrafo, la constante 
--    ejGrafoD :: Grafo Int Int 
-- para representar el grafo anterior donde se considera que las aristas
-- son los pares (x,y) con x < y. Por ejemplo,
--    ghci> ejGrafoD
--    G D ([1,2,3,4,5],
--         [((1,2),12),((1,3),34),((1,5),78),((2,4),55),((2,5),32),
--          ((3,4),61),((3,5),44),((4,5),93)])
-- ---------------------------------------------------------------------

ejGrafoD :: Grafo Int Int
ejGrafoD = creaGrafo D (1,5) [(1,2,12),(1,3,34),(1,5,78),
                              (2,4,55),(2,5,32),
                              (3,4,61),(3,5,44),
                              (4,5,93)]

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función
--    dirigido :: (Ix v,Num p) => (Grafo v p) -> Bool
-- tal que (dirigido g) se verifica si g es dirigido. Por ejemplo,
--    dirigido ejGrafoD   ==  True
--    dirigido ejGrafoND  ==  False
-- ---------------------------------------------------------------------

dirigido :: (Ix v,Num p) => (Grafo v p) -> Bool
dirigido (G o _) = o == D

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
-- tal que (nodos g) es la lista de todos los nodos del grafo g. Por
-- ejemplo, 
--    nodos ejGrafoND  ==  [1,2,3,4,5]
--    nodos ejGrafoD   ==  [1,2,3,4,5]
-- ---------------------------------------------------------------------

nodos :: (Ix v,Num p) => (Grafo v p) -> [v]
nodos (G _ (ns,_)) = ns

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    adyacentes :: (Ix v, Num p) => Grafo v p -> v -> [v]
-- tal que (adyacentes g v) es la lista de los vértices adyacentes al
-- nodo v en el grafo g. Por ejemplo,
--    adyacentes ejGrafoND 4  ==  [5,2,3]
--    adyacentes ejGrafoD  4  ==  [5]
-- ---------------------------------------------------------------------

adyacentes :: (Ix v, Num p) => Grafo v p -> v -> [v]
adyacentes (G _ (_,e)) v = nub [u | ((w,u),_) <- e, w == v]

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
-- (aristaEn g a) se verifica si a es una arista del grafo g. Por
-- ejemplo,
--    aristaEn ejGrafoND (5,1)  ==  True
--    aristaEn ejGrafoND (4,1)  ==  False
--    aristaEn ejGrafoD  (5,1)  ==  False
--    aristaEn ejGrafoD  (1,5)  ==  True
-- ---------------------------------------------------------------------

aristaEn :: (Ix v,Num p) => Grafo v p -> (v,v) -> Bool
aristaEn g (x,y) = y `elem` adyacentes g x

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
-- tal que (peso v1 v2 g) es el peso de la arista que une los vértices
-- v1 y v2 en el grafo g. Por ejemplo,
--    peso 1 5 ejGrafoND  ==  78
--    peso 1 5 ejGrafoD   ==  78
-- ---------------------------------------------------------------------

peso :: (Ix v,Num p) => v -> v -> Grafo v p -> p
peso x y (G _ (_,gs)) = head [c | ((x',y'),c) <- gs, x==x', y==y']

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función
--    aristas :: (Ix v,Num p) => Grafo v p -> [(v,v,p)]
-- (aristasD g) es la lista de las aristas del grafo g. Por ejemplo, 
--    ghci> aristas ejGrafoD
--    [(1,2,12),(1,3,34),(1,5,78),(2,4,55),(2,5,32),(3,4,61),
--     (3,5,44),(4,5,93)] 
--    ghci> aristas ejGrafoND
--    [(1,2,12),(1,3,34),(1,5,78),(2,1,12),(2,4,55),(2,5,32),
--     (3,1,34),(3,4,61),(3,5,44),(4,2,55),(4,3,61),(4,5,93),
--     (5,1,78),(5,2,32),(5,3,44),(5,4,93)]
-- ---------------------------------------------------------------------

aristas :: (Ix v,Num p) => Grafo v p -> [(v,v,p)]
aristas (G _ (_,g)) = [(v1,v2,p) | ((v1,v2),p) <- g]

-- ---------------------------------------------------------------------
-- Importación de librerías auxiliares                                --
-- ---------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir, por recursión, la función 
--    sumaCuadradosR :: Integer -> Integer
-- tal que (sumaCuadradosR n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosR 4  ==  30 
-- ---------------------------------------------------------------------

sumaCuadradosR :: Integer -> Integer
sumaCuadradosR 0 = 0
sumaCuadradosR n = n*n + sumaCuadradosR n 

-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Comprobar con QuickCheck si sumaCuadradosR n es igual a
-- n(n+1)(2n+1)/6. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_SumaCuadrados n =
  n >= 0 ==>
    sumaCuadradosR n == n * (n+1) * (2*n+1) `div` 6  

-- La comprobación es
--    Main> quickCheck prop_SumaCuadrados
--    OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 1.3. Definir, por comprensión, la función 
--    sumaCuadradosC :: Integer --> Integer
-- tal que (sumaCuadradosC n) es la suma de los cuadrados de los números
-- de 1 a n. Por ejemplo, 
--    sumaCuadradosC 4  ==  30 
-- ---------------------------------------------------------------------

sumaCuadradosC :: Integer -> Integer
sumaCuadradosC n = sum [x^2 | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 1.4. Comprobar con QuickCheck que las funciones
-- sumaCuadradosR y sumaCuadradosC son equivalentes sobre los números
-- naturales. 
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaCuadradosR n =
    n >= 0 ==> sumaCuadradosR n == sumaCuadradosC n

-- La comprobación es
--    *Main> quickCheck prop_sumaCuadrados
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Se quiere formar una escalera con bloques cuadrados,
-- de forma que tenga un número determinado de escalones. Por ejemplo,
-- una escalera con tres escalones tendría la siguiente forma:
--        XX
--      XXXX
--    XXXXXX
-- Definir, por recursión, la función 
--    numeroBloquesR :: Integer -> Integer    
-- tal que (numeroBloquesR n) es el número de bloques necesarios para
-- construir una escalera con n escalones. Por ejemplo,
--    numeroBloquesR 1   == 2
--    numeroBloquesR 3   == 12
--    numeroBloquesR 10  == 110
-- ---------------------------------------------------------------------

numeroBloquesR :: Integer -> Integer    
numeroBloquesR 0 = 0
numeroBloquesR n = 2*n + numeroBloquesR (n-1) 

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir, por comprensión, la función 
--    numeroBloquesC :: Integer -> Integer    
-- tal que (numeroBloquesC n) es el número de bloques necesarios para
-- construir una escalera con n escalones. Por ejemplo,
--    numeroBloquesC 1   == 2
--    numeroBloquesC 3   == 12
--    numeroBloquesC 10  == 110
-- ---------------------------------------------------------------------

numeroBloquesC :: Integer -> Integer    
numeroBloquesC n = sum [2*x | x <- [1..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Comprobar con QuickCheck que (numeroBloquesC n) es
-- igual a n+n^2.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_numeroBloquesR n =
    n >0 ==> numeroBloquesC n == n+n^2

-- La comprobación es
--    *Main> quickCheck prop_numeroBloques
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir, por recursión, la función 
--    sumaCuadradosImparesR :: Integer -> Integer
-- tal que (sumaCuadradosImparesR n) es la suma de los cuadrados de los
-- números impares desde 1 hasta n. 
--    sumaCuadradosImparesR 1  ==  1
--    sumaCuadradosImparesR 7  ==  84
--    sumaCuadradosImparesR 4  ==  10
-- ---------------------------------------------------------------------

sumaCuadradosImparesR :: Integer -> Integer
sumaCuadradosImparesR 1 = 1
sumaCuadradosImparesR n 
    | odd n     = n^2 + sumaCuadradosImparesR (n-1)
    | otherwise = sumaCuadradosImparesR (n-1)
                     
-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir, por comprensión, la función 
--    sumaCuadradosImparesC :: Integer -> Integer
-- tal que (sumaCuadradosImparesC n) es la suma de los cuadrados de los
-- números impares desde 1 hasta n. 
--    sumaCuadradosImparesC 1  ==  1
--    sumaCuadradosImparesC 7  ==  84
--    sumaCuadradosImparesC 4  ==  10
-- ---------------------------------------------------------------------

sumaCuadradosImparesC :: Integer -> Integer
sumaCuadradosImparesC n = sum [x^2 | x <- [1..n], odd x]

-- Otra definición más simple es
sumaCuadradosImparesC' :: Integer -> Integer
sumaCuadradosImparesC' n = sum [x^2 | x <- [1,3..n]]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir, por recursión, la función
--    digitosR :: Integer -> [Integer]
-- tal que (digitosR n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosR 320274  ==  [3,2,0,2,7,4]
-- ---------------------------------------------------------------------

digitosR :: Integer -> [Integer]
digitosR n = reverse (digitosR' n)

digitosR' n
    | n < 10    = [n]
    | otherwise = (n `rem` 10) : digitosR' (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Definir, por comprensión, la función
--    digitosC :: Integer -> [Integer]
-- tal que (digitosC n) es la lista de los dígitos del número n. Por
-- ejemplo, 
--    digitosC 320274  ==  [3,2,0,2,7,4]
-- Indicación: Usar las funciones show y read.
-- ---------------------------------------------------------------------

digitosC :: Integer -> [Integer]
digitosC n = [read [x] | x <- show n]

-- ---------------------------------------------------------------------
-- Ejercicio 4.3. Comprobar con QuickCheck que las funciones digitosR y
-- digitosC son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_digitos n =
    n >= 0 ==> 
    digitosR n == digitosC n
  
-- La comprobación es
--    *Main> quickCheck prop_digitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir, por recursión, la función 
--    sumaDigitosR :: Integer -> Integer
-- tal que (sumaDigitosR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosR 3     ==  3
--    sumaDigitosR 2454  == 15
--    sumaDigitosR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosR :: Integer -> Integer
sumaDigitosR n
    | n < 10    = n
    | otherwise = n `rem` 10 + sumaDigitosR (n `div` 10)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Definir, sin usar recursión, la función 
--    sumaDigitosNR :: Integer -> Integer
-- tal que (sumaDigitosNR n) es la suma de los dígitos de n. Por ejemplo,
--    sumaDigitosNR 3     ==  3
--    sumaDigitosNR 2454  == 15
--    sumaDigitosNR 20045 == 11
-- ---------------------------------------------------------------------

sumaDigitosNR :: Integer -> Integer
sumaDigitosNR n = sum (digitosC n)

-- ---------------------------------------------------------------------
-- Ejercicio 5.3. Comprobar con QuickCheck que las funciones sumaDigitosR
-- y sumaDigitosNR son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_sumaDigitos n =
    n >= 0 ==>
    sumaDigitosR n == sumaDigitosNR n

-- La comprobación es
--    *Main> quickCheck prop_sumaDigitos
--    +++ OK, passed 100 tests.

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función 
--    esDigito :: Integer -> Integer -> Bool
-- tal que (esDigito x n) se verifica si x es un dígito de n. Por
-- ejemplo, 
--    esDigito 4 1041  ==  True
--    esDigito 3 1041  ==  False
-- ---------------------------------------------------------------------

esDigito :: Integer -> Integer -> Bool
esDigito x n = x `elem` digitosC n

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    numeroDeDigitos :: Integer -> Integer
-- tal que (numeroDeDigitos x) es el número de dígitos de x. Por ejemplo,
--    numeroDeDigitos 34047  ==  5
-- ---------------------------------------------------------------------

numeroDeDigitos :: Integer -> Int
numeroDeDigitos x = length (digitosC x)