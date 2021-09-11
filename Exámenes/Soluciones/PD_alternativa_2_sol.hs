-- --------------------------------------------------------------------
-- Ejercicio 1 . 
-- A) Definir una funcion (ramaCentral ar) que, dado un arbol ternario
-- que responde al tipo:
data Arbol a =  H a  | N a (Arbol a) (Arbol a) (Arbol a)
                deriving Show
-- devuelva la rama central del arbol ar, es decir, la lista de los
-- nodos que estan en el centro.
-- Por ejemplo, para el arbol:
arbolito = (N 3 
                  (H 4)
                  (N 5  
                         (N 1 (H 2) (H 4) (H 1))
                         (N 0 (H 3) (H 1) (H 5))
                         (H 1))
                  (N 7 (H 4) (H 4) (H 4)))
-- la funcion devolvera:
-- ramaCentral arbolito == [3,5,0,1]
-- La representacion de arbolito es:
--                    3
--          /         |       \
--         4          5        7
--               /    |  \    /|\
--              1     0   1  4 4 4 
--             /|\   /|\
--            2 4 1 3 1 5
ramaCentral :: Arbol a -> [a]
ramaCentral (H x) = [x]
ramaCentral (N x ai ac ad)= x:ramaCentral ac
-- B) Para el mismo tipo de arbol, definir una funcion (ramas ar) que
-- devuelva la lista de ramas del arbol. Por ejemplo:
-- ramas arbolito = [[3,4],
--                  [3,5,1,2],[3,5,1,4],[3,5,1,1],
--                  [3,5,0,3],[3,5,0,1],[3,5,0,5],
--                  [3,5,1],
--                  [3,7,4],[3,7,4],[3,7,4]]
ramas (H x) = [[x]]
ramas (N x ai ac ad) = [x:ys|ys<-ramas ai]++[x:ys|ys<-ramas ac]++[x:ys|ys<-ramas ad]
-- C) Para el mismo tipo de arbol, definir una funcion (mini ar) que
-- devuelva el menor valor almacenado en el arbol ar. Para el ejemplo
-- arbolito: 
-- mini arbolito == 0
mini (H x) = x
mini (N x ai ac ad) = minimum [x, mini ai, mini ac, mini ad]
-- --------------------------------------------------------------------
-- Ejercicio 2 . 
-- Los divisores medios de un n�mero son los que ocupan la
-- posici�n media entre los divisores de n, ordenados de menor a mayor. Por
-- ejemplo, los divisores de 60 son [1,2,3,4,5,6,10,12,15,20,30,60] y sus
-- divisores medios son 6 y 10. En el caso de 36, sus divisores son 
-- [1,2,3,4,6,9,12,18,36] y la posicion media la ocupa el 6.
-- A) Definir la funcion (divisoresMedios x) que, dado un numero x
-- devuelva el par de los divisores medios de x. Por ejemplo:
-- divisoresMedios 60 == (6,10)
-- divisoresMedios 36 == (6,6)
divisores x = [y|y<-[1 .. x], mod x y == 0]
-- Si la longitud de divisores es par, entonces el par de los centrales,
-- pero si es impar, devuelve el central repetido.
divisoresMedios x = if even l
                    then (xs!!((div l 2)-1), 
                          xs!!(div l 2))
                    else (xs!!(div l 2), xs!!(div l 2))
    where xs = divisores x
          l = length xs
-- B)
-- El �rbol binario de factorizaci�n de un n�mero compuesto n se
-- construye de la siguiente manera:
-- * la ra�z es el n�mero n,
-- * la rama izquierda es el �rbol de factorizaci�n de su primer divisor medio
-- * la rama derecha es el �rbol de factorizaci�n de su segundo divisor medio 
--
-- Si el n�mero es primo, su �rbol de factorizaci�n s�lo tiene una hoja con
-- dicho n�mero.
-- El tipo de dato arbol binario sera:
data ArbolBin a = Hoja a | Nodo a (ArbolBin a) (ArbolBin a)
                  deriving Show
-- Por ejemplo, el �rbol de factorizaci�n de 60 es
--
--        60
--       /  \
--      6    10
--     / \   / \
--    2   3 2   5
--
-- Definir la funci�n
-- arbolFac :: Int -> ArbolBin Int
-- tal que '(arbolFac n)' es el �rbol de factorizaci�n de 'n'. Por
-- ejemplo,
-- arbolFac 60  ==  Nodo 60 (Nodo 6 (Hoja 2) (Hoja 3)) (Nodo 10 (Hoja 2) (Hoja 5))
-- arbolFac 45  ==  Nodo 45 (Hoja 5) (Nodo 9 (Hoja 3) (Hoja 3))
-- arbolFac 7   ==  Hoja 7
-- arbolFac 36  == Nodo 36 (Nodo 6 (Hoja 2) (Hoja 3)) (Nodo 6 (Hoja 2) (Hoja 3))
arbolFac :: Int -> ArbolBin Int
arbolFac x |primo x = Hoja x
           |otherwise = Nodo x (arbolFac i) (arbolFac d)
           where (i,d) = divisoresMedios x
                 primo n = length (divisores n) == 2
-- --------------------------------------------------------------------
-- Ejercicio 3 . 
-- Una tanda de longitud n de una lista xs es una sublista de xs
-- formada por n elementos iguales.
--
-- Definir la funci�n
--    tandas :: Eq a => Int -> [a] -> [[a]]
-- tal que (tandas n xs) es la lista de las tandas de xs que tienen
-- n elementos como m�nimo. Por ejemplo,
--    tandas 3  "aabbbcddddffxffxx" ==  ["bbb","dddd"]
--    tandas 4  "aabbbcddddffxffxx" ==  ["dddd"]
--    tandas 5  "aabbbcddddffxffxx" ==  []
-- Definici�n (por recursi�n con takeWhile y dropWhile):
tandas :: Eq a => Int -> [a] -> [[a]]
tandas _ [] = []
tandas n xs@(x:_) 
    | length ys >= n = ys : tandas n (dropWhile (x==) xs) 
    | otherwise      = tandas n (dropWhile (x==) xs) 
    where ys = takeWhile (x==) xs

-- --------------------------------------------------------------------
-- Ejercicio 4 . 
-- Una funci�n de precio determina el precio de cada elemento; por
-- ejemplo,
precioCI :: String -> Int
precioCI "leche"       = 10
precioCI "mantequilla" = 18
precioCI "patatas"     = 22
precioCI "chocolate"   = 16
-- 
-- Definir (POR COMPRENSION, RECURSION Y PLEGADO) la funci�n
--    precioTotal :: (String -> Int) -> [String] -> Int
-- tal que (precioTotal f xs) es el precio total de los elementos de xs
-- respecto de la funci�n de precio f. Por ejemplo,
--    precioTotal precioCI ["leche", "leche", "mantequilla"]  ==  38
--    precioTotal precioCI ["chocolate", "mantequilla"]       ==  34
-- 1� soluci�n (por comprensi�n):
precioTotal1 :: (String -> Int) -> [String] -> Int
precioTotal1 f xs = sum [f x | x <- xs]
 
-- 2� soluci�n (por recursi�n):
precioTotal2 :: (String -> Int) -> [String] -> Int
precioTotal2 f []     = 0
precioTotal2 f (x:xs) = f x + precioTotal2 f xs
 
-- 3� soluci�n (por plegado)
precioTotal3 :: (String -> Int) -> [String] -> Int
precioTotal3 f = foldr g 0
    where g x y = f x + y
