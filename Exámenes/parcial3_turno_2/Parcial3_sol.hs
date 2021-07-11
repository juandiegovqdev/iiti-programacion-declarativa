-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 3 (turno 2)                                21 de Enero de 2021
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- ----------------------------------------------------------------------

import Data.Array
import I1M.Pila
import Data.List

-- ---------------------------------------------------------------------
-- Ejercicio 1. (2,5 puntos)
-- ---------------------------------------------------------------------
-- La sucesión de Loomis generada por un número entero positivo 'x' es la 
-- sucesión cuyos términos se definen por:

--   f(0) es x
--   f(n) es la suma de f(n-1) y el producto de los dígitos no nulos de f(n-1)

-- Por ejemplo, si f(n) es 62, entonces f(n+1) es 62 + (6*2) = 74. Los primeros
-- términos de las primeras sucesiones de Loomis son:

-- Generada por 1: 1, 2, 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 116, 122, …
-- Generada por 2: 2, 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 116, 122, 126, …
-- Generada por 3: 3, 6, 12, 14, 18, 26, 38, 62, 74, 102, 104, 108, 116, 122, 126, …
-- Generada por 4: 4, 8, 16, 22, 26, 38, 62, 74, 102, 104, 108, 116, 122, 126, 138, …
-- Generada por 5: 5, 10, 11, 12, 14, 18, 26, 38, 62, 74, 102, 104, 108, 116, 122, …

-- Ejercicio 1.a (1 punto) Define la función  
--    sucLoomis :: Integer -> [Integer]
-- tal que (sucLoomis x) es la sucesión de Loomis generada por x. Por ejemplo,
--  λ> take 15 (sucLoomis 1)
--  [1,2,4,8,16,22,26,38,62,74,102,104,108,116,122]
--  λ> take 15 (sucLoomis 2)
--  [2,4,8,16,22,26,38,62,74,102,104,108,116,122,126]
--  λ> take 15 (sucLoomis 3)
--  [3,6,12,14,18,26,38,62,74,102,104,108,116,122,126]
--  λ> take 15 (sucLoomis 4)
--  [4,8,16,22,26,38,62,74,102,104,108,116,122,126,138]
--  λ> take 15 (sucLoomis 100)
--  [100,101,102,104,108,116,122,126,138,162,174,202,206,218,234]
--  λ> sucLoomis 1 !! (2*10^5)
--  235180736652
-- ---------------------------------------------------------------------

-- Una definición de sucLoomis
-- ==========================
 
sucLoomis :: Integer -> [Integer]
sucLoomis x = x: sucLoomis y
  where y = x + productoDigitosNoNulos x
 
productoDigitosNoNulos :: Integer -> Integer
productoDigitosNoNulos = product . digitosNoNulos
 
digitosNoNulos :: Integer -> [Integer]
digitosNoNulos x = [read [c] | c <- show x, c /= '0']
 
-- Otra definición de sucLoomis (con orden superior)
-- ==========================
 
sucLoomis2 :: Integer -> [Integer]
sucLoomis2 = iterate siguienteLoomis 
 
siguienteLoomis :: Integer -> Integer
siguienteLoomis y = y + productoDigitosNoNulos y

-- ---------------------------------------------------------------------
-- Ejercicio 1.b (1,5 puntos) Se observa que a partir de un término todas 
-- coinciden con la generada por el 1. Dicho término se llama el punto de
-- convergencia. Por ejemplo,

-- la generada por 2 converge a 2    (el 2 pertence a la sucesión del 1)
-- la generada por 3 converge a 26   (el 26 pertence a la sucesión del 1)
-- la generada por 4 converge a 4    (el 4 pertence a la sucesión del 1)
-- la generada por 5 converge a 26   ... 

-- Define la función 
--    convergencia :: Integer -> Integer
-- (convergencia x) es el término de convergencia de la sucesión de Loomis
-- generada por x con la generada por el 1. Por ejemplo,
--  convergencia  2      ==  2
--  convergencia  3      ==  26
--  convergencia  4      ==  4
--  convergencia 17      ==  38
--  convergencia 89      ==  150056
--  convergencia (10^12) ==  1000101125092
-- Nota: observa que las sucesiones son crecientes (cada término es más
-- grande que el anterior), esto te puede ayudar.
-- ---------------------------------------------------------------------

-- Una definición de convergencia
-- =============================
 
convergencia1 :: Integer -> Integer
convergencia1 x =
  head (dropWhile noEnSucLoomisDe1 (sucLoomis x))
 
noEnSucLoomisDe1 :: Integer -> Bool
noEnSucLoomisDe1 x = not (pertenece x sucLoomisDe1)
 
sucLoomisDe1 :: [Integer]
sucLoomisDe1 = sucLoomis 1
 
pertenece :: Integer -> [Integer] -> Bool
pertenece x ys =
  x == head (dropWhile (<x) ys) -- asumiendo que es creciente
 
-- Otra definición de convergencia
-- =============================
 
convergencia2 :: Integer -> Integer
convergencia2 x = aux (sucLoomis2 1) (sucLoomis2 x)
 where aux as@(x:xs) bs@(y:ys) | x == y    = x
                               | x < y     = aux xs bs
                               | otherwise = aux as ys


-- ---------------------------------------------------------------------
-- Ejercicio 2. (2 puntos)
-- ---------------------------------------------------------------------
-- Los árboles se pueden representar mediante el siguiente tipo de datos

data Arbol a = N a [Arbol a]
  deriving (Show, Eq)

-- 
-- Por ejemplo, los árboles
--      1               3
--     / \             /|\
--    6   3           / | \
--        |          5  4  7
--        5          |     /\
--                   6    2  1
-- se representan por

ej1, ej2 :: Arbol Int
ej1 = N 1 [N 6 [],N 3 [N 5 []]]
ej2 = N 3 [N 5 [N 6 []], N 4 [], N 7 [N 2 [], N 1 []]]

-- Define la función (propaga a), tal que reciba un árbol a, y devuelva
-- otro árbol donde el valor de cada nodo del árbol a se sume a sus hijos.
-- Por ejemplo, sobre los ejemplos anteriores, obtendríamos:
--      1               3
--     / \             /|\
--    7   4           / | \
--        |          8  7  10
--        9          |     /\
--                  12   12  11
-- > propaga ej1
-- N 1 [N 7 [],N 4 [N 9 []]]
-- > propaga ej2
-- N 3 [N 8 [N 14 []],N 7 [],N 10 [N 12 [],N 11 []]]
-- ---------------------------------------------------------------------

propaga :: Num a => Arbol a -> Arbol a
propaga (N x as) = N x (map (prop x) as)
  where prop x (N y as) = (N (x+y) (map (prop (x+y)) as))

-- -------------------------------------------------------------------
-- Ejercicio 3. (2,5 puntos)
-- ------------------------------------------------------------------- 
-- Una forma de detectar que todos los paréntesis de una cadena están
-- cerrados es empleando el siguiente algoritmo basado en pilas:
--   * Cada vez que nos encontremos un '(', metemos su posición en la 
--     pila.
--   * Cada vez que nos encontremos un ')', si la pila es vacía,
--     paramos y devolvemos su posición; si no, sacamos un elemento de 
--     la pila.
--   * Al terminar de recorrer la cadena, si la pila está vacía, la
--     secuencia de paréntesis es correcta y devolvemos nothing, si no,
--     devolvemos la cima de la pila.

-- Define la función
--   comprueba :: String -> Maybe Int
-- tal que reciba una cadena y la recorra con el método descrito
-- usando el TAD Pila. Por ejemplo,
-- λ> comprueba "fib n = (fib (n-1)) + (fib (n-2))"
--    Nothing
-- λ> comprueba "fib n = (fib (n-1) + (fib (n-2))"
--    Just 9
-- λ> comprueba ")a("
--    Just 1
-- λ> comprueba "(x+1)))"
--    Just 6
-- -------------------------------------------------------------------

comprueba :: String -> Maybe Int
comprueba cs = comprueba' (zip cs [1..]) (vacia)

comprueba' :: [(Char,Int)] -> Pila Int -> Maybe Int
comprueba' [] p | esVacia p = Nothing
                | otherwise = Just (cima p)
comprueba' ((c,i):cs) p
  | c == '(' = comprueba' cs (apila i p)
  | c == ')' && (esVacia  p) = Just i 
  | c == ')' && not (esVacia  p) = comprueba' cs (desapila p)
  | otherwise = comprueba' cs p
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 4. (3 puntos)
-- ------------------------------------------------------------------- 
-- En este ejercicio vamos a trabajar con un Sudoku de 9x9 (solo de 
-- este tamaño, así que puedes usar el 9 sin problema en las
-- soluciones). Define un programa interactivo donde (cada ítem vale 
-- 1 punto):
--    * Se le muestre al usuario la matriz definida en la variable
--      sudoku (fíjate en el formato del ejemplo).
--    * Le pida al usuario una fila, una columna, y un número del 1 al 9.
--      Por simplicidad, asume que es los números introducidos son correctos.
--    * Compruebe si la inserción es posible realizarla en el sudoku. Es
--      decir, si hay un 0 en esa posición, y ese número no está en la fila,
--      en la columna, ni en la submatriz de 3x3. Debe indicar si el 
--      movimiento es correcto o no, y acabar.
-- Ejemplos:
-- *Main> main
-- [9,2,0,8,0,0,0,4,0]
-- [0,0,1,3,4,0,0,0,6]
-- [4,0,3,0,5,6,8,0,0]
-- [2,1,0,0,6,4,5,0,0]
-- [0,0,4,5,0,0,0,0,0]
-- [6,0,5,1,0,2,4,7,0]
-- [0,0,8,0,2,0,0,1,3]
-- [0,4,0,0,1,0,0,0,0]
-- [0,9,7,6,0,3,2,0,4]
-- Indica fila
-- 9
-- Indica columna
-- 1
-- Indica número
-- 2
-- Incorrecto
-- *Main> main
-- [9,2,0,8,0,0,0,4,0]
-- [0,0,1,3,4,0,0,0,6]
-- [4,0,3,0,5,6,8,0,0]
-- [2,1,0,0,6,4,5,0,0]
-- [0,0,4,5,0,0,0,0,0]
-- [6,0,5,1,0,2,4,7,0]
-- [0,0,8,0,2,0,0,1,3]
-- [0,4,0,0,1,0,0,0,0]
-- [0,9,7,6,0,3,2,0,4]
-- Indica fila
-- 9
-- Indica columna
-- 1
-- Indica número
-- 8
-- Incorrecto
-- *Main> main
-- [9,2,0,8,0,0,0,4,0]
-- [0,0,1,3,4,0,0,0,6]
-- [4,0,3,0,5,6,8,0,0]
-- [2,1,0,0,6,4,5,0,0]
-- [0,0,4,5,0,0,0,0,0]
-- [6,0,5,1,0,2,4,7,0]
-- [0,0,8,0,2,0,0,1,3]
-- [0,4,0,0,1,0,0,0,0]
-- [0,9,7,6,0,3,2,0,4]
-- Indica fila
-- 9
-- Indica columna
-- 1
-- Indica número
-- 5
-- Correcto

type Sudoku = Array (Int,Int) Int

sudoku :: Sudoku
sudoku = listArray ((1,1),(9,9)) [9,2,0,8,0,0,0,4,0,
                                  0,0,1,3,4,0,0,0,6,
                                  4,0,3,0,5,6,8,0,0,
                                  2,1,0,0,6,4,5,0,0,
                                  0,0,4,5,0,0,0,0,0,
                                  6,0,5,1,0,2,4,7,0,
                                  0,0,8,0,2,0,0,1,3,
                                  0,4,0,0,1,0,0,0,0,
                                  0,9,7,6,0,3,2,0,4]

filasSudoku :: [a] -> [[a]]
filasSudoku [] = []
filasSudoku xs = take 9 xs : filasSudoku (drop 9 xs)

imprimeSudoku :: Sudoku -> IO ()
imprimeSudoku a = sequence_ (map (putStrLn.show) (filasSudoku (elems a)))

posible :: Int -> Int -> Int -> Sudoku -> Bool 
posible f c n a = a!(f,c) == 0 
      && and [a!(i,c)/=n | i <- [1..9]]
      && and [a!(f,j)/=n | j <- [1..9]]
      && and [a!(i,j)/=n | i <- [3*fs+1..3*fs+3], j <- [3*cs+1..3*cs+3]]
      where fs = div (f-1) 3
            cs = div (c-1) 3

main :: IO()
main = do
  imprimeSudoku sudoku
  putStrLn "Indica fila"
  fs <- getLine
  let f = read fs :: Int
  putStrLn "Indica columna"
  cs <- getLine
  let c = read cs :: Int
  putStrLn "Indica número"
  ns <- getLine
  let n = read ns :: Int
  if (posible f c n sudoku) then
      putStrLn "Correcto" else 
      putStrLn "Incorrecto"

-- -------------------------------------------------------------------
