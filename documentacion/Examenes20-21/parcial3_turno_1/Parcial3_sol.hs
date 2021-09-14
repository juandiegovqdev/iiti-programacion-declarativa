import Data.Array
import Data.List
--import I1M.Cola
import ColaConListas
import System.Directory

-- ---------------------------------------------------------------------
-- Ejercicio 1.
-- ---------------------------------------------------------------------
-- La siguiente sucesión de números naturales
--    binarioPar :: [Integer]
-- es tal que el número n pertenece a la sucesión si su representación en 
-- binario acaba en un número par de ceros. Los primeros términos son:
-- 1, 3, 4, 5, 7, 9, 11, 12, 13, 15, 16, 17, 19, 20, 21, 23, 25, 27, 28,...
-- Porque: 1 acaba en 0 ceros, 3 acaba en 0 ceros, 4 acaba en 2 ceros, ...

-- Ejercicio 1.a. Define la función usando recursión y/o
-- comprensión, incluyendo las funciones auxiliares.

binarioPar :: [Integer]
binarioPar = aux 1
  where aux n | even (numceros (nat2bin n)) = n:aux (n+1)
              | otherwise = aux(n+1)

numceros :: [Integer] -> Integer
numceros (0:xs) = 1 + numceros xs
numceros _ = 0

nat2bin :: Integer -> [Integer]
nat2bin n | n < 2 = [n]
          | otherwise = (rem n 2):nat2bin (div n 2)

-- Ejercicio 1.b. Define la función solo usando funciones de
-- orden superior, incluyendo las funciones auxiliares.
-- Nota 1: puede ser de utilidad la funcion (iterate f n)
-- Nota 2: si usas las funciones auxiliares del apartado anterior, el
-- ejercicio se evalúa pero con la mitad de puntuación.

binarioParO :: [Integer]
binarioParO = filter (parCeros'.nat2bin') [1..]

parCeros' :: [Integer] -> Bool
parCeros' = even.length.takeWhile (==0) 

nat2bin' :: Integer -> [Integer]
nat2bin' = map (\x -> rem x 2) . takeWhile (/=0) . iterate (\x -> div x 2) 

-- también se puede hacer así:
nat2bin'' = map ((flip rem) 2) . takeWhile (/=0) . iterate ((flip div) 2) 
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. 
-- ---------------------------------------------------------------------
-- Un árbol binario está balanceado si para cada nodo, el número de 
-- elementos por el subárbol izquierdo y por el derecho es aproximadamente
-- igual. También diremos que está ordenado si para cada nodo, todos los
-- elementos por el subárbol izquierdo son menores que el valor del nodo,
-- y los del subárbol derecho son mayores. Una forma de ordenar y balancear
-- un árbol binario es:
--    1) poner los elementos del árbol en una lista y ordenarlos
--    2) crear un árbol a partir de la lista ordenada de elementos, donde
--       la raiz sea el elemento en la mitad de la lista, el subárbol
--       izquierdo sea el correspondiente a la primera mitad de la lista,
--       y el subárbol derecho sea el correspondiente a la segunda mitad 
--       (exceptuando el elemento en la mitad, ya que se ha usado para la
--        raiz).
-- Define la función (ordenaBalancea a), tal que dado un árbol binario 
-- con el tipo definido abajo, devuelva otro árbol del mismo tipo que 
-- esté ordenado y balanceado usando el procedimiento explicado. Dar a la
-- función un tipado lo más genérico posible. Por ejemplo,
-- > ordenaBalancea ejA1  
-- N 4 (N 3 (N 2 H H) H) (N 5 H H)
-- > ordenaBalancea ejA2
-- N 3 (N 1 (N 0 H H) (N 2 H H)) (N 8 (N 4 H H) H)

data Arbol a = H | N a (Arbol a) (Arbol a)
  deriving (Eq,Show)

ejA1,ejA2 :: Arbol Int
ejA1 = (N 5 (H) (N 4 (N 3 H H) (N 2 H H)))
ejA2 = (N 4 (N 8 (N 0 H (N 2 H H)) H) (N 1 (N 3 H H) H))

ordenaBalancea :: Ord a => Arbol a -> Arbol a 
ordenaBalancea a = creaArbol os 
    where xs = aplana a
          os = sort xs

creaArbol :: [a] -> Arbol a
creaArbol [] = H
creaArbol xs = N x (creaArbol xs1) (creaArbol xs2')
  where n = div (length xs) 2
        (xs1,xs2) = splitAt n xs -- igual que (take n xs,drop n xs)
        xs2' = tail xs2 
        x = head xs2 

aplana :: Arbol a -> [a]
aplana (H) = []
aplana (N x i d) = x: aplana i ++ aplana d
-- ---------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 3.
-- ------------------------------------------------------------------- 
-- Vamos a implementar un sistema de cintas de productos mediante una
-- lista de colas. Las cintas están conectadas entre sí, de tal manera
-- que los elementos de la segunda cinta pasan a la primera, los de la
-- tercera pasan a la segunda, etc. Si una cinta se queda vacía, se
-- elimina de la lista. Define la función
--     sacaCinta :: [Cola a] -> (Maybe a,[Cola a])
-- tal que (sacaCinta cs) devuelve el par (Just a,cs'), donde a es el 
-- primer elemento de la primera cinta, y cs' es la lista de cintas 
-- calculada a partir de cs, donde una cinta le ha pasado su primer
-- elemento a la siguiente, como se ha explicado arriba. Si la lista 
-- cs es vacía entonces el par devuelto es (Nothing,[]). Observa los 
-- siguientes ejemplos: 
-- > ejGC1   -- Este es un primer ejemplo con 2 cintas
-- [C [1,2,3,4,5],C [6,7,8,9,10]]
-- > sacaCinta ejGC1    
-- (Just 1,[C [2,3,4,5,6],C [7,8,9,10]])
-- > ejGC2   -- Este es un segundo ejemplo con 2 cintas
-- [C [1,2,3,4,5],C [6]]
-- > sacaCinta ejGC2
-- (Just 1,[C [2,3,4,5,6]])
-- > sacaCinta []
-- (Nothing,[])

ejGC1,ejGC2 :: [Cola Int] 
ejGC1 = [foldr inserta vacia [5,4..1],foldr inserta vacia [10,9..6]]
ejGC2 = [foldr inserta vacia [5,4..1],inserta 6 vacia]

sacaCinta :: [Cola a] -> (Maybe a,[Cola a])
sacaCinta [] = (Nothing,[])
sacaCinta cs = (Just (primero (head cs)),restoG cs)

restoG :: [Cola a] -> [Cola a]
restoG [] = []
restoG [c] | esVacia rc = []
           | otherwise = [rc]
  where rc = resto c
restoG (c1:c2:cs) = (inserta p (resto c1)):restoG (c2:cs)
  where p = primero c2
-- ------------------------------------------------------------------- 

-- -------------------------------------------------------------------
-- Ejercicio 4. 
-- ------------------------------------------------------------------- 
-- En este ejercicio vamos a trabajar con un Sudoku de 9x9 (solo de 
-- este tamaño, así que puedes usar el 9 sin problema en las
-- soluciones). 

-- Ejercicio 4.a. Define el tipo Sudoku, el cual es sinónimo
-- de una matriz de dos dimensiones cuyos valores son enteros.

type Sudoku = Array (Int,Int) Int

-- Ejercicio 4.b. Define un programa interactivo donde:
--    * Se le pida al usuario un nombre de fichero, y éste se
--      cargue si existe el fichero, si no devuelve un mensaje de error
--      "Fichero no existente" y acaba. El contenido del fichero se debe
--      procesar para crear una variable de tipo Sudoku (matriz de dos
--      dimensiones de enteros.)
--    * Se le pida al usuario una fila, una columna, e imprima
--      la lista de números que pueden introducirse en esa posición. Si la
--      posición corresponde a un 0, entonces sería una lista vacía.
-- Ejemplos:
-- *Main> main
-- Indica nombre de fichero
-- sudoku.txt
-- Indica fila
-- 9
-- Indica columna
-- 1
-- Los números posibles son: [1,5]
-- *Main> main
-- Indica nombre de fichero
-- sud.txt
-- Fichero no existente
-- *Main> main
-- Indica nombre de fichero
-- sudoku.txt
-- Indica fila
-- 5
-- Indica columna
-- 7
-- Los números posibles son: [1,3,6,9]
-- *Main> main
-- Indica nombre de fichero
-- sudoku.txt
-- Indica fila
-- 3
-- Indica columna
-- 3
-- Los números posibles son: []

cargaSudoku :: String -> Sudoku
cargaSudoku s = listArray ((1,1),(9,9)) ls
  where ls = map read (words s)

posibilidades :: Int -> Int -> Sudoku -> [Int] 
posibilidades f c a 
  | a!(f,c) /= 0 = []
  | otherwise = filter ((flip notElem) xs) [1..9]
    where xs = [a!(i,c) | i <- [1..9], a!(i,c) /= 0] ++
                [a!(f,j) | j <- [1..9], a!(f,j) /= 0] ++ 
                [a!(i,j) | i <- [3*fs+1..3*fs+3], j <- [3*cs+1..3*cs+3], a!(i,j)/=0]
          fs = div (f-1) 3
          cs = div (c-1) 3

main :: IO()
main = do
  putStrLn "Indica nombre de fichero"
  fileName <- getLine 
  exists <- doesFileExist fileName
  input <- if exists then readFile fileName else return ""
  if (null input) then
    putStrLn "Fichero no existente"
   else do
    let sudoku = cargaSudoku input
    putStrLn "Indica fila"
    fs <- getLine
    let f = read fs :: Int
    putStrLn "Indica columna"
    cs <- getLine
    let c = read cs :: Int
    let xs = posibilidades f c sudoku
    putStrLn $ "Los números posibles son:" ++ (show xs)
-- -------------------------------------------------------------------