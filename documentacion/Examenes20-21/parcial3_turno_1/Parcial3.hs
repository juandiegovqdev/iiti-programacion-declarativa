-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 3 (turno 1)                                21 de Enero de 2021
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:                Puesto ocupado:
-- ----------------------------------------------------------------------

import Data.Array
import Data.List
--import I1M.Cola
import ColaConListas
import System.Directory

-- ---------------------------------------------------------------------
-- Ejercicio 1. (2,5 puntos)
-- ---------------------------------------------------------------------
-- La siguiente sucesión de números naturales
--    binarioPar :: [Integer]
-- es tal que el número n pertenece a la sucesión si su representación en 
-- binario acaba en un número par de ceros. Los primeros términos son:
-- 1, 3, 4, 5, 7, 9, 11, 12, 13, 15, 16, 17, 19, 20, 21, 23, 25, 27, 28,...
-- Porque: 1 acaba en 0 ceros, 3 acaba en 0 ceros, 4 acaba en 2 ceros, ...

-- Ejercicio 1.a (1 punto) Define la función usando recursión y/o
-- comprensión, incluyendo las funciones auxiliares.

binarioPar :: [Integer]
binarioPar = undefined

numceros :: [Integer] -> Integer
numceros (0:xs) = 1 + numceros xs
numceros _ = 0

nat2bin :: Integer -> [Integer]
nat2bin n | n < 2 = [n]
          | otherwise = (rem n 2):nat2bin (div n 2)

-- Ejercicio 1.b (1,5 puntos) Define la función solo usando funciones de
-- orden superior, incluyendo las funciones auxiliares.
-- Nota 1: puede ser de utilidad la funcion (iterate f n)
-- Nota 2: si usas las funciones auxiliares del apartado anterior, el
-- ejercicio se evalúa pero con la mitad de puntuación (0,75 puntos).

binarioParO :: [Integer]
binarioParO = undefined

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. (2,5 puntos)
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

ordenaBalancea = undefined 

-- ---------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 3. (2 puntos)
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
-- siguientes ejemplos, verás que la primera cinta también ha perdido
-- su primer elemento puesto que se ha devuelto en el primero del par:
-- > ejGC1   -- Este es un primer ejemplo con 2 cintas
-- [C [1,2,3,4,5],C [6,7,8,9,10]]
-- > sacaCinta ejGC1     -- el 1 pasa al primero del par 
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
sacaCinta = undefined 

-- ------------------------------------------------------------------- 

-- -------------------------------------------------------------------
-- Ejercicio 4. (3 puntos)
-- ------------------------------------------------------------------- 
-- En este ejercicio vamos a trabajar con un Sudoku de 9x9 (solo de 
-- este tamaño, así que puedes usar el 9 sin problema en las
-- soluciones). 

-- Ejercicio 4.a (0,5 puntos) Define el tipo Sudoku, el cual es sinónimo
-- de una matriz de dos dimensiones cuyos valores son enteros.

-- Sudoku = ...

-- Ejercicio 4.b (2,5 puntos) Define un programa interactivo donde:
--    * (1 punto) Se le pida al usuario un nombre de fichero, y éste se
--      cargue si existe el fichero, si no devuelve un mensaje de error
--      "Fichero no existente" y acaba. El contenido del fichero se debe
--      procesar para crear una variable de tipo Sudoku (matriz de dos
--      dimensiones de enteros.)
--    * (1,5 puntos) Se le pida al usuario una fila, una columna, e imprima
--      la lista de números que pueden introducirse en esa posición. Si el
--      valor en la posición no es 0, entonces imprime una lista vacía.
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

main :: IO()
main = undefined

-- -------------------------------------------------------------------
