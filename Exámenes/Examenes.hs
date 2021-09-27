{-# LANGUAGE OverloadedStrings #-}

import Data.Matrix
import Data.Array
import Data.Char
import Data.List
import Control.Exception (catch, SomeException)
import PilaConListas
--import I1M.Pila
import Test.QuickCheck
import Control.Monad
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import Control.Parallel.Strategies (Eval,runEval,rpar)
import Control.DeepSeq

-- ----------------------------------------------------------------------
-- Ejercicio 1. 
-- ----------------------------------------------------------------------
-- Una forma de transmitir valores numéricos en binario de manera compacta
-- es la codificación unaria. Una variante es la siguiente:
--  * El 0 se codifica con un 0.
--  * Un valor n se codifica con n+2 bits: n veces el 1, un 0, y seguido 
--    de un bit de signo siendo 0 si n es positivo o 1 si n es negativo.
--    De esta forma, el 2 se codifica como 1100, y el -2 como 1101.
-- Definir la función decodifica, tal que reciba una lista de ceros y unos
-- que representan una coficiación unaria y devuelva la lista de valores
-- numéricos correspondientes. Se piden dos definiciones, una con orden 
-- superior y otra con solo recursividad. Por ejemplo:
--   λ> decodificaO [1,1,0,1,1,1,0,0] == [-2,2]
--   λ> decodificaO [0,0,1,1,1,0,1,1,1,1,0,0,1,0,1] == [0,0,-3,3,-1]

-- Ejercicio 1.1. Definir con orden superior

decodificaO :: [Int] -> [Int]
decodificaO = undefined

-- Ejercicio 1.2. Definir con solo recursión

decodificaR :: [Int] -> [Int]
decodificaR = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2.
-- ----------------------------------------------------------------------
-- Ejercicio 2.1. Implementar el TAD Pila usando el diccionario 
-- Data.Map. Se debe indicar en un breve comentario cual es el diseño 
-- seguido (es decir, qué se almacena como clave y qué como valor). No
-- se permite la traducción a listas, por lo que se debe hacer uso 
-- exclusivo de las funciones que provee Data.Map. Escribir el código 
-- necesario para exportar las funciones como un módulo en el lugar indicado
-- en este fichero .hs, y hazlo dentro de un comentario.
-- Nota: debes importar el módulo Data.Map de forma cualificada

-- Descomentar estos ejemplos después de definir el tipo y las funciones
--ejPila1 :: Pila Int
--ejPila1 = foldr apila vacia [1..20]
--ejPila2 :: Pila Char
--ejPila2 = foldr apila vacia ['a'..'z']

{- Diseño de la solución: 
 Como clave: 
 Como valor: 
-}

-- Definir el tipo aquí debajo
-- .... Pila a = ....

-- Definir la función vacia, que devuelva una pila vacía
vacia = undefined

-- Definir la función esVacia, que indique si la pila está vacía
esVacia = undefined

-- Definir apila, que añada un elemento a la cima de la pila
apila = undefined

-- Definir la función cima, que devuelva el primer elemento de la pila
cima = undefined

-- Definir la función desapila, que devuelva la pila quitando la cima
desapila = undefined

-- Ejercicio 2.2 (0,5 puntos). En un Data.Maybe, devolver simplemente 
-- el máximo valor de la pila, o nada si la pila está vacía. 
-- Nota: Si no has podido hacer el ejercicio 2.1, puedes importar el 
-- fichero TADPila.hs, adjunto al examen. Por ejemplo,
--   λ> maximo vacia == Nothing
--   λ> maximo ejPila1 == Just 20
--   λ> maximo ejPila2 == Just 'z'

maximo = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 3. 
-- ----------------------------------------------------------------------
-- Un árbol binario se puede definir con el siguiente tipo de dato algebráico:

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
   deriving (Show, Eq)

-- Vamos a considerar que un árbol está ordenado si para cada valor v de un 
-- nodo, los valores que hay en el subárbol izquierdo son menores
-- que v, y los valores que hay en el subárbol derecho son mayores que v.
-- También diremos que el árbol es balanceado si para cada nodo interno 
-- el número de nodos en el subárbol izquierdo es igual al número de nodos
-- en el subárbol derecho. Por ejemplo:
--
--       ejArbol1      ejArbol3           ejArbol2          ejArbol4
--
--           3             3                 8                  5
--          / \           / \              /   \              /   \
--         4   2         2   4            5     9            2     8
--                                       / \   / \          / \   / \
--                                      1   3 2   6        1   3 6   9
--
--   ejArbol1 y ejArbol2 no están ordenados. ejArbol3 y ejArbol4 son la
--   versión ordenada de ejArbol1 y ejArbol2, respectivamente.

-- Ejercicio 3.1. Comprueba con QuickCheck que el número de 
-- nodos de un árbol binario es siempre impar. Se valorará con 0,5 si se
-- define la función con composición de funciones, y 0,25 en otro caso.

prop_arboles_binarios :: Arbol a -> Bool
prop_arboles_binarios = undefined
  
-- Ejercicio 3.2. Define la función (ordena a), tal que reciba
-- un árbol binario balanceado, a, y devuelva un árbol binario balanceado y
-- ordenado con los mismos valores de a.
-- Nota 1: puedes usar listas o cualquier módulo que te sea útil
-- Nota 2: por sencillez, asume que el árbol de entrada es balanceado
-- Por ejemplo:
--   λ> ordena ejArbol1 == N 3 (H 2) (H 4)
--   λ> ordena ejArbol2 == N 5 (N 2 (H 1) (H 3)) (N 8 (H 6) (H 9))

ejArbol1, ejArbol2, ejArbol4 :: Arbol Int
ejArbol1 = N 3 (H 4) (H 2)
ejArbol2 = N 8 (N 5 (H 1) (H 3)) (N 9 (H 2) (H 6))
ejArbol4 = N 7 (N 8 (H 2) (H 0)) (H 5)

ordena = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 4.
-- ----------------------------------------------------------------------
-- Vamos a definir un laberinto en una matriz de caracteres usando para
-- ello:
--   * L para una casilla libre
--   * P para una casilla con una pared
--   * E para la casilla de entrada al laberinto
--   * S para la casilla de salida del laberinto
-- Dado un laberinto como la definición anterior, hacer una aplicación en
-- CodeWorld donde se dibuje el borde del laberinto en negro, las paredes
-- también en negro, las casillas libres en blanco, la casilla de salida
-- en azul, la casilla de entrada en verde, y la casilla donde está el 
-- jugador con un circulo amarillo. El jugador siempre comienza en la
-- casilla de entrada y no puede atravesar paredes ni salir del laberinto.
-- Se debe permitir moverse con las teclas de dirección. Se adjuntan un par
-- de capturas de pantalla sobre ejemplo de cómo debería verse.
-- Nota importante: Hay que tener en cuenta que se debe dibujar el tablero
-- con la misma orientación que la matriz, por lo que debes fijarte en que
-- el eje X corresponde con las columnas de la matriz, y el eje Y corresponde
-- de manera inversa con las filas de la matriz.

laberinto :: Matrix Char
laberinto = fromLists ["LLLLLPLLLS",
                       "PLPPPLLPPP",
                       "PLLLLLLPPP",
                       "PLPPLPPPLP",
                       "ELLLLLLLLP"]
            
-- define el tipo que describa el estado
--  .... Estado = ...

-- define la función para pintar el estado
--pintaEstado :: Estado -> Picture
pintaEstado = undefined

-- define la función manejaEvento
--manejaEvento :: Event -> Estado -> Estado
manejaEvento = undefined

-- define el estado inicial
--inicio :: Matrix Char -> Estado
inicio = undefined

-- define la función principal main
main :: IO()
main = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 5. 
-- ----------------------------------------------------------------------
-- Ejercicio 5.1. Redefine la función main para que haya un
-- menú de texto previo a lanzar el entorno codeworld. Este menú debe:
--  1) preguntar primero por un nombre de fichero y leerlo por teclado,
--  2) procesar el fichero con control de excepciones. Si el fichero
--     no existe, hay que volver a preguntar por otro fichero.
--  3) cargar todos los laberintos del fichero como matrices de caracteres,
--  4) mostrar cuántos laberintos se han leído,
--  5) elegir el primer tablero, mostrarlo por pantalla y lanzar codeworld.
-- Los ficheros de entrada tienen una línea por laberinto, de la forma
-- "F C CADENA", siendo F el número de filas, C número de columnas y CADENA
-- un string con las casillas del laberinto. Por ejemplo,
-- 5 10 LLLLLPLLLSPLPPPLLPPPPLLLLLLPPPPLPPLPPPLPELLLLLLLLP
-- corresponde al laberinto de ejemplo del ejercicio 4. Un ejemplo de uso:
--   λ> main
--   Indica el nombre de fichero: maze.txt
--   maze.txt: openFile: does not exist (No such file or directory)
--   Indica el nombre de fichero: laberinto.txt
--   He leído 1 laberintos, lanzando: 
--   ┌                     ┐
--   │ 'S' 'L' 'L' 'L' 'P' │
--   │ 'P' 'L' 'L' 'L' 'P' │
--   │ 'P' 'L' 'L' 'L' 'P' │
--   │ 'P' 'L' 'L' 'L' 'L' │
--   │ 'P' 'P' 'P' 'L' 'E' │
--   └                     ┘
--   Open me on http://127.0.0.1:3000/

-- Nota: si no has podido hacer el ejercicio 4, el menú debe finalizar tras
-- mostrar el laberinto.

--main :: IO ()
--main = menu

menu :: IO()
menu = undefined

-- Ejercicio 5.2. La función soluciones recibe una matriz
-- y devuelve el número de posibles soluciones en el laberinto. Define una
-- función main que cargue todos los laberintos en el fichero cuyo nombre
-- se pase como primer argumento, y compruebe el mínimo de todos ellos.
-- Esta comprobación debe hacerse haciendo uso del parallel map visto en
-- clase (abajo copiado). Compila el programa, ejecútalo con el fichero
-- laberintos.txt y calcula la aceleración conseguida al lanzarlo con 4
-- procesadores contra 1 procesador.

{-
Comando de compilación =
Comando de ejecución 1 procesador =
Comando de ejecución 4 procesadores =
Speedup = 
-}

--main :: IO ()
--main = undefined

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do b <- rpar (f a)
                     bs <- parMap f as
                     return (b:bs)
                     
soluciones :: Matrix Char -> Int
soluciones m = length $ concat [ caminosRAux [[(i,j)]] | i <- [1..nrows m], j <- [1..ncols m], m!(i,j) == 'E' ]
  where
    caminosRAux [] = []
    caminosRAux (cs:css)
      | fuera || m!p == 'P' || elem p (tail cs) = caminosRAux css
      | m!p == 'S' = cs : caminosRAux css
      | otherwise = caminosRAux iz ++ caminosRAux dc ++ caminosRAux ar ++ caminosRAux ab ++ caminosRAux css
        where p@(x,y) = head cs
              iz = [(x-1,y):cs]
              dc = [(x+1,y):cs]
              ar = [(x,y+1):cs]
              ab = [(x,y-1):cs]
              fuera = x < 1 || x > nrows m || y < 1 || y > ncols m         

-- ---------------------------------------------------------------------
-- Nota. Para comprobar propiedades de árboles con QuickCheck se
-- utilizará el siguiente generador. Se aconseja no borrarlo.
-- ---------------------------------------------------------------------

instance Arbitrary a => Arbitrary (Arbol a) where
  arbitrary = sized arbol
    where
      arbol 0       = liftM H arbitrary 
      arbol n | n>0 = oneof [liftM H arbitrary, 
                             liftM3 N arbitrary subarbol subarbol]
                      where subarbol = arbol (div n 2)
      arbol _       = error "Imposible"

-- ----------------------------------------------------------------------
-- En esta prueba vamos a implementar una codificación Huffman sencilla.
-- Veamos primero de qué se trata. Por ejemplo, la cadena "ABRRKBAARAA"  
-- se podría codificar con 11 bytes si usamos un byte por caracter. Sin 
-- embargo, podemos asignar un código de bits distinto a cada caracter 
-- según su frecuencia de aparición en la cadena, y así conseguir 
-- codificarlo en menos bits. El procedimiento es como sigue.

-- Primero calculamos la frecuencia de cada caracter en la cadena de
-- entrada, es decir, el número de apariciones dividido por el total:
-- A  5/11 = 0,4547
-- R  3/11 = 0,2728
-- B  2/11 = 0,1819
-- K  1/11 = 0,0909

-- Ahora vamos a crear un árbol donde vamos a ordenar los caracteres.
-- El árbol Huffman tiene la siguiente forma. Las hojas almacenan los 
-- caracteres, y aparecen a menor profundidad si tienen mayor frecuencia.
-- Observa que los nodos no tienen asociados valores, y para cada nodo, 
-- la rama izquierda siempre tendrá asociado el 0, y la rama derecha tendrá
-- asociado el 1.
--       o
--      / \  
--   0 /   \ 1
--    /     \
--   A       o
--          / \
--       0 /   \ 1
--        /     \
--       R       o
--              / \
--           0 /   \ 1
--            /     \
--           B       K 

-- Finalmente, la codificación de cada caracter es simplemente el camino 
-- desde la raíz a la hoja de ese caracter:
-- A    0
-- R   10
-- B  110
-- K  111

-- Por tanto, la codificación de la cadena original "ABRRKBAARAA" es:
-- 0 110 10 10 111 110 0 0 10 0 0             (ahora cabe en 3 bytes)
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 1.  
-- ----------------------------------------------------------------------
-- Define la función (limpia cs), tal que reciba una cadena de caracteres 
-- y devuelva otra donde solo queden letras del abecedario en inglés
-- (sin ñ, ç, tildes, ...) convertidas a minúsculas.
-- NOTA: Si lo haces con solo plegado (foldr o foldl) tendrás 0,7 puntos, 
-- si no, la mitad de puntos.
-- Por ejemplo,
-- > limpia "aaA A!"
-- "aaaa"
-- > limpia "¿Estudias?"
-- "estudias"

limpia :: String -> String
limpia = undefined

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2. 
-- ----------------------------------------------------------------------
-- Define el tipo sinónimo TablaFrecuencias tal que sea un vector (es decir, 
-- un Array de 1 dimensión) cuyos índices sean enteros y los valores sean
-- números reales.

--TablaFrecuencias = ....

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 3. 
-- ----------------------------------------------------------------------
-- Define la función (frecuencias cs) tal que reciba un String y devuelva 
-- un valor del tipo TablaFrecuencias. La cadena cs debe ser procesada
-- por la función 'limpia' del ejercicio 1. La tabla de frecuencias debe 
-- tener una posición para cada letra del abecedario en inglés (26 en toal),
-- siendo el primer elemento la frecuencia de la 'a', el segundo el de la 'b'
-- ... el último el de la 'z'. Si una letra no aparece en cs, su frecuencia
-- es 0. 
-- NOTA: si no has hecho el primer ejercicio, asume que cs contiene solo letras
-- en minúsculas.
-- Por ejemplo,
-- > frecuencias "ABRRKBAARAA"
-- array (0,25) [(0,0.45454547),(1,0.18181819),(2,0.0),(3,0.0),(4,0.0),(5,0.0),
-- (6,0.0),(7,0.0),(8,0.0),(9,0.0),(10,9.090909e-2),(11,0.0),(12,0.0),(13,0.0),
-- (14,0.0),(15,0.0),(16,0.0),(17,0.27272728),(18,0.0),(19,0.0),(20,0.0),(21,0.0),
-- (22,0.0),(23,0.0),(24,0.0),(25,0.0)]
-- > head (elems (frecuencias "AaAa Aa"))
-- 1.0

frecuencias = undefined

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 4.
-- ----------------------------------------------------------------------
-- Define la función (pilaFrecuencias tf) tal que reciba una tabla de 
-- frecuencias y devuelva una pila cuyos elementos sean pares (c,f), 
-- donde c es un caracter y f es su frecuencia. Los elementos en la 
-- pila deben estar ordenados de mayor a menor por frecuencia (es decir,
-- la cima de la pila es el caracter con mayor frecuencia), y solo 
-- deben aparecer aquellos cuya frecuencia no sea 0.
-- NOTA: Si no has podido hacer el ejercicio 3, puedes usar la variable
-- 'tf' del ejemplo siguiente.
-- Por ejemplo,
-- > let tf = array (0,3) [(0,0.16666667),(1,0.33333334),(2,0.5),(3,0.0)]
-- > pilaFrecuencias tf
-- ('c',0.5)|('b',0.33333334)|('a',0.16666667)|-
-- > pilaFrecuencias $ frecuencias "ABRRKBAARAA"
-- ('a',0.45454547)|('r',0.27272728)|('b',0.18181819)|('k',9.090909e-2)|-

pilaFrecuencias = undefined

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 5. 
-- ----------------------------------------------------------------------
-- Vamos a usar el siguiente tipo de dato algebraico para representar un
-- árbol de Huffman. Observa que los 0s y 1s no se representan, tan solo 
-- los caracteres. 

data ArbolH = H Char | HN | N (ArbolH) (ArbolH)
  deriving Show

-- Define la función (arbolHuffman cs) tal que reciba un String y devuelva
-- un árbol de Huffman. Observa el ejemplo del comienzo del enunciado. Los 
-- caracteres se ponen en el subárbol izquierdo de cada nodo como hojas H, 
-- y en orden de mayor a menor frecuencia. En concreto, el caracter con más
-- frecuencia se pone como subárbol izquierdo de la raíz, y el caracter con
-- menor frecuencia terminará en el subárbol derecho del nodo más profundo.
-- En el caso de que solo haya un caracter único en el String, se pondrá ese
-- caracter a la izquierda de la raíz, y a la derecha se pondrá la hoja HN. 
-- NOTA 1: Se permite traducir la pila de pilaFrecuencias a una lista, antes
-- de solucionar el ejercicio, pero contará sobre 1,5 puntos.
-- NOTA 2: Si no has podido definir las funciones anteriores, básate en la 
-- siguiente definición aunque los resultados salgan distintos:
--pilaFrecuencia cs = foldr apila vacia (nub (limpia cs))
-- Por ejemplo,
-- > arbolHuffman "ABRRKBAARAA"
-- N (H 'a') (N (H 'r') (N (H 'b') (H 'k')))
-- > arbolHuffman "abbccc"
-- N (H 'c') (N (H 'b') (H 'a'))
-- > arbolHuffman "aAaA aA" 
-- N (H 'a') HN

arbolHuffman :: String -> ArbolH
arbolHuffman = undefined

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 6.  
-- ----------------------------------------------------------------------
-- Define la función (codificaHuffman cs), tal que reciba una cadena de 
-- caracteres y devuelva una lista de enteros. La lista de enteros contendrá 
-- la secuencia de bits de la codificación final (0s y 1s). Para ello, tienes 
-- que:
--    1. limpiar la cadena cs para que solo contenga letras minúsculas,
--    2. construir el árbol de Huffman con la función anterior,
--    3. codificar cada caracter con la secuencia de 1s y 0s correspondiente.
-- La codificación de un caracter es el camino de la raíz al caracter, donde
-- siempre que entres por un subárbol derecho equivale a un 1, y por un 
-- subárbol izquierdo a un 0.
-- NOTA: si no has conseguido la función 'arbolHuffman', puedes usar
-- directamente el siguiente árbol predefinido, aunque los resultados en los
-- ejemplos sean distintos):
-- arbolHuffman' = N (H 'a') (N (H 'r') (N (H 'b') (N (H 'k') (H 'c'))))
-- Por ejemplo,
-- > codificaHuffman "ABRRKBAARAA"
-- [0,1,1,0,1,0,1,0,1,1,1,1,1,0,0,0,1,0,0,0]
-- > codificaHuffman "abbccc"     
-- [1,1,1,0,1,0,0,0,0]
-- > codificaHuffman "aA aA!"
-- [0,0,0,0]

codificaHuffman :: String -> [Int]
codificaHuffman = undefined

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 7.
-- ----------------------------------------------------------------------
-- Define un programa tal que:
-- 1) pida al usuario que introduzca un nombre
-- de fichero, se cargue el fichero y se limpie dejando solo letras
-- minúsculas. Si el fichero no existe o está vacío, debe mostrar un 
-- mensaje de error y volver a pedir un fichero. Importa los módulos que
-- te hagan falta.
-- 2) muestre por pantalla cuantos bytes harían falta para codificar
-- el fichero usando un byte por letra.
-- 3) muestre por pantalla cuantos bytes (no bits!) harían falta para
-- codificar el fichero usando codificación Huffman.
-- 4) escribe en el fichero output.txt la secuencia de la codificación
-- Huffman. Por simplicidad, escríbelo como texto.
-- NOTA: si no has hecho el ejercicio anterior, puedes usar la siguiente
-- definición, aunque los resultados en los ejemplos sean distintos:
--codificaHuffman' cs = (bin.ord.head) cs 
--    where bin n | n < 2 = [n]
--                | otherwise = (rem n 2):bin (div n 2)
-- Por ejemplo,
-- > main
-- Nombre de fichero: in.txt
-- in.txt: openFile: does not exist (No such file or directory)
-- Nombre de fichero erróneo. Prueba de nuevo.
-- Nombre de fichero: input.txt
-- Numero de bytes sin comprimir: 11 bytes
-- Numero de bytes con compresión: 3 bytes
-- > readFile "output.txt"
-- "01101010111110001000"

main :: IO ()
main = undefined

-- -------------------------------------------------------------------
-- Ejercicio 2.
-- -------------------------------------------------------------------
-- (2.1) Definir los tipos Jugador (con dos posibles valores para
-- poder distinguir quien tiene el turno en cada momento), Mesa (como
-- sinónimo de una lista de enteros, para guardar la cantidad de
-- estrellas en cada fila de la mesa) y Fila (con dos posibles
-- valores, para indicar que no se ha escogido ninguna fila aún o el
-- índice, un entero, de la fila que se ha escogido). Utilizar data
-- para la definición de los tipos solicitados (salvo Mesa).
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.2) Definir el tipo NimFilas, utilizando data, para la
-- información sobre el jugador que tiene el turno, la cantidad de
-- estrellas en cada fila y la fila escogida. Dicho tipo debe
-- pertenecer a la clase Show, por lo que hay que incluir una
-- definición de la función show.
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- Definición alternativa del tipo NimFilas si no se han resuelto los
-- ejercicios anteriores. 

-- type NimFilas = (Int, [Int], Int)

-- El primer elemento de la tupla será el jugador: 1 ó 2, el segundo
-- una lista con la cantidad de estrellas en cada fila de la mesa; y
-- el tercero: 0 si no se ha escogido fila o el índice de la fila
-- escogida.
-- -------------------------------------------------------------------


-- -------------------------------------------------------------------
-- (2.3) Definir una función,

-- inicioFilas :: Int -> NimFilas

-- tal que, dado un entero n, devuelva la información sobre el inicio
-- del juego: tiene el turno el primer jugador, sobre la mesa hay n
-- filas de estrellas completas y no se ha escogido ninguna fila.

-- Main> let mesaInicial = inicioFilas 5
-- Main> :t mesaInicial
-- mesaInicial :: NimFilas
-- Main> mesaInicial
-- 1: *
-- 2: **
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el primer jugador
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.4) Definir una función,

-- estrellasRestantesFilas :: NimFilas -> [Int]

-- tal que, dada la información sobre un instante de la partida,
-- devuelve una lista con la cantidad de estrellas que hay, sobre la
-- mesa, en cada una de las filas. 

-- Main> estrellasRestantesFilas mesaInicial
-- [1,2,3,4,5]
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.5) Definir una función,

-- jugadorFilas :: NimFilas -> Int

-- tal que, dada la información sobre un instante de la partida,
-- devuelve 1 si tiene el turno el primer jugador y 2 en caso
-- contrario.

-- Main> jugadorFilas mesaInicial
-- 1
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.6) Definir una función,

-- fila :: NimFilas -> Maybe Int

-- tal que, dada la información sobre un instante de la partida,
-- devuelve el índice de la fila escogida.

-- Main> fila mesaInicial
-- Nothing
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.7) Definir una función, utilizando guardas,

-- escogeFila :: NimFilas -> Int -> NimFilas

-- tal que, dada la información sobre un instante de la partida en el
-- que aún no se ha elegido ninguna fila y un entero i, devuelve la
-- información con la que continuaría la partida si el jugador que
-- tiene el turno escoge la fila i-ésima para eliminar estrellas de la
-- misma. Si ya hay una fila escogida, o no existe dicha fila, o en
-- dicha fila no quedan estrellas, la función debe devolver un error
-- (con un mensaje distinto según el motivo).

-- Main> escogeFila mesaInicial 8
-- Exception: escogeFila: índice incorrecto
-- Main> escogeFila mesaInicial 3
-- 1: *
-- 2: **
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el primer jugador
-- (que va a eliminar de la fila 3)
-- Main> escogeFila (escogeFila mesaInicial 3) 2
-- Exception: escogeFila: ya se ha escogido una fila
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.8) Definir una función,

-- eliminaEstrellasFilas :: NimFilas -> Int -> NimFilas

-- dada la información sobre un instante de la partida en el que ya
-- se ha escogido una fila y la cantidad de estrellas que se quieren
-- quitar de dicha fila, devuelve la información con la que se
-- continuaría la partida. 

-- Main> eliminaEstrellasFilas mesaInicial 2
-- Exception: eliminaEstrellasFilas: primero hay que elegir la fila
-- Main> eliminaEstrellasFilas (escogeFila mesaInicial 2) 8
-- Exception: eliminaEstrellasFilas: sólo se pueden coger 1, 2 ó 3
--            estrellas
-- Main> eliminaEstrellasFilas (escogeFila mesaInicial 2) 3
-- Exception: eliminaEstrellasFilas: en la fila escogida no hay
--            suficientes estrellas 
-- Main> eliminaEstrellasFilas (escogeFila mesaInicial 2) 1
-- 1: *
-- 2: *
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el segundo jugador
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.9) Añadir al inicio del fichero la definición del módulo
-- compartiendo el tipo NimFilas y las funciones solicitadas en los
-- ejercicios anteriores.
-- -------------------------------------------------------------------
