-- Programación Declarativa 2019/20
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 1a Convocatoria                                   3 de Febrero de 2020
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- ----------------------------------------------------------------------
-- INSTRUCCIONES PARA LA ENTREGA
-- 1. CAMBIA EL NOMBRE de este archivo por:   Febrero_<codigo>_<uvus>.hs
--    donde "<uvus>" es tu UVUS y "<codigo>" es el código alfanumérico
--    en tu hoja del enunciado.
-- 2. COMENTA LAS LÍNEAS CON ERRORES hasta que se pueda cargar el fichero
--    sin problemas. ESCRIBE tu nombre y apellidos en la cabecera.
-- 3. COMPRIME este archivo en un único fichero llamado EXACTAMENTE:
--      ENTREGA-<uvus>.tar.gz      (o bien)       ENTREGA-<uvus>.tar.xz
--    donde "<uvus>" es tu UVUS. No te olvides del guión después de
--    ENTREGA, y no lo comprimas en un fichero .zip.
-- 4. REINICIA el equipo. En el menú de selección del sistema (con fondo
--    blanco), HAZ CLICK SOBRE "Enviar examen" al lado de sistema Ubuntu.
-- 5. ESCRIBE tus apellidos, nombre y UVUS en la hoja del enunciado, y
--    entrégala al profesor.
-- 6. Después de comprobar que se ha entregado, VUELVE A TU EQUIPO y
--    APÁGALO.
-- ----------------------------------------------------------------------
-- ORIENTACIONES
-- · Escribe la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrate de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada.
-- · Puedes añadir tantas funciones auxiliares (incluyendo el tipo
--   adecuadamente) como necesites.
-- ----------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import Data.Matrix
import CodeWorld
import Test.QuickCheck
import Control.Monad
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import Control.Parallel.Strategies (Eval,runEval,rpar)
import Control.DeepSeq

-- ----------------------------------------------------------------------
-- Ejercicio 1. (2 puntos)
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

-- Ejercicio 1.1 (1 punto). Definir con orden superior

decodificaO :: [Int] -> [Int]
decodificaO = undefined

-- Ejercicio 1.2 (1 punto). Definir con solo recursión

decodificaR :: [Int] -> [Int]
decodificaR = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2. (2 puntos)
-- ----------------------------------------------------------------------
-- Ejercicio 2.1 (1,5 puntos) Implementar el TAD Pila usando el diccionario 
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
-- Ejercicio 3. (2 puntos)
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

-- Ejercicio 3.1 (0,5 puntos). Comprueba con QuickCheck que el número de 
-- nodos de un árbol binario es siempre impar. Se valorará con 0,5 si se
-- define la función con composición de funciones, y 0,25 en otro caso.

prop_arboles_binarios :: Arbol a -> Bool
prop_arboles_binarios = undefined
  
-- Ejercicio 3.2 (1,5 puntos). Define la función (ordena a), tal que reciba
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
-- Ejercicio 4. (2 puntos)
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
-- Ejercicio 5. (2 puntos)
-- ----------------------------------------------------------------------
-- Ejercicio 5.1 (1,5 puntos). Redefine la función main para que haya un
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

-- Ejercicio 5.2 (0,5 puntos). La función soluciones recibe una matriz
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
