-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Febrero                                          11 de Febrero de 2021
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- Lugar ocupado:         laboratorio:          puesto:
-- ----------------------------------------------------------------------
-- INSTRUCCIONES PARA LA ENTREGA
-- 1. CAMBIA EL NOMBRE de este archivo por:       <uvus>.hs
--    donde "<uvus>" es tu UVUS.
-- 2. COMENTA LAS LÍNEAS CON ERRORES hasta que se pueda cargar el fichero
--    sin problemas. ESCRIBE tu nombre y apellidos en la cabecera.
-- 3. COMPRIME este archivo en un único fichero llamado EXACTAMENTE:
--      ENTREGA-<uvus>.tar.gz      (o bien)       ENTREGA-<uvus>.tar.xz
--    donde "<uvus>" es tu UVUS. No te olvides del guión después de
--    ENTREGA, y NO lo comprimas en un fichero .zip.
-- 4. REINICIA el equipo. En el menú de selección del sistema (con fondo
--    blanco), HAZ CLICK SOBRE "Enviar examen" al lado de sistema Ubuntu.
-- 5. Pregunta al profesor si ha llegado tu correo correctamente, si es
--    así, ya puedes dejar tu puesto SIN APAGAR EL EQUIPO.
-- ----------------------------------------------------------------------
-- ORIENTACIONES
-- · Escribe la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Se valorará el uso correcto de tipados para cada función definida.
-- · Puedes añadir tantas funciones auxiliares (incluyendo el tipo
--   adecuadamente) como necesites.
-- ----------------------------------------------------------------------

import Data.Array
import Data.Char
import Data.List
import Control.Exception (catch, SomeException)
import PilaConListas
--import I1M.Pila

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
-- Ejercicio 1. (0,7 puntos) 
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
-- Ejercicio 2. (0,3 puntos)
-- ----------------------------------------------------------------------
-- Define el tipo sinónimo TablaFrecuencias tal que sea un vector (es decir, 
-- un Array de 1 dimensión) cuyos índices sean enteros y los valores sean
-- números reales.

--TablaFrecuencias = ....

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 3. (1,5 puntos)
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
-- Ejercicio 4. (2 puntos)
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
-- Ejercicio 5. (2 puntos) 
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
-- Ejercicio 6. (1,5 puntos) 
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
-- Ejercicio 7. (2 puntos)
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

-- ----------------------------------------------------------------------




