-- Programación Declarativa 2019/20
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2                                       28 de Noviembre de 2019
-- ----------------------------------------------------------------------

import Data.Maybe
import ColaConListas

-- ----------------------------------------------------------------------
-- Ejercicio 1 (2,5 puntos)
-- La función (gmapf f g p xss) recibe una lista de listas xss, una 
-- función f, una función g y una propiedad p. La función gmapf se comporta
-- como sigue: para cada sublista xs de xss, si todos sus elementos cumplen
-- la propiedad p, entonces aplica g al resultado de aplicar la función f a
-- todos los elementos de xs. Por ejemplo,
-- > gmapf (*2) sum (>=0) [[-10,10],[0,1],[1,2,3]] == [2,12]
--   Aclaración: Solo las sublistas [0,1] y [1,2,3] cumplen que todos sus
--               elementos son (>=0). Después, aplica (*2) a cada uno,
--               obteniendo [0,2] y [2,4,6], y luego aplica sum a cada una.
-- > gmapf (`mod` 8) (maximum) (/=0) [[1..10],[0..10],[2,3,9]] == [7,3]
--   Aclaración: Solo las sublistas [1..10] y [2,3,9] cumplen que todos sus
--               elementos son (/=0). Después, aplica (`mod` 8) a cada uno,
--               obteniendo [1,2,3,4,5,6,7,0,1,2] y [2,3,1], y luego aplica
--               maximum a cada una, obteniendo [7,3].
--
-- Ejercicio 1.1. Definir la función gmapf usando solo comprensión (es
-- decir, nada de recursión ni funciones de orden superior), usando
-- un tipado polimórfico.
gmapfC = undefined

-- Ejercicio 1.2. Definir la función gmapf usando solo recursión (es decir,
-- nada de comprensión ni funciones de orden superior), usando un tipado
-- polimórfico.
gmapfR = undefined

-- Ejercicio 1.3. Definir la función gmapf usando solo orden superior(es
-- decir, nada de comprensión ni recursión), con un tipado polimórfico.
gmapfP = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2 (2,25 puntos). Algunos compiladores, como gcc, usan
-- el generador de números pseudo-aleatorios GLC (Generador Lineal
-- Congruencial). Este generador recibe cuatro parámetros: s (semilla),
-- a (multiplicador), c (incremento) y m (módulo). GLC devuelve una
-- sucesión de números de la siguiente forma:
--   * el primer término es:   X{0} = s
--   * el término n-ésimo es:  X{n} = (a*X{n-1}+c) módulo m 
--
-- Ejercicio 2.1. Definir la función (glc s a m c), tal que devuelva la
-- sucesión de números usando s como semilla, a el multiplicador, m
-- el módulo y c el incremento. Por ejemplo, 
-- > take 15 $ glc 0 1 10 3 == [0,3,6,9,2,5,8,1,4,7,0,3,6,9,2]
-- > let gcc = glc 5 1103515245 (2^31) 12354  
-- > take 5 $ gcc == [5,1222621283,1895947369,1361643255,681666925]

glc = undefined

-- Ejercicio 2.2. Activa el modo para medir el tiempo y memoria en ghci,
-- ":set +s", y evalua la siguiente línea. Copia como un comentario, justo
-- aquí debajo, el resultado y el tiempo obtenido.
-- Nota: Necesitarás tener una versión de la función glc para hacerlo en
-- un tiempo razonable.

evaluaEsto = takeWhile (/=4) $ tail $ glc 4 3 (2^25) 0
{- Copia y pega en la siguiente línea lo que obtengas en ghci 


-}
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 3 (2,25 puntos). Dada la definición siguiente de árbol
-- binario (donde las hojas no albergan valores):

data Arbol a = H | N (Arbol a) a (Arbol a)

-- definir la función (anchura a), que reciba un árbol a, y devuelva
-- un entero que indique la máxima anchura alcanzada en los niveles
-- del árbol. La anchura en un nivel n se calcula como el número de
-- nodos internos en ese nivel. Por ejemplo, para arboljEj1, la anchura
-- en el nivel 0 (su raíz) es 1, y su anchura a nivel 1 es 2, por lo
-- que su anchura en total es 2.
--
--              1      <-- nivel 0                         1
--  arbolEj1=  / \                           arbolEj2=    / \
--            2   3    <-- nivel 1                       2   3
--                                                      /   / \
--                                                     4   5   6
-- > anchura arbolEj1 == 2
-- > anchura arbolEj2 == 3

arbolEj1 = (N (N H 2 H) 1 (N H 3 H))
arbolEj2 = (N  (N (N H 4 H) 2 H)  1  (N (N H 5 H) 3 (N H 6 H)))

anchura = undefined
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 4 (3 puntos). En la línea de hipermercados Carrefive se va
-- a implantar un sistema de fila única: los clientes hacen solo una
-- fila para acceder a las cajas, las cuales tienen pequeñas filas de
-- hasta cinco clientes como máximo. Nos interesa simular el sistema para
-- comprobar su efectividad.
--
-- Ejercicio 4.1. Define el tipo de dato Caja, tal que se pueda usar para
-- indicar si la caja está Cerrada o Abierta. En caso de que esté abierta,
-- debe llevar asociada una cola de enteros (usando el TAD de Cola adjunto).
-- Debemos de ser capaces de imprimir un valor del tipo Caja. Definir
-- también el sinónimo Cajas, que sea una lista de valores de tipo Caja.


-- Ejercicio 4.2. Define la función (capacidades cs), donde cs sea un
-- elemento de tipo Cajas (recuerda, una lista de Caja), y devuelva otra
-- lista con elementos de tipo Maybe con enteros, de tal forma que:
--   * si la caja está cerrada, entonces Nothing
--   * si la caja está abierta, entonces Just l, donde l es la longitud de
--     la cola.
-- Por ejemplo,
-- > capacidades ej1 == [Nothing,Just 3,Nothing,Just 2]
--
-- Nota: Descomenta el ejemplo una vez hayas definido el tipo en el 4.1.
--ej1 = [Cerrada,
--       Abierta (foldr inserta vacia [4,3,1]),
--       Cerrada,
--       Abierta (foldr inserta vacia [5,2])]

capacidades = undefined

-- Ejercicio 4.3. Define la función (siguienteTurno cs), donde cs sea una
-- lista de tipo Maybe con enteros, y devuelva el índice (empezando por 0)
-- del elemento de la lista que no sea Nothing y cuyo valor sea el más
-- pequeño (queremos comprobar la caja con la fila más corta). Por ejemplo,
-- > siguienteTurno ejC1 == 3

ejC1 :: [Maybe Int]
ejC1 = [Nothing,
        Just 3,
        Nothing,
        Just 2]

siguienteTurno = undefined

-- Ejercicio 4.4. Define la función (nuevoCliente a cs), donde a sea un entero
-- representando un cliente, y cs sea una lista de Caja. Se deberá entonces
-- insertar el cliente en la caja con la fila más corta. Puedes usar las
-- funciones que has definido anteriormente. Por ejemplo,
-- > nuevoCliente 6 ej1 == [Cerrada,Abierta (C [1,3,4]),Cerrada,Abierta (C [2,5,6])]

nuevoCliente = undefined
-- ----------------------------------------------------------------------
