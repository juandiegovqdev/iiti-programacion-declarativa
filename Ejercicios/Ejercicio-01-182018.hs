-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 3 Parcial                                          18 de Enero 2018
-- -------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}
import Data.List 
import Test.QuickCheck

-- -------------------------------------------------------------------
-- Ejercicio 1 (1.5 ptos)
-- Los siguientes apartados están encaminados a representar y manejar
-- los números binarios con cuatro bits.

-- La siguiente tabla contiene dichos números y su correspondiente
-- representación binaria con cuatro bits (consideraremos que el bit
-- más a la izquierda es el que ocupa la primera posición).

-- 0000 1000 0100 1100 0010 1010 0110 1110 0001 1001 0101 1101 
--    0    1    2    3    4    5    6    7    8    9   10   11

-- 0011 1011 0111 1111
--   12   13   14   15
-- -------------------------------------------------------------------
-- (1.1) Definir el tipo PosicionUno con cuatro posibles valores, uno
-- por cada una de las cuatro posibles posiciones en las que puede
-- aparecer un 1 en un número binario de cuatro bits. Incluir
-- PosicionUno en la clase Eq con la definición por defecto del
-- operador (==).

-- Solución:
-- ---------

type PosicionUno = Int -> Int

-- -------------------------------------------------------------------
-- Diremos que dos elementos, pi y pj, del tipo PosicionUno verifican
-- la relación pi <= pj si pi se corresponde con una posición anterior
-- (o la misma) a pj.
-- Por ejemplo,
-- Si pi representa la segunda posición y pj representa la cuarta se
--    verifica pi <= pj.
-- Si pi representa la tercera posición y pj representa la primera no
--    se verifica pi <= pj.
-- -------------------------------------------------------------------
-- (1.2) Añadir el tipo PosicionUno a la clase Ord y definir el
-- operador (<=). Para definir el operador utilizar equiparación de
-- patrones (incluir tantas ecuaciones como sea necesario).

-- Solución:
-- ---------


-- -------------------------------------------------------------------
-- Para representar un número binario sólo indicaremos las
-- posiciones de los 1 que aparecen en el mismo.
-- Por ejemplo,
-- 0000: No tiene ningún uno. 
-- 1010: Los 1 ocupan la primera y la tercera posición.
-- 0111: Los 1 ocupan la segunda, la tercera y la cuarta posición.
-- -------------------------------------------------------------------
-- (1.3) Definir el tipo Binario para representar un número binario
-- enumerando las posiciones de los 1.  Para representar los números
-- con uno (o más) 1 utilizar una constructor recursivo que combine la
-- posición de un 1 y un número binario.

-- Solución:
-- ---------


-- -------------------------------------------------------------------
-- Consideramos que un elemento x de tipo Binario representa
-- correctamente a un número binario n con uno (o más) 1, si x es la
-- combinación de la menor posición en la que aparece un 1 y la
-- representación correcta del número binario que resulta al cambiar dicho
-- 1 por un 0 en n (o el cero si ya no quedan más 1 en n).

-- Por ejemplo:
-- Para que x represente correctamente al 0110 debe ser la combinación
-- de: hay un 1 en la segunda posición y la representación correcta
-- del número 0010.
-- Para que x represente correctamente al 0001 debe ser la combinación
-- de: hay un 1 en la cuarta posición y el número 0000.
-- -------------------------------------------------------------------
-- (1.4) Definir la función

-- esCorrecta :: Binario -> Bool

-- que dado un elemento de tipo Binario determine si representa
-- correctamente a un número binario según lo descrito anteriormente.

-- Solución:
-- ---------


-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 2 (2 ptos)
-- Consideremos la siguientes definiciones para representar números
-- binarios con cualquier cantidad de bits.

-- dos elementos; C representa al 0 y U al 1.

data Bit = C | U
  
-- dos elementos; números binarios con un único bit o una sucesión
-- ordenada de bits.

data BinarioAlt = BU Bit | BS Bit BinarioAlt
       
-- Por ejemplo:
-- El 0110 vendría dado por BS C (BS U (BS U (BU C)))
-- El 0001 vendría dado por BS C (BS C (BS C (BU U)))

instance Arbitrary Bit where
  arbitrary = elements [C, U]

instance Arbitrary BinarioAlt where
  arbitrary =
    do cs <- arbitrary
       let mano [] = BU C
           mano [c] = BU c
           mano (c:cs) = BS c (mano cs)
       return (mano cs)

-- -------------------------------------------------------------------
-- (2.1) Incluir ambos tipos en la clase Show y definir la función
-- show para que se comporte como sigue:

-- > BS C (BS U (BS U (BU C)))
-- 0110
-- > BS C (BS C (BS C (BU U)))
-- 0001

-- Solución:
-- ---------


-- -------------------------------------------------------------------
-- (2.2) Definir la función, utilizando recursión terminal, 

binarioEnteroAlt :: BinarioAlt -> Int

-- tal que, dado un número binario x, devuelva el número natural al
-- que representa.

-- Solución:
-- ---------

binarioEnteroAlt n = undefined


-- -------------------------------------------------------------------
-- (2.3) Definir la función,

ceros :: Int -> BinarioAlt -> BinarioAlt

-- tal que, dado un número natural k y otro binario x, devuelva la
-- representación de x que resulta si añadimos al final k ceros.
-- Comprobar con quickCheck que el número natural al que representan
-- es el mismo.
-- Por ejemplo 

-- Solución:
-- ---------

ceros = undefined

-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 3 (2.5 ptos)
-- Los siguientes apartados están encaminados a representar árboles
-- binarios, mostrarlos gráficamente y colorear sus ramas de forma
-- interactiva.

-- Consideremos la siguiente definición de árbol binario en el que
-- cada rama tiene asociada una etiqueta:

data Arbol v r = H v | N (r, Arbol v r) v (r, Arbol v r)
               deriving (Show, Eq)

-- Por ejemplo, consideremos el siguiente árbol

--          * 
--        // \
--       //   \
--      +     *
--     / \\  
--    *   * 

-- en el que los vértices son caracteres y las ramas están etiquetadas
-- con valores lógicos (las ramas que aparecen con tramo doble tendrán
-- True como etiqueta; el resto False). Dicho árbol se representa por:

ejemplo :: Arbol Char Bool
ejemplo = N (True, N (False, (H '*')) '+' (True, (H '*')))
          '*' (False, (H '*'))

-- -------------------------------------------------------------------
-- Ejercicio 4 (2 ptos)
-- Las matrices se pueden representar mediante listas de listas. Por
-- ejemplo, la matriz
--    |1 2 5| 
--    |3 0 7| 
--    |9 1 6| 
--    |6 4 2|
-- se puede representar por

ej :: [[Int]]
ej = [[1,2,5],
      [3,0,7],
      [9,1,6],
      [6,4,2]]
-- -------------------------------------------------------------------
-- (4.1) Definir la función, utilizando plegado y/o listas por
-- compresión, 

ordenaPor :: Ord a => [[a]] -> Int -> [[a]]

-- tal que, dada una matriz xss y el índice de una de sus columnas k,
-- devuelva la matriz obtenida ordenando la matriz xss por los
-- elementos de la columna k. Por ejemplo,
--    ordenaPor ej 0  ==  [[1,2,5],[3,0,7],[6,4,2],[9,1,6]]
--    ordenaPor ej 1  ==  [[3,0,7],[9,1,6],[1,2,5],[6,4,2]]
--    ordenaPor ej 2  ==  [[6,4,2],[1,2,5],[9,1,6],[3,0,7]]

-- Solución:
-- ---------

ordenaPor = undefined

-- -------------------------------------------------------------------
-- Ejercicio 5 (2 ptos)
-- Los árboles se pueden representar mediante el siguiente tipo de
-- datos

data ArbolM t = NM t [ArbolM t]
             deriving Show
                      
-- Por ejemplo, los árboles
--      1         1             1          
--     / \       / \           / \   
--    8   3     8   3         8   3  
--        |        /|\       /|\  |   
--        4       4 5 6     4 5 6 7
-- se pueden representan por

ej1, ej2, ej3 :: ArbolM Int
ej1 = NM 1 [NM 8 [],NM 3 [NM 4 []]]
ej2 = NM 1 [NM 8 [], NM 3 [NM 4 [], NM 5 [], NM 6 []]]
ej3 = NM 1 [NM 8 [NM 4 [], NM 5 [], NM 6 []], NM 3 [NM 7 []]]

-- (5.1) Definir la función

nodos :: Int -> ArbolM t -> [t]

-- tal que, dado un número natural k y un árbol a, devuelva la lista
-- de los nodos de a que tienen k sucesores. Por ejemplo,
--    nodos 0 ej1  ==  [8,4]
--    nodos 1 ej1  ==  [3]
--    nodos 2 ej1  ==  [1]
--    nodos 3 ej1  ==  []
--    nodos 3 ej2  ==  [3]
-- ----------------------------------------------------------------------------

-- Solución:
-- ---------

nodos = undefined

-- -------------------------------------------------------------------
