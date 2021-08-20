-- PD
-- El TAD de los multiconjuntos mediante diccionarios.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- Un multiconjunto es una colección de elementos en los que no importa
-- el orden de los elementos, pero sí el número de veces en que
-- aparecen. Por ejemplo, la factorización prima de un número se puede
-- representar como un multiconjunto de números primos. 
-- 
-- El objetivo de esta relación de ejercicios es implementar el TAD de
-- los multiconjuntos utilizando los diccionarios.
-- 
-- El manual, con ejemplos, de la librería Data.Map se encuentra en
-- http://bit.ly/25B1na0

-- Creación del módulo. Se debería renombrar el fichero a Multiconjunto.hs

module Multiconjunto
  ( MultiConj
   , vacio
   , unitario
   , inserta
   , listaAmc
   , insertaVarios
   , borra
   , borraVarias
   , borraTodas
   , esVacio
   , cardinal
   , cardDistintos
   , pertenece
   , noPertenece
   , ocurrencias
   , elementos
   , esSubmultiConj
   , union
   , unionG
   , diferencia
   , interseccion
   , filtra
   , mapMC
  ) where


-- ---------------------------------------------------------------------
-- Librerías auxiliares                                               --
-- ---------------------------------------------------------------------

import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- El tipo de dato de multiconjuntos                                  --
-- ---------------------------------------------------------------------

-- Un multiconjunto se puede representar mediante un diccionario donde
-- las claves son los elementos del multiconjunto y sus valores sus
-- números de ocurrencias. Por ejemplo, el multiconjunto 
--    {a, b, a, c, b, a, e}
-- se representa por el diccionario
--    [(a,3), (b,2), (c,1), (e,1)]

type MultiConj a = M.Map a Int
 
-- ---------------------------------------------------------------------
-- Construcciones de multiconjuntos                                   --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la constante
--    vacio :: MultiConj a
-- para el multiconjunto vacío. Por ejemplo,
--    vacio  ==  fromList []
-- ---------------------------------------------------------------------

vacio :: MultiConj a
vacio = M.empty

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    unitario :: a -> MultiConj a
-- tal que (unitario x) es el multiconjunto cuyo único elemento es
-- x. Por ejemplo,
--    unitario 'a'  ==  fromList [('a',1)]
-- ---------------------------------------------------------------------

unitario :: a -> MultiConj a
unitario x = M.singleton x 1

-- ---------------------------------------------------------------------
-- Añadir y quitar elementos                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inserta :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (inserta x m) es el multiconjunto obtenido añadiéndole a m el 
-- elemento x. Por ejemplo,
--    λ> inserta 'a' (unitario 'a')
--    fromList [('a',2)]
--    λ> inserta 'b' it
--    fromList [('a',2),('b',1)]
--    λ> inserta 'a' it
--    fromList [('a',3),('b',1)]
--    λ> inserta 'b' it
--    fromList [('a',3),('b',2)]
-- ---------------------------------------------------------------------

inserta :: Ord a => a -> MultiConj a -> MultiConj a
inserta x = M.insertWith (+) x 1

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    listaAmc :: Ord a => [a] -> MultiConj a
-- tal que (listaAmc xs) es el multiconjunto cuyos elementos son los de
-- la lista xs. Por ejemplo,
--    listaAmc "ababc"  ==  fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

listaAmc :: Ord a => [a] -> MultiConj a
listaAmc xs = M.fromListWith (+) (zip xs (repeat 1))

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (insertaVarios x n m) es el multiconjunto obtenido
-- añadiéndole a m n copias del elemento x. Por ejemplo, 
--    λ> insertaVarios 'a' 3 vacio
--    fromList [('a',3)]
--    λ> insertaVarios 'b' 2 it 
--    fromList [('a',3),('b',2)]
--    λ> insertaVarios 'a' 2 it 
--    fromList [('a',5),('b',2)]
-- ---------------------------------------------------------------------

insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios x n = M.insertWith (+) x n 

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    borra :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borra x m) es el multiconjunto obtenido borrando una
-- ocurrencia de x en m. Por ejemplo,
--    λ> borra 'a' (listaAmc "ababc")
--    fromList [('a',1),('b',2),('c',1)]
--    λ> borra 'a' it
--    fromList [('b',2),('c',1)]
--    λ> borra 'a' it
--    fromList [('b',2),('c',1)]
-- ---------------------------------------------------------------------

borra :: Ord a => a -> MultiConj a -> MultiConj a
borra x = M.update f x
    where f m | m <= 1    = Nothing
              | otherwise = Just (m - 1)

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (borraVarias x n m) es el multiconjunto obtenido a partir del
-- m borrando n ocurrencias del elemento x. Por ejemplo,
--    λ> listaAmc "ababcad"
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    λ> borraVarias 'a' 2 (listaAmc "ababcad")
--    fromList [('a',1),('b',2),('c',1),('d',1)]
--    λ> borraVarias 'a' 5 (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias x n = M.update (f n) x
    where f n m | m <= n    = Nothing
                | otherwise = Just (m - n)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borraTodas x m) es el multiconjunto obtenido a partir del
-- m borrando todas las ocurrencias del elemento x. Por ejemplo,
--    λ> borraTodas 'a' (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
borraTodas x = M.delete x

-- ---------------------------------------------------------------------
-- Consultas                                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 9. Definir la función 
--    esVacio :: MultiConj a -> Bool
-- tal que (esVacio m) se verifica si el multiconjunto m es vacío. Por
-- ejemplo, 
--    esVacio vacio  ==  True
--    esVacio (inserta 'a' vacio)  ==  False
-- ---------------------------------------------------------------------

esVacio :: MultiConj a -> Bool
esVacio = M.null

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    cardinal :: MultiConj a -> Int
-- tal que (cardinal m) es el número de elementos (contando las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardinal (listaAmc "ababcad")  ==  7
-- ---------------------------------------------------------------------

cardinal :: MultiConj a -> Int
cardinal = sum . M.elems

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    cardDistintos :: MultiConj a -> Int
-- tal que (cardDistintos m) es el número de elementos (sin contar las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardDistintos (listaAmc "ababcad")  ==  4
-- ---------------------------------------------------------------------

cardDistintos :: MultiConj a -> Int
cardDistintos = M.size

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    pertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (pertenece x m) se verifica si el elemento x pertenece al
-- multiconjunto m. Por ejemplo,
--    pertenece 'b' (listaAmc "ababcad")  ==  True
--    pertenece 'r' (listaAmc "ababcad")  ==  False
-- ---------------------------------------------------------------------

pertenece :: Ord a => a -> MultiConj a -> Bool
pertenece x = M.member x

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    noPertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (noPertenece x m) se verifica si el elemento x no pertenece al
-- multiconjunto m. Por ejemplo,
--    noPertenece 'b' (listaAmc "ababcad")  ==  False
--    noPertenece 'r' (listaAmc "ababcad")  ==  True
-- ---------------------------------------------------------------------

noPertenece :: Ord a => a -> MultiConj a -> Bool
noPertenece x = not . pertenece x

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    ocurrencias :: Ord a => a -> MultiConj a -> Int
-- tal que (ocurrencias x m) es el número de ocurrencias de x en el
-- multiconjunto m. Por ejemplo,
--    ocurrencias 'a' (listaAmc "ababcad")  ==  3
--    ocurrencias 'r' (listaAmc "ababcad")  ==  0
-- ---------------------------------------------------------------------

ocurrencias :: Ord a => a -> MultiConj a -> Int
ocurrencias x = M.findWithDefault 0 x

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la función 
--    elementos :: Ord a => MultiConj a -> [a]
-- tal que (elementos m) es la lista de los elementos (sin repeticiones)
-- del multiconjunto m. Por ejemplo,
--    elementos (listaAmc "ababcad")  ==  "abcd"
-- ---------------------------------------------------------------------

elementos :: Ord a => MultiConj a -> [a]
elementos = M.keys

-- ---------------------------------------------------------------------
-- Ejercicio 16.Definir la función
--    esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
-- tal que (esSubmultiConj m1 m2) se verifica si m1 es un
-- submulticonjuto de m2 (es decir; los elementos de m1 pertenecen a m2
-- con un númro de ocurrencias igual o mayor). Por ejemplo,
--    λ> let m1 = listaAmc "ababcad"
--    λ> let m2 = listaAmc "bcbaadaa"
--    λ> m1
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    λ> m2
--    fromList [('a',4),('b',2),('c',1),('d',1)]
--    λ> esSubmultiConj m1 m2
--    True
--    λ> esSubmultiConj m2 m1
--    False
-- ---------------------------------------------------------------------

esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj m1 m2 =
    all (\x -> ocurrencias x m1 <= ocurrencias x m2)
        (elementos m1)

-- ---------------------------------------------------------------------
-- Elemento minimo y máximo de un multiconjunto                       --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    minimo :: MultiConj a -> a
-- tal que (minimo m) es el mínimo elemento del multiconjunto m. Por
-- ejemplo, 
--    minimo (listaAmc "cdacbab")  ==  'a'
-- ---------------------------------------------------------------------

minimo :: MultiConj a -> a
minimo = fst . M.findMin

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    maximo :: MultiConj a -> a
-- tal que (maximo m) es el máximo elemento del multiconjunto m. Por
-- ejemplo, 
--    maximo (listaAmc "cdacbab")  ==  'd'
-- ---------------------------------------------------------------------

maximo :: MultiConj a -> a
maximo = fst . M.findMax

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función   
--    borraMin :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMin m) es el multiconjunto obtenido eliminando una
-- ocurrencia del menor elemento de m. Por ejemplo,
--    λ> borraMin (listaAmc "cdacbab")
--    fromList [('a',1),('b',2),('c',2),('d',1)]
--    λ> borraMin it
--    fromList [('b',2),('c',2),('d',1)]
-- ---------------------------------------------------------------------

borraMin :: Ord a => MultiConj a -> MultiConj a
borraMin m = borra (minimo m) m

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función   
--    borraMax :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMax m) es el multiconjunto obtenido eliminando una
-- ocurrencia del mayor elemento de m. Por ejemplo,
--    λ> borraMax (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    λ> borraMax it
--    fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

borraMax :: Ord a => MultiConj a -> MultiConj a
borraMax m = borra (maximo m) m

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función   
--    borraMinTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMinTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del menor elemento de m. Por ejemplo,
--    λ> borraMinTodo (listaAmc "cdacbab")
--    fromList [('b',2),('c',2),('d',1)]
--    λ> borraMinTodo it
--    fromList [('c',2),('d',1)]
-- ---------------------------------------------------------------------

borraMinTodo :: Ord a => MultiConj a -> MultiConj a
borraMinTodo m = borraTodas (minimo m) m

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función   
--    borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
-- tal que (borraMaxTodo m) es el multiconjunto obtenido eliminando
-- todas las ocurrencias del mayor elemento de m. Por ejemplo,
--    λ> borraMaxTodo (listaAmc "cdacbab")
--    fromList [('a',2),('b',2),('c',2)]
--    λ> borraMaxTodo it
--    fromList [('a',2),('b',2)]
-- ---------------------------------------------------------------------

borraMaxTodo :: Ord a => MultiConj a -> MultiConj a
borraMaxTodo m = borraTodas (maximo m) m

-- ---------------------------------------------------------------------
-- Operaciones: unión, intersección y diferencia de multiconjuntos    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Definir la función
--    union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (union m1 m2) es la unión de los multiconjuntos m1 y m2. Por
-- ejemplo, 
--    λ> let m1 = listaAmc "cdacba"
--    λ> let m2 = listaAmc "acec"
--    λ> m1
--    fromList [('a',2),('b',2),('c',2),('d',1)]
--    λ> m2
--    fromList [('a',1),('c',2),('e',1)]
--    λ> union m1 m2
--    fromList [('a',3),('b',2),('c',4),('d',1),('e',1)]
-- ---------------------------------------------------------------------

union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
union m1 m2 = M.unionWith (+) m1 m2

-- ---------------------------------------------------------------------
-- Ejercicio 24. Definir la función
--    unionG :: Ord a => [MultiConj a] -> MultiConj a
-- tal que (unionG ms) es la unión de la lista de multiconjuntos ms. Por
-- ejemplo, 
--    λ> unionG (map listaAmc ["aba", "cda", "bdb"])
--    fromList [('a',3),('b',3),('c',1),('d',2)]
-- ---------------------------------------------------------------------

unionG :: Ord a => [MultiConj a] -> MultiConj a
unionG ms = foldr union vacio ms

-- ---------------------------------------------------------------------
-- Ejercicio 25. Definir la función
--    diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (diferencia m1 m2) es la diferencia de los multiconjuntos m1
-- y m2. Por ejemplo,
--    λ> diferencia (listaAmc "abacc") (listaAmc "dcb")
--    fromList [('a',2),('c',1)]
-- ---------------------------------------------------------------------

diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia m1 m2 = M.differenceWith f m1 m2
    where f x y | x <= y    = Nothing
                | otherwise = Just (x - y)

-- ---------------------------------------------------------------------
-- Ejercicio 26. Definir la función
--    interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (interseccion m1 m2) es la intersección de los multiconjuntos
-- m1 y m2. Por ejemplo,
--    λ> interseccion (listaAmc "abacc") (listaAmc "dcbc")
--    fromList [('b',1),('c',2)]
-- ---------------------------------------------------------------------

interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
interseccion m1 m2 = M.intersectionWith min m1 m2

-- ---------------------------------------------------------------------
-- Filtrado y partición                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 27. Definir la función
--    filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
-- tal que (filtra p m) es el multiconjunto de los elementos de m que
-- verifican la propiedad p. Por ejemplo,
--    λ> filtra (>'b') (listaAmc "abaccaded") 
--    fromList [('c',2),('d',2),('e',1)]
-- ---------------------------------------------------------------------

filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
filtra p = M.filterWithKey (\k _ -> p k) 

-- ---------------------------------------------------------------------
-- Ejercicio 28. Definir la función
--    particion :: Ord a => 
--                 (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
-- tal que (particion p m) es el par cuya primera componente consta de
-- los elementos de m que cumplen p y la segunda por los que no lo
-- cumplen. Por ejemplo, 
--    λ> particion (>'b') (listaAmc "abaccaded") 
--    (fromList [('c',2),('d',2),('e',1)],fromList [('a',3),('b',1)])
-- ---------------------------------------------------------------------

particion :: Ord a => 
             (a -> Bool) -> MultiConj a -> (MultiConj a,MultiConj a)
particion p = M.partitionWithKey (\k _ -> p k) 

-- ---------------------------------------------------------------------
-- Función aplicativa                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 29. Definir la función
--    mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
-- tal que (mapMC f m) es el multiconjunto obtenido aplicando la función
-- f a todos los  elementos de m. Por ejemplo,
--    λ> mapMC (:"N") (listaAmc "abaccaded") 
--    fromList [("aN",3),("bN",1),("cN",2),("dN",2),("eN",1)]
-- ---------------------------------------------------------------------

mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
mapMC f = M.mapKeysWith (+) f


-- ---------------------------------------------------------------------
-- Creación del módulo
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Redefine la forma en que se imprime una expresión de
-- tipo multiconjunto para que, por ejemplo, el multiconjunto:
-- [("a",3),("b",1)] se escriba como "a*3,b*1"
-- ---------------------------------------------------------------------

-- No se puede. Multiconjunto no es un nuevo tipo, sino un sinónimo de
-- multiconjunto. Habría que crear un nuevo tipo para ello (con data).

-- ---------------------------------------------------------------------
-- Ejercicio 24. Incluye el código necesario en este fichero para que sea
-- un módulo donde solo se permita el uso de las funciones definidas,
-- ocultando los detalles de implementación, definiendo de tal forma el 
-- TAD de los multiconjuntos.
-- ---------------------------------------------------------------------

-- Ver solución arriba
