-- PD- 2020-21
-- Tipos: Multiconjuntos como Diccionarios
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

-- El manual, con ejemplos, de la librería Data.Map se encuentra en
-- http://bit.ly/25B1na0

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
--    fromList [(a,3), (b,2), (c,1), (e,1)]

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
vacio = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    unitario :: a -> MultiConj a
-- tal que (unitario x) es el multiconjunto cuyo único elemento es
-- x. Por ejemplo,
--    unitario 'a'  ==  fromList [('a',1)]
-- ---------------------------------------------------------------------

unitario :: a -> MultiConj a
unitario x = undefined

-- ---------------------------------------------------------------------
-- Añadir y quitar elementos                                          --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    inserta :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (inserta x m) es el multiconjunto obtenido añadiéndole a m el 
-- elemento x. Por ejemplo,
--    ghci> inserta 'a' (unitario 'a')
--    fromList [('a',2)]
--    ghci> inserta 'b' it
--    fromList [('a',2),('b',1)]
--    ghci> inserta 'a' it
--    fromList [('a',3),('b',1)]
--    ghci> inserta 'b' it
--    fromList [('a',3),('b',2)]
-- ---------------------------------------------------------------------

inserta :: Ord a => a -> MultiConj a -> MultiConj a
inserta x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    listaAmc :: Ord a => [a] -> MultiConj a
-- tal que (listaAmc xs) es el multiconjunto cuyos elementos son los de
-- la lista xs. Por ejemplo,
--    listaAmc "ababc"  ==  fromList [('a',2),('b',2),('c',1)]
-- ---------------------------------------------------------------------

listaAmc :: Ord a => [a] -> MultiConj a
listaAmc xs = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (insertaVarios x n m) es el multiconjunto obtenido
-- añadiéndole a m n copias del elemento x. Por ejemplo, 
--    ghci> insertaVarios 'a' 3 vacio
--    fromList [('a',3)]
--    ghci> insertaVarios 'b' 2 it 
--    fromList [('a',3),('b',2)]
--    ghci> insertaVarios 'a' 2 it 
--    fromList [('a',5),('b',2)]
-- ---------------------------------------------------------------------

insertaVarios :: Ord a => a -> Int -> MultiConj a -> MultiConj a
insertaVarios x n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    borra :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borra x m) es el multiconjunto obtenido borrando una
-- ocurrencia de x en m. Por ejemplo,
--    ghci> borra 'a' (listaAmc "ababc")
--    fromList [('a',1),('b',2),('c',1)]
--    ghci> borra 'a' it
--    fromList [('b',2),('c',1)]
--    ghci> borra 'a' it
--    fromList [('b',2),('c',1)]
-- ---------------------------------------------------------------------

borra :: Ord a => a -> MultiConj a -> MultiConj a
borra x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función
--    borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
-- tal que (borraVarias x n m) es el multiconjunto obtenido a partir del
-- m borrando n ocurrencias del elemento x. Por ejemplo,
--    ghci> listaAmc "ababcad"
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    ghci> borraVarias 'a' 2 (listaAmc "ababcad")
--    fromList [('a',1),('b',2),('c',1),('d',1)]
--    ghci> borraVarias 'a' 5 (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

borraVarias :: Ord a => a -> Int -> MultiConj a -> MultiConj a
borraVarias x n = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función
--    borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
-- tal que (borraTodas x m) es el multiconjunto obtenido a partir del
-- m borrando todas las ocurrencias del elemento x. Por ejemplo,
--    ghci> borraTodas 'a' (listaAmc "ababcad")
--    fromList [('b',2),('c',1),('d',1)]
-- ---------------------------------------------------------------------

borraTodas :: Ord a => a -> MultiConj a -> MultiConj a
borraTodas x = undefined

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
esVacio = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    cardinal :: MultiConj a -> Int
-- tal que (cardinal m) es el número de elementos (contando las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardinal (listaAmc "ababcad")  ==  7
-- ---------------------------------------------------------------------

cardinal :: MultiConj a -> Int
cardinal = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    cardDistintos :: MultiConj a -> Int
-- tal que (cardDistintos m) es el número de elementos (sin contar las
-- repeticiones) del multiconjunto m. Por ejemplo,
--    cardDistintos (listaAmc "ababcad")  ==  4
-- ---------------------------------------------------------------------

cardDistintos :: MultiConj a -> Int
cardDistintos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    pertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (pertenece x m) se verifica si el elemento x pertenece al
-- multiconjunto m. Por ejemplo,
--    pertenece 'b' (listaAmc "ababcad")  ==  True
--    pertenece 'r' (listaAmc "ababcad")  ==  False
-- ---------------------------------------------------------------------

pertenece :: Ord a => a -> MultiConj a -> Bool
pertenece x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 13. Definir la función
--    noPertenece :: Ord a => a -> MultiConj a -> Bool
-- tal que (noPertenece x m) se verifica si el elemento x no pertenece al
-- multiconjunto m. Por ejemplo,
--    noPertenece 'b' (listaAmc "ababcad")  ==  False
--    noPertenece 'r' (listaAmc "ababcad")  ==  True
-- ---------------------------------------------------------------------

noPertenece :: Ord a => a -> MultiConj a -> Bool
noPertenece x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 14. Definir la función
--    ocurrencias :: Ord a => a -> MultiConj a -> Int
-- tal que (ocurrencias x m) es el número de ocurrencias de x en el
-- multiconjunto m. Por ejemplo,
--    ocurrencias 'a' (listaAmc "ababcad")  ==  3
--    ocurrencias 'r' (listaAmc "ababcad")  ==  0
-- ---------------------------------------------------------------------

ocurrencias :: Ord a => a -> MultiConj a -> Int
ocurrencias x = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 15: Definir la función 
--    elementos :: Ord a => MultiConj a -> [a]
-- tal que (elementos m) es la lista de los elementos (sin repeticiones)
-- del multiconjunto m. Por ejemplo,
--    elementos (listaAmc "ababcad")  ==  "abcd"
-- ---------------------------------------------------------------------

elementos :: Ord a => MultiConj a -> [a]
elementos = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 16.Definir la función
--    esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
-- tal que (esSubmultiConj m1 m2) se verifica si m1 es un
-- submulticonjuto de m2 (es decir; los elementos de m1 pertenecen a m2
-- con un númro de ocurrencias igual o mayor). Por ejemplo,
--    ghci> let m1 = listaAmc "ababcad"
--    ghci> let m2 = listaAmc "bcbaadaa"
--    ghci> m1
--    fromList [('a',3),('b',2),('c',1),('d',1)]
--    ghci> m2
--    fromList [('a',4),('b',2),('c',1),('d',1)]
--    ghci> esSubmultiConj m1 m2
--    True
--    ghci> esSubmultiConj m2 m1
--    False
-- ---------------------------------------------------------------------

esSubmultiConj :: Ord a => MultiConj a -> MultiConj a -> Bool
esSubmultiConj m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Operaciones: unión, intersección y diferencia de multiconjuntos    --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 17. Definir la función
--    union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (union m1 m2) es la unión de los multiconjuntos m1 y m2. Por
-- ejemplo, 
--    ghci> let m1 = listaAmc "cdacba"
--    ghci> let m2 = listaAmc "acec"
--    ghci> m1
--    fromList [('a',2),('b',1),('c',2),('d',1)]
--    ghci> m2
--    fromList [('a',1),('c',2),('e',1)]
--    ghci> union m1 m2
--    fromList [('a',3),('b',1),('c',4),('d',1),('e',1)]
-- ---------------------------------------------------------------------

union :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
union m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 18. Definir la función
--    unionG :: Ord a => [MultiConj a] -> MultiConj a
-- tal que (unionG ms) es la unión de la lista de multiconjuntos ms. Por
-- ejemplo, 
--    ghci> unionG (map listaAmc ["aba", "cda", "bdb"])
--    fromList [('a',3),('b',3),('c',1),('d',2)]
-- ---------------------------------------------------------------------

unionG :: Ord a => [MultiConj a] -> MultiConj a
unionG ms = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 19. Definir la función
--    diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (diferencia m1 m2) es la diferencia de los multiconjuntos m1
-- y m2. Por ejemplo,
--    ghci> diferencia (listaAmc "abacc") (listaAmc "dcb")
--    fromList [('a',2),('c',1)]
-- ---------------------------------------------------------------------

diferencia :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
diferencia m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 20. Definir la función
--    interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
-- tal que (interseccion m1 m2) es la intersección de los multiconjuntos
-- m1 y m2. Por ejemplo,
--    ghci> interseccion (listaAmc "abcacc") (listaAmc "bdcbc")
--    fromList [('b',1),('c',2)]
-- ---------------------------------------------------------------------

interseccion :: Ord a => MultiConj a -> MultiConj a -> MultiConj a
interseccion m1 m2 = undefined

-- ---------------------------------------------------------------------
-- Filtrado y partición                                               --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 21. Definir la función
--    filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
-- tal que (filtra p m) es el multiconjunto de los elementos de m que
-- verifican la propiedad p. Por ejemplo,
--    ghci> filtra (>'b') (listaAmc "abaccaded") 
--    fromList [('c',2),('d',2),('e',1)]
-- ---------------------------------------------------------------------

filtra :: Ord a => (a -> Bool) -> MultiConj a -> MultiConj a
filtra p = undefined

-- ---------------------------------------------------------------------
-- Función aplicativa                                                 --
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 22. Definir la función
--    mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
-- tal que (mapMC f m) es el multiconjunto obtenido aplicando la función
-- f a todos los  elementos de m. Por ejemplo,
--    ghci> mapMC (:"N") (listaAmc "abaccaded") 
--    fromList [("aN",3),("bN",1),("cN",2),("dN",2),("eN",1)]
-- ---------------------------------------------------------------------

mapMC :: Ord b => (a -> b) -> MultiConj a -> MultiConj b
mapMC f = undefined

-- ---------------------------------------------------------------------
-- Creación del módulo
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 23. Redefine la forma en que se imprime una expresión de
-- tipo multiconjunto para que, por ejemplo, el multiconjunto:
-- [("a",3),("b",1)] se escriba como "a*3,b*1"
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 24. Incluye el código necesario en este fichero para que sea
-- un módulo donde solo se permita el uso de las funciones definidas,
-- ocultando los detalles de implementación, definiendo de tal forma el 
-- TAD de los multiconjuntos.
-- ---------------------------------------------------------------------

