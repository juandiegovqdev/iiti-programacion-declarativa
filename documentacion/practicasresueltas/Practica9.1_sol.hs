-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------
 
-- El objetivo de esta relación de ejercicios es definir funciones sobre 
-- el TAD de las pilas, utilizando las implementaciones estudiadas en el 
-- tema 14 cuyas transparencias se encuentran en 
--    http://www.cs.us.es/~jalonso/cursos/i1m-19/temas/tema-14.html
-- 
-- Para realizar los ejercicios hay que instalar la librería I1M que
-- contiene la implementación de TAD de las pilas. Los pasos para
-- instalarla son los siguientes:
-- + cabal update
-- + cabal install I1M
--
-- Otra forma es descargar las implementaciones de las implementaciones
-- de las pilas:
-- + PilaConTipoDeDatoAlgebraico.hs que está en http://bit.ly/21z3g49
-- + PilaConListas.hs               que está en http://bit.ly/21z3oAD
 
-- ---------------------------------------------------------------------
-- Importación de librerías                                           --
-- ---------------------------------------------------------------------
 
import Data.List
import Test.QuickCheck
 
-- Hay que elegir una implementación del TAD pilas.
import PilaConTipoDeDatoAlgebraico
-- import PilaConListas
-- import I1M.Pila
 
-- ---------------------------------------------------------------------
-- Ejemplos
-- ---------------------------------------------------------------------
 
-- A lo largo de esta relación de ejercicios usaremos los siguientes
-- ejemplos de pila
p1, p2, p3, p4, p5 :: Pila Int
p1 = foldr apila vacia [1..20]
p2 = foldr apila vacia [2,5..18]
p3 = foldr apila vacia [3..10]
p4 = foldr apila vacia [4,-1,7,3,8,10,0,3,3,4]
p5 = foldr apila vacia [1..5]
 
-- ---------------------------------------------------------------------
-- Ejercicio 1: Definir la función
--    filtraPila :: (a -> Bool) -> Pila a -> Pila a
-- tal que (filtraPila p q) es la pila obtenida con los elementos de
-- pila q que verifican el predicado p, en el mismo orden. Por ejemplo,
--    ghci> p1
--    1|2|3|4|5|6|7|8|9|10|11|12|13|14|15|16|17|18|19|20|-
--    ghci> filtraPila even p1
--    2|4|6|8|10|12|14|16|18|20|-
-- ---------------------------------------------------------------------
 
filtraPila :: (a -> Bool) -> Pila a -> Pila a
filtraPila p q
    | esVacia q = vacia
    | p cq      = apila cq (filtraPila p dq)
    | otherwise = filtraPila p dq
    where cq = cima q
          dq = desapila q
 
-- ---------------------------------------------------------------------
-- Ejercicio 2: Definir la función
--    mapPila :: (a -> a) -> Pila a -> Pila a
-- tal que (mapPila f p) es la pila formada con las imágenes por f de
-- los elementos de pila p, en el mismo orden. Por ejemplo,
--    ghci> mapPila (+7) p1
--    8|9|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|-
-- ---------------------------------------------------------------------
 
mapPila :: (a -> a) -> Pila a -> Pila a
mapPila f p
    | esVacia p = p
    | otherwise = apila (f cp) (mapPila f dp)
    where cp = cima p
          dp = desapila p
 
-- ---------------------------------------------------------------------
-- Ejercicio 3: Definir la función
--    pertenecePila :: Eq a => a -> Pila a -> Bool
-- tal que (pertenecePila y p) se verifica si y es un elemento de la
-- pila p. Por ejemplo,
--    pertenecePila 7 p1  == True
--    pertenecePila 70 p1 == False
-- ---------------------------------------------------------------------
 
pertenecePila :: Eq a => a -> Pila a -> Bool
pertenecePila x p 
    | esVacia p  = False
    | otherwise  = x == cp || pertenecePila x dp
    where cp = cima p
          dp = desapila p
 
-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--    contenidaPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (contenidaPila p1 p2) se verifica si todos los elementos de
-- de la pila p1 son elementos de la pila p2. Por ejemplo,
--    contenidaPila p2 p1  == True
--    contenidaPila p1 p2  == False
-- ---------------------------------------------------------------------
 
contenidaPila :: Eq a => Pila a -> Pila a -> Bool
contenidaPila p1 p2 
    | esVacia p1 = True
    | otherwise  = pertenecePila cp1 p2 && contenidaPila dp1 p2 
    where cp1 = cima p1
          dp1 = desapila p1
 
-- ---------------------------------------------------------------------
-- Ejercicio 4: Definir la función
--    prefijoPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (prefijoPila p1 p2) se verifica si la pila p1 es justamente
-- un prefijo de la pila p2. Por ejemplo,
--    prefijoPila p3 p2 == False
--    prefijoPila p5 p1 == True
-- ---------------------------------------------------------------------
 
prefijoPila :: Eq a => Pila a -> Pila a -> Bool
prefijoPila p1 p2 
    | esVacia p1 = True
    | esVacia p2 = False
    | otherwise  = cp1 == cp2 && prefijoPila dp1 dp2
    where cp1 = cima p1
          dp1 = desapila p1
          cp2 = cima p2
          dp2 = desapila p2
 
-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función
--    subPila :: Eq a => Pila a -> Pila a -> Bool
-- tal que (subPila p1 p2) se verifica si p1 es una subpila de p2. Por
-- ejemplo,
--    subPila p2 p1 == False
--    subPila p3 p1 == True
-- ---------------------------------------------------------------------
 
subPila :: Eq a => Pila a -> Pila a -> Bool
subPila p1 p2
    | esVacia p1 = True
    | esVacia p2 = False
    | cp1 == cp2 = prefijoPila dp1 dp2 || subPila p1 dp2
    | otherwise  = subPila p1 dp2
    where cp1 = cima p1
          dp1 = desapila p1
          cp2 = cima p2
          dp2 = desapila p2
 
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función
--    ordenadaPila :: Ord a => Pila a -> Bool
-- tal que (ordenadaPila p) se verifica si los elementos de la pila p
-- están ordenados en orden creciente. Por ejemplo,
--    ordenadaPila p1 == True
--    ordenadaPila p4 == False
-- ---------------------------------------------------------------------
 
ordenadaPila :: Ord a => Pila a -> Bool
ordenadaPila p 
    | esVacia p  = True
    | esVacia dp = True
    | otherwise  = cp <= cdp && ordenadaPila dp
    where cp  = cima p
          dp  = desapila p
          cdp = cima dp
 
-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir la función
--    lista2Pila :: [a] -> Pila a
-- tal que (lista2Pila xs) es la pila formada por los elementos de
-- xs. Por ejemplo,
--    lista2Pila [1..6] == 1|2|3|4|5|6|-
-- ---------------------------------------------------------------------
 
lista2Pila :: [a] -> Pila a
lista2Pila = foldr apila vacia
 
-- ---------------------------------------------------------------------
-- Ejercicio 7.2. Definir la función
--    pila2Lista :: Pila a -> [a]
-- tal que (pila2Lista p) es la lista formada por los elementos de la
-- lista p. Por ejemplo,
--    pila2Lista p2 == [2,5,8,11,14,17]
-- ---------------------------------------------------------------------
 
pila2Lista :: Pila a -> [a]
pila2Lista p
    | esVacia p = []
    | otherwise = cp : pila2Lista dp
    where cp = cima p
          dp = desapila p
 
-- ---------------------------------------------------------------------
-- Ejercicio 7.3. Comprobar con QuickCheck que la función pila2Lista es
-- la inversa de lista2Pila, y recíprocamente.
-- ---------------------------------------------------------------------
 
prop_pila2Lista p =
    lista2Pila (pila2Lista p) == p
 
-- ghci> quickCheck prop_pila2Lista
-- +++ OK, passed 100 tests.
 
prop_lista2Pila xs =
    pila2Lista (lista2Pila xs) == xs
 
-- ghci> quickCheck prop_lista2Pila
-- +++ OK, passed 100 tests.
 
-- ---------------------------------------------------------------------
-- Ejercicio 8.1. Definir la función 
--    ordenaInserPila :: Ord a => Pila a -> Pila a
-- tal que (ordenaInserPila p) es la pila obtenida ordenando por
-- inserción los los elementos de la pila p. Por ejemplo,
--    ghci> ordenaInserPila p4
--    -1|0|3|3|3|4|4|7|8|10|-
-- ---------------------------------------------------------------------
 
ordenaInserPila :: Ord a => Pila a -> Pila a
ordenaInserPila p
    | esVacia p = p
    | otherwise = insertaPila cp (ordenaInserPila dp)
    where cp = cima p
          dp = desapila p
 
insertaPila :: Ord a => a -> Pila a -> Pila a
insertaPila x p 
    | esVacia p = apila x p
    | x < cp    = apila x p
    | otherwise = apila cp (insertaPila x dp)
    where cp = cima p
          dp = desapila p
 
-- ---------------------------------------------------------------------
-- Ejercicio 8.2. Comprobar con QuickCheck que la pila 
--    (ordenaInserPila p) 
-- está ordenada correctamente.
-- ---------------------------------------------------------------------
 
prop_ordenaInserPila p =
    pila2Lista (ordenaInserPila p) == sort (pila2Lista p)
 
-- ghci> quickCheck prop_ordenaInserPila
-- +++ OK, passed 100 tests.
 
-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función
--    nubPila :: Eq a => Pila a -> Pila a
-- tal que (nubPila p) es la pila con los elementos de p sin repeticiones. 
-- Por ejemplo,
--    ghci> p4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> nubPila p4
--    -1|7|8|10|0|3|4|-
-- ---------------------------------------------------------------------
 
nubPila :: (Eq a) => Pila a -> Pila a
nubPila p 
    | esVacia p           = vacia
    | pertenecePila cp dp = nubPila dp
    | otherwise           = apila cp (nubPila dp)
    where cp = cima p
          dp = desapila p
 
-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir la propiedad siguiente: "la composición de
-- las funciones nub y pila2Lista coincide con la composición de las
-- funciones pila2Lista y nubPila", y comprobarla con QuickCheck.
-- En caso de ser falsa, redefinir la función nubPila para que se
-- verifique la propiedad.
-- ---------------------------------------------------------------------
 
-- La propiedad es
prop_nubPila p =
    nub (pila2Lista p) == pila2Lista (nubPila p)
 
-- La comprobación es
--    ghci> quickCheck prop_nubPila
--    *** Failed! Falsifiable (after 8 tests):  
--    -7|-2|0|-5|-7|-
--    ghci> let p = foldr apila vacia [-7,-2,0,-5,-7]
--    ghci> p
--    -7|-2|0|-5|-7|-
--    ghci> pila2Lista p
--    [-7,-2,0,-5,-7]
--    ghci> nub (pila2Lista p)
--    [-7,-2,0,-5]
--    ghci> nubPila p
--    -2|0|-5|-7|-
--    ghci> pila2Lista (nubPila p)
--    [-2,0,-5,-7]
 
-- Falla porque nub quita el último de los elementos repetidos de la
-- lista, mientras que nubPila quita el primero de ellos.
 
-- La redefinimos
nubPila' :: Eq a => Pila a -> Pila a
nubPila' p 
    | esVacia p           = p
    | pertenecePila cp dp = apila cp (nubPila' (eliminaPila cp dp))
    | otherwise           = apila cp (nubPila' dp)
    where cp = cima p
          dp = desapila p
 
eliminaPila :: Eq a => a -> Pila a -> Pila a
eliminaPila x p 
    | esVacia p = p
    | x == cp    = eliminaPila x dp
    | otherwise = apila cp (eliminaPila x dp)
    where cp = cima p
          dp = desapila p
 
-- La propiedad es
prop_nubPila' p =
    nub (pila2Lista p) == pila2Lista (nubPila' p)
 
-- La comprobación es
--    ghci> quickCheck prop_nubPila'
--    +++ OK, passed 100 tests.
 
-- ---------------------------------------------------------------------
-- Ejercicio 10: Definir la función 
--    maxPila :: Ord a => Pila a -> a
-- tal que (maxPila p) sea el mayor de los elementos de la pila p. Por
-- ejemplo, 
--    ghci> p4
--    4|-1|7|3|8|10|0|3|3|4|-
--    ghci> maxPila p4
--    10
-- ---------------------------------------------------------------------
 
maxPila :: Ord a => Pila a -> a
maxPila p 
    | esVacia p = error "pila vacia"
    | esVacia dp = cp
    | otherwise = max cp (maxPila dp)
    where cp = cima p
          dp = desapila p
 
-- ---------------------------------------------------------------------
-- Generador de pilas                                          --
-- ---------------------------------------------------------------------
 
-- genPila es un generador de pilas. Por ejemplo,
--    ghci> sample genPila
--    -
--    0|0|-
--    -
--    -6|4|-3|3|0|-
--    -
--    9|5|-1|-3|0|-8|-5|-7|2|-
--    -3|-10|-3|-12|11|6|1|-2|0|-12|-6|-
--    2|-14|-5|2|-
--    5|9|-
--    -1|-14|5|-
--    6|13|0|17|-12|-7|-8|-19|-14|-5|10|14|3|-18|2|-14|-11|-6|-
genPila :: (Num a, Arbitrary a) => Gen (Pila a)
genPila = do xs <- listOf arbitrary
             return (foldr apila vacia xs)
 
-- El tipo pila es una instancia del arbitrario. 
instance (Arbitrary a, Num a) => Arbitrary (Pila a) where
    arbitrary = genPila

