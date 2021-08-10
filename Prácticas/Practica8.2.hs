-- PD-Práctica 9.2 2019-20
-- Árboles con tipos de datos algebráicos
-- =====================================================================

-- En esta relación se presenta ejercicios sobre árboles binarios
-- definidos como tipos de datos algebraicos.

-- ---------------------------------------------------------------------
-- Nota. En los siguientes ejercicios se trabajará con los árboles
-- binarios definidos como sigue 
--    data Arbol a = H a
--                 | N a (Arbol a) (Arbol a)
--                 deriving (Show, Eq)
-- Donde la H representa que es una Hoja, y la N es un nodo interior
-- Por ejemplo, el árbol
--         9 
--        / \
--       /   \
--      3     7
--     / \  
--    2   4 
-- se representa por
--    N 9 (N 3 (H 2) (H 4)) (H 7) 
-- ---------------------------------------------------------------------

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
             deriving (Show, Eq)

-- ---------------------------------------------------------------------
-- Ejercicio 1.1. Definir la función
--    nHojas :: Arbol a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojas (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  3
-- ---------------------------------------------------------------------

nHojas :: Arbol a -> Int
nHojas (H a) = 1
nHojas (N _ a1 a2) = nHojas a1 + nHojas a2


-- ---------------------------------------------------------------------
-- Ejercicio 1.2. Definir la función
--    nNodos :: Arbol a -> Int
-- tal que (nNodos x) es el número de nodos del árbol x. Por ejemplo,
--    nNodos (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  2
-- ---------------------------------------------------------------------

nNodos :: Arbol a -> Int
nNodos (N _ a1 a2) = 1 + nNodos a1 + nNodos a2
nNodos (H _) = 0

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. Definir la función
--    profundidad :: Arbol a -> Int
-- tal que (profundidad x) es la profundidad del árbol x. Por ejemplo,
--    profundidad (N 9 (N 3 (H 2) (H 4)) (H 7))              ==  2
--    profundidad (N 9 (N 3 (H 2) (N 1 (H 4) (H 5))) (H 7))  ==  3
--    profundidad (N 4 (N 5 (H 4) (H 2)) (N 3 (H 7) (H 4)))  ==  2
-- ---------------------------------------------------------------------

profundidad :: Arbol a -> Int
profundidad  (H _) = 1
profundidad (N x a1 a2) = profundidadAux (N x a1 a2) 

profundidadAux :: Arbol a -> Int
profundidadAux (H _) = 0
profundidadAux (N _ a1 a2)
      | profundidadAux a1 > profundidadAux a2 = 1 + profundidadAux a1 
      | otherwise                             = 1 + profundidadAux a2

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función
--    anadeHojas :: Arbol a -> a -> a -> Arbol a
-- tal que (anadeHojas a x y) añade a cada hoja del árbol a
-- dos hojas con los datos x e y. Por ejemplo,
--   anadeHojas (H 5) 0 10 == N 5 (H 0) (H 10)
--   anadeHojas (N 7 (H 5) (H 9)) 1 4 == N 7 (N 5 (H 1) (H 4)) (N 9 (H 1) (H 4))
-- ---------------------------------------------------------------------

anadeHojas :: Arbol a -> a -> a -> Arbol a
anadeHojas (H z) x y = N z (H x) (H y)
anadeHojas (N z a1 a2) x y = N z (anadeHojas a1 x y) (anadeHojas a2 x y)

-- ---------------------------------------------------------------------
-- Ejercicio 3.1. Definir la función
--    preorden :: Arbol a -> [a]
-- tal que (preorden x) es la lista correspondiente al recorrido
-- preorden del árbol x; es decir, primero visita la raíz del árbol, a
-- continuación recorre el subárbol izquierdo y, finalmente, recorre el
-- subárbol derecho. Por ejemplo,
--    preorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [9,3,2,4,7]
-- ---------------------------------------------------------------------

preorden :: Arbol a -> [a]
preorden (H x) = [x]
preorden (N x a1 a2) = [x] ++ preorden a1 ++ preorden a2

-- ---------------------------------------------------------------------
-- Ejercicio 3.2. Definir la función
--    inorden :: Arbol a -> [a]
-- tal que (inorden x) es la lista correspondiente al recorrido
-- inorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación la raíz, y finalmente el subárbol derecho. 
-- Por ejemplo,
--    inorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,3,4,9,7]
-- ---------------------------------------------------------------------

inorden :: Arbol a -> [a]
inorden (H x) = [x]
inorden (N x a1 a2) = inorden a1 ++ [x] ++ inorden a2

-- ---------------------------------------------------------------------
-- Ejercicio 3.3. Definir la función
--    postorden :: Arbol a -> [a]
-- tal que (postorden x) es la lista correspondiente al recorrido
-- postorden del árbol x; es decir, primero recorre el subárbol
-- izquierdo, a continuación el subárbol derecho y, finalmente, la raíz
-- del árbol. Por ejemplo,
--    postorden (N 9 (N 3 (H 2) (H 4)) (H 7))  ==  [2,4,3,7,9]
-- ---------------------------------------------------------------------

postorden :: Arbol a -> [a]
postorden (H x) = [x]
postorden (N x a1 a2) = postorden a1 ++ postorden a2 ++ [x]

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función
--    espejo :: Arbol a -> Arbol a
-- tal que (espejo x) es la imagen especular del árbol x. Por ejemplo,
--    espejo (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 7) (N 3 (H 4) (H 2))
-- ---------------------------------------------------------------------

espejo :: Arbol a -> Arbol a
espejo (H x) = H x
espejo (N x a1 a2) = N x (espejo a2) (espejo a1)

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. La función take está definida por
--    take :: Int -> [a] -> [a]
--    take 0            = []
--    take (n+1) []     = []
--    take (n+1) (x:xs) = x : take n xs
-- 
-- Definir la función 
--    takeArbol ::  Int -> Arbol a -> Arbol a
-- tal que (takeArbol n t) es el subárbol de t de profundidad n. Por
-- ejemplo,
--    takeArbol 0 (N 9 (N 3 (H 2) (H 4)) (H 7)) == H 9
--    takeArbol 1 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (H 3) (H 7)
--    takeArbol 2 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
--    takeArbol 3 (N 9 (N 3 (H 2) (H 4)) (H 7)) == N 9 (N 3 (H 2) (H 4)) (H 7)
-- ---------------------------------------------------------------------
 
takeArbol :: Int -> Arbol a -> Arbol a
takeArbol 0 (H x) = H x
takeArbol 0 (N x a1 a2) = H x
takeArbol x (H y) = H y
takeArbol x (N y a1 a2) = N y (takeArbol (x-1) a1) (takeArbol (x-1) a2)

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. La función
--    repeat :: a -> [a]
-- está definida de forma que (repeat x) es la lista formada por
-- infinitos elementos x. Por ejemplo,
--    repeat 3  ==  [3,3,3,3,3,3,3,3,3,3,3,3,3,...
-- La definición de repeat es
--    repeat x = xs where xs = x:xs
-- 
-- Definir la función
--    repeatArbol :: a -> Arbol a
-- tal que (repeatArbol x) es es árbol con infinitos nodos x. Por
-- ejemplo, 
--    takeArbol 0 (repeatArbol 3) == H 3
--    takeArbol 1 (repeatArbol 3) == N 3 (H 3) (H 3)
--    takeArbol 2 (repeatArbol 3) == N 3 (N 3 (H 3) (H 3)) (N 3 (H 3) (H 3))
-- ---------------------------------------------------------------------

repeatArbol :: a -> Arbol a
repeatArbol x = N x (repeatArbol x) (repeatArbol x)

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. La función 
--    replicate :: Int -> a -> [a]
-- está definida por 
--    replicate n = take n . repeat
-- es tal que (replicate n x) es la lista de longitud n cuyos elementos
-- son x. Por ejemplo,
--    replicate 3 5  ==  [5,5,5]
-- 
-- Definir la función 
--    replicateArbol :: Int -> a -> Arbol a
-- tal que (replicate n x) es el árbol de profundidad n cuyos nodos son
-- x. Por ejemplo,
--    replicateArbol 0 5  ==  H 5
--    replicateArbol 1 5  ==  N 5 (H 5) (H 5)
--    replicateArbol 2 5  ==  N 5 (N 5 (H 5) (H 5)) (N 5 (H 5) (H 5))
-- ---------------------------------------------------------------------

replicateArbol :: Int -> a -> Arbol a
replicateArbol 0 y = H y
replicateArbol x y = N y (replicateArbol (x-1) y) (replicateArbol (x-1) y)

-- ---------------------------------------------------------------------
-- Ejercicio 7.1. Definir la función
--    mapArbol :: (a -> a) -> Arbol a -> Arbol a
-- tal que (mapArbol f x) es el árbol obtenido aplicándole a cada nodo de
-- x la función f. Por ejemplo,
--    ghci> mapArbol (*2) (N 9 (N 3 (H 2) (H 4)) (H 7)) 
--    N 18 (N 6 (H 4) (H 8)) (H 14)
-- ---------------------------------------------------------------------

mapArbol :: (a -> a) -> Arbol a -> Arbol a
mapArbol f (H x) = H (f x)
mapArbol f (N x a1 a2) = N (f x) (mapArbol f a1) (mapArbol f a2)

-- ---------------------------------------------------------------------
-- Ejercicio 8. Se consideran los árboles con operaciones booleanas
-- definidos por   
--    data ArbolB = HB Bool 
--                | Conj ArbolB ArbolB
--                | Disy ArbolB ArbolB
--                | Neg ArbolB
-- 
-- Por ejemplo, los árboles
--                Conj                            Conj          
--               /   \                           /   \          
--              /     \                         /     \         
--           Disy      Conj                  Disy      Conj     
--          /   \       /  \                /   \      /   \    
--       Conj    Neg   Neg True          Conj    Neg   Neg  True 
--       /  \    |     |                 /  \    |     |        
--    True False False False          True False True  False     
--
-- se definen por
--    ej1, ej2:: ArbolB
--    ej1 = Conj (Disy (Conj (HB True) (HB False))
--                     (Neg (HB False)))
--               (Conj (Neg (HB False))
--                     (HB True))
--    
--    ej2 = Conj (Disy (Conj (HB True) (HB False))
--                     (Neg (HB True)))
--               (Conj (Neg (HB False))
--                     (HB True))
-- 
-- Definir la función 
--    valorB :: ArbolB -> Bool
-- tal que (valorB ar) es el resultado de procesar el árbol realizando
-- las operaciones booleanas especificadas en los nodos. Por ejemplo,
--    valorB ej1 == True
--    valorB ej2 == False
-- ---------------------------------------------------------------------

data ArbolB = HB Bool 
            | Conj ArbolB ArbolB
            | Disy ArbolB ArbolB
            | Neg ArbolB

ej1, ej2:: ArbolB
ej1 = Conj (Disy (Conj (HB True) (HB False))
                 (Neg (HB False)))
           (Conj (Neg (HB False))
                 (HB True))

ej2 = Conj (Disy (Conj (HB True) (HB False))
                 (Neg (HB True)))
           (Conj (Neg (HB False))
                 (HB True))

valorB:: ArbolB -> Bool
valorB (Neg (HB True)) = False
valorB (Neg (HB False)) = True
valorB (HB True) = True
valorB (HB False) = False
valorB (Conj a1 a2) = (valorB a1) && (valorB a2)
valorB (Disy a1 a2) = (valorB a1) || (valorB a2)
valorB (Neg (Disy a1 a2)) = not ((valorB a1) || (valorB a2))
valorB (Neg (Conj a1 a2)) = not ((valorB a1) && (valorB a2))

-- ---------------------------------------------------------------------
-- Ejercicio 9. Los árboles generales se pueden representar mediante el
-- siguiente tipo de dato  
--    data ArbolG a = N a [ArbolG a]
--                  deriving (Eq, Show)
-- Por ejemplo, los árboles
--      1               3               3
--     / \             /|\            / | \
--    2   3           / | \          /  |  \
--        |          5  4  7        5   4   7
--        4          |     /\       |   |  / \
--                   6    2  1      6   1 2   1
--                                     / \
--                                    2   3
--                                        |
--                                        4
-- se representan por
--    ejG1, ejG2, ejG3 :: ArbolG Int
--    ejG1 = N 1 [N 2 [],N 3 [N 4 []]]
--    ejG2 = N 3 [N 5 [N 6 []], 
--               N 4 [], 
--               N 7 [N 2 [], N 1 []]]
--    ejG3 = N 3 [N 5 [N 6 []], 
--               N 4 [N 1 [N 2 [],N 3 [N 4 []]]], 
--               N 7 [N 2 [], N 1 []]]
-- 
-- Definir la función
--     ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
-- tal que (ramifica a1 a2 p) el árbol que resulta de añadir una copia
-- del árbol a2 a los nodos de a1 que cumplen un predicado p. Por
-- ejemplo, 
--    ramifica ejG1 (NG 8 []) (>4) =>  NG 1 [NG 2 [],NG 3 [NG 4 []]]
--    ramifica ejG1 (NG 8 []) (>3) =>  NG 1 [NG 2 [],NG 3 [NG 4 [NG 8 []]]]
--    ramifica ejG1 (NG 8 []) (>2) =>  NG 1 [NG 2 [],NG 3 [NG 4 [NG 8 []],NG 8 []]]
--    ramifica ejG1 (NG 8 []) (>1) =>  NG 1 [NG 2 [NG 8 []],NG 3 [NG 4 [NG 8 []],NG 8 []]]
--    ramifica ejG1 (NG 8 []) (>0) =>  NG 1 [NG 2 [NG 8 []],NG 3 [NG 4 [NG 8 []],NG 8 []],NG 8 []]
-- ---------------------------------------------------------------------

data ArbolG a = NG a [ArbolG a]
              deriving (Eq, Show)

ejG1, ejG2, ejG3 :: ArbolG Int
ejG1 = NG 1 [NG 2 [],NG 3 [NG 4 []]]
ejG2 = NG 3 [NG 5 [NG 6 []], 
           NG 4 [], 
           NG 7 [NG 2 [], NG 1 []]]
ejG3 = NG 3 [NG 5 [NG 6 []], 
           NG 4 [NG 1 [NG 2 [],NG 3 [NG 4 []]]], 
           NG 7 [NG 2 [], NG 1 []]]

ramifica :: ArbolG a -> ArbolG a -> (a -> Bool) -> ArbolG a
ramifica = undefined

-- ---------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    nHojasG :: ArbolG a -> Int
-- tal que (nHojas x) es el número de hojas del árbol x. Por ejemplo,
--    nHojasG ejG1  ==  2
--    nHojasG ejG2  ==  4
--    nHojasG ejG3  ==  5
-- ---------------------------------------------------------------------

nHojasG :: ArbolG a -> Int
nHojasG xs = nHojasGAux [xs] 

nHojasGAux :: [ArbolG a] -> Int
nHojasGAux [] = 0
nHojasGAux ((NG _ y):xs)
      | null y    = 1 + nHojasGAux xs
      | otherwise = nHojasGAux y + nHojasGAux xs

-- ---------------------------------------------------------------------
-- Ejercicio 11. Definir la función
--    profundidad :: ArbolG a -> Int
-- tal que (profundidadG x) es la profundidad del árbol x. Por ejemplo,
--    profundidadG ejG1  ==  2
--    profundidadG ejG2  ==  2
--    profundidadG ejG3  ==  4
-- ---------------------------------------------------------------------

profundidadG :: ArbolG a -> Int
profundidadG xs = profundidadGAux [xs]

profundidadGAux :: [ArbolG a] -> Int
profundidadGAux [] = 0
profundidadGAux ((NG _ y):xs)
      | null y    = nHojasGAux xs
      | otherwise = 1 + nHojasGAux y + nHojasGAux xs

-- ---------------------------------------------------------------------
-- Ejercicio 12. Definir la función
--    bin2gen :: ArbolG a -> Int
-- tal que (bin2gen x) es la traducción del árbol x definido con el tipo
-- "Arbol" (es decir, árbol binario) a "ArbolG" (es decir, árbol
-- genérico). Por ejemplo,
--    bin2gen (N 9 (N 3 (H 2) (H 4)) (H 7)) ==  (NG 9 [NG 3 [NG 2 [],NG 4 []], NG 7 []])
-- ---------------------------------------------------------------------

bin2gen :: Arbol a -> ArbolG a
bin2gen = undefined
