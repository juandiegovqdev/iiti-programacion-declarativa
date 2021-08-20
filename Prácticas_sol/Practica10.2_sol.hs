-- PD 2020-21. Vectores y matrices: ejercicios de exámenes. Solución.
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presenta una recopilación de ejercicios vectores
-- y matrices propuestos en exámenes de la asignatura I1M.

-- ---------------------------------------------------------------------
-- § Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Data.Array
import Data.List

-- Nota. En la relación usaremos los tipos de los vectores y las matrices 
-- definidos por 

type Vector a = Array Int a
type Matriz a = Array (Int,Int) a

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    esTriangularS :: Num a => Matriz a -> Bool
-- tal que (esTriangularS p) se verifica si p es una matriz triangular
-- superior. Por ejemplo, 
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,1,0,4,7,0,0,5])
--    True
--    ghci> esTriangularS (listArray ((1,1),(3,3)) [1,2,3,1,2,4,1,2,5])
--    False
-- ---------------------------------------------------------------------

esTriangularS:: (Num a, Eq a) => Matriz a -> Bool
esTriangularS p = and [p!(i,j) == 0 | i <- [1..m], j <- [1..n], i > j]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función
--    antidiagonal :: (Num a, Eq a) => Matriz a -> Bool
-- tal que (antidiagonal m) se verifica si es cuadrada y todos los
-- elementos de m que no están en su diagonal secundaria son nulos. Por
-- ejemplo,   
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0])
--    True
--    ghci> antidiagonal (listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5])
--    False
-- ---------------------------------------------------------------------

-- m1, m2 :: Matriz Int
-- m1 = listArray ((1,1),(3,3)) [0,0,4, 0,6,0, 0,0,0]
-- m2 = listArray ((1,1),(3,3)) [7,0,4, 0,6,0, 0,0,5]

antidiagonal :: (Num a, Eq a) => Matriz a -> Bool 
antidiagonal p = 
    m == n && all (==0) [p!(i,j) | i <- [1..n], j <- [1..n], j /= n+1-i]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función
--    esEscalar:: Num a => Matriz a -> Bool
-- tal que (esEscalar p) se verifica si p es una matriz es escalar; es
-- decir, diagonal con todos los elementos de la diagonal principal
-- iguales. Por ejemplo,
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,5,0,0,0,5])  ==  True
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
--    esEscalar (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  False
-- ---------------------------------------------------------------------

esEscalar:: (Num a, Eq a) => Matriz a -> Bool
esEscalar p = esDiagonal p && todosIguales (elems (diagonalPral p))

-- (esDiagonal p) se verifica si la matriz p es diagonal. Por ejemplo.
--    esDiagonal (listArray ((1,1),(3,3)) [5,0,0,0,6,0,0,0,5])  ==  True
--    esDiagonal (listArray ((1,1),(3,3)) [5,0,0,1,5,0,0,0,5])  ==  False
esDiagonal:: (Num a, Eq a) => Matriz a -> Bool
esDiagonal p = all (==0) [p!(i,j) | i<-[1..m],j<-[1..n], i/=j]
    where (_,(m,n)) = bounds p

-- (todosIguales xs) se verifica si todos los elementos de xs son
-- iguales. Por ejemplo,
--    todosIguales [5,5,5]  ==  True
--    todosIguales [5,6,5]  ==  False
todosIguales :: Eq a => [a] -> Bool
todosIguales (x:y:ys) = x == y && todosIguales (y:ys) 
todosIguales _ = True 

-- (diagonalPral p) es la diagonal principal de la matriz p. Por
-- ejemplo, 
--    ghci> diagonalPral (listArray ((1,1),(3,3)) [5,0,0,1,6,0,0,2,4])
--    array (1,3) [(1,5),(2,6),(3,4)]
diagonalPral :: Num a => Matriz a -> Vector a
diagonalPral p = array (1,n) [(i,p!(i,i)) | i <- [1..n]]
    where n         = min k l
          (_,(k,l)) = bounds p


-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función 
--    aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
-- tal que (aplicaT t f) es la tabla obtenida aplicado la función f a
-- los elementos de la tabla t. Por ejemplo,
--    ghci> aplicaT (array (1,5) [(1,6),(2,3),(3,-1),(4,9),(5,20)]) (+1)
--    array (1,5) [(1,7),(2,4),(3,0),(4,10),(5,21)]
--    ghci> :{
--    *Main| aplicaT (array ((1,1),(2,3)) [((1,1),3),((1,2),-1),((1,3),0),
--    *Main|                               ((2,1),0),((2,2),0),((2,3),-1)])
--    *Main|         (*2)
--    *Main| :}
--    array ((1,1),(2,3)) [((1,1),6),((1,2),-2),((1,3),0),
--                         ((2,1),0),((2,2),0),((2,3),-2)]
-- ---------------------------------------------------------------------

aplicaT :: (Ix a, Num b) => Array a b -> (b -> c) -> Array a c
aplicaT t f = listArray (bounds t) (map f (elems t))


-- ---------------------------------------------------------------------
-- Ejercicio 5. Dada una matriz numérica A de dimensiones (m,n) y una
-- matriz booleana B de las mismas dimensiones, y dos funciones f y g,
-- la transformada de A respecto de B, f y g es la matriz C (de las
-- mismas dimensiones), tal que, para cada celda (i,j):   
--    C(i,j) = f(A(i,j)) si B(i,j) es verdadero
--    C(i,j) = f(A(i,j)) si B(i,j) es falso
-- Por ejemplo, si A y B son las matrices
--    |1 2|   |True  False|  
--    |3 4|   |False True |
-- respectivamente, y f y g son dos funciones tales que f(x) = x+1 y
-- g(x) = 2*x, entonces la transformada de A respecto de B, f y g es
--    |2 4|
--    |6 5|
--     
-- Definir la función
--    transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
-- tal que (transformada a b f g) es la transformada de A respecto de B,
-- f y g. Por ejemplo,
--  ghci> let a = listArray ((1,1),(2,2)) [1,2,3,4] :: Matriz Int
--  ghci> let b = listArray ((1,1),(2,2)) [True,False,False,True] :: Matriz Bool
--  ghci> transformada a b (+1) (*2)
--  array ((1,1),(2,2)) [((1,1),2),((1,2),4),((2,1),6),((2,2),5)]
-- ---------------------------------------------------------------------

transformada :: Matriz a -> Matriz Bool -> (a -> b) -> (a -> b) -> Matriz b
transformada a b f g = 
    array ((1,1),(m,n)) [((i,j),aplica i j) | i <- [1..m], j <- [1..m]]
    where (m,n) = snd (bounds a)
          aplica i j | b!(i,j)   = f (a!(i,j))
                     | otherwise = g (a!(i,j))

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Un vector se denomina estocástico si todos sus
-- elementos son mayores o iguales que 0 y suman 1.  
-- 
-- Definir la función 
--    vectorEstocastico :: Vector Float -> Bool
-- tal que (vectorEstocastico v) se verifica si v es estocástico. Por
-- ejemplo,  
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.7]) == True
--    vectorEstocastico (listArray (1,5) [0.1, 0.2, 0, 0, 0.9]) == False
-- ---------------------------------------------------------------------

vectorEstocastico :: Vector Float -> Bool
vectorEstocastico v = all (>=0) xs && sum xs == 1
    where xs = elems v

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Una matriz se denomina estocástica si sus columnas
-- son vectores estocásticos.  
-- 
-- Definir la función 
--    matrizEstocastica :: Matriz Float -> Bool
-- tal que (matrizEstocastico p) se verifica si p es estocástica. Por
-- ejemplo,  
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.9,0.8]) == True
--    matrizEstocastica (listArray ((1,1),(2,2)) [0.1,0.2,0.3,0.8]) == False
-- ---------------------------------------------------------------------

matrizEstocastica :: Matriz Float -> Bool        
matrizEstocastica p = all vectorEstocastico (columnas p)

-- (columnas p) es la lista de las columnas de la matriz p. Por ejemplo, 
--    ghci> columnas (listArray ((1,1),(2,3)) [1..6])
--    [array (1,2) [(1,1.0),(2,4.0)],
--     array (1,2) [(1,2.0),(2,5.0)],
--     array (1,2) [(1,3.0),(2,6.0)]]
--    ghci> columnas (listArray ((1,1),(3,2)) [1..6])
--    [array (1,3) [(1,1.0),(2,3.0),(3,5.0)],
--     array (1,3) [(1,2.0),(2,4.0),(3,6.0)]]
columnas :: Matriz Float -> [Vector Float]
columnas p = 
    [array (1,m) [(i,p!(i,j)) | i <- [1..m]] | j <- [1..n]]
    where (_,(m,n)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    maximos :: Matriz Int -> [Int]
-- tal que (maximos p) es la lista de los máximos locales de la matriz
-- p; es decir de los elementos de p que son mayores que todos sus
-- vecinos. Por ejemplo,  
--    ghci> maximos (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,0,2,5,4])
--    [9,7]
-- ya que los máximos locales de la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |0 2 5 4|
-- son 9 y 7.
-- ---------------------------------------------------------------------

maximos :: Matriz Int -> [Int]
maximos p = 
    [p!(i,j) | (i,j) <- indices p,
               and [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]


-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    algunMenor :: Matriz Int -> [Int]
-- tal que (algunMenor p) es la lista de los elementos de p que tienen
-- algún vecino menor que él. Por ejemplo,  
--    algunMenor (listArray ((1,1),(3,4)) [9,4,6,5,8,1,7,3,4,2,5,4])
--    [9,4,6,5,8,7,4,2,5,4]          
-- pues sólo el 1 y el 3 no tienen ningún vecino menor en la matriz
--    |9 4 6 5|
--    |8 1 7 3|
--    |4 2 5 4|
-- ---------------------------------------------------------------------        

algunMenor :: Matriz Int -> [Int]
algunMenor p = 
    [p!(i,j) | (i,j) <- indices p,
               or [p!(a,b) < p!(i,j) | (a,b) <- vecinos (i,j)]] 
    where (_,(m,n)) = bounds p
          vecinos (i,j) = [(a,b) | a <- [max 1 (i-1)..min m (i+1)],
                                   b <- [max 1 (j-1)..min n (j+1)],
                                   (a,b) /= (i,j)]

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función
--      proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
-- tal que (proporcional v1 v2) verifica si los vectores v1 y v2 son proporcionales, 
-- es decir, v1 == k*v2, con un k un número escalar cualquiera. Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    proporcional v1 v1                           = True
--    proporcional v1 v2                           = False
--    proporcional v1 (listArray (1,3) [0,-5,5])   = True
--    proporcional v1 (listArray (1,3) [0,-5,4])   = False
--    proporcional (listArray (1,3) [0,-5,5]) v1   = True
--    proporcional v1 (listArray (1,3) [0,0,0])    = True
--    proporcional (listArray (1,3) [0,0,0]) v1    = False
-- ---------------------------------------------------------------------

proporcional :: (Fractional a, Eq a) => Vector a -> Vector a -> Bool
proporcional v1 v2 
    | esCero v1 = esCero v2
    | otherwise = and [v2!i == k*(v1!i) | i <- [1..n]]
    where (_,n) = bounds v1
          j     = minimum [i | i <- [1..n], v1!i /= 0]
          k     = (v2!j) / (v1!j)

-- (esCero v) se verifica si v es el vector 0.
esCero :: (Fractional a, Eq a) => Vector a -> Bool 
esCero v = null [x | x <- elems v, x /= 0]

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir la función
--    esAutovector :: (Fractional a, Eq a) => 
--                    Vector a -> Matriz a -> Bool
-- tal que (esAutovector v p) compruebe si v es un autovector de p
-- (es decir, el producto de v por p es un vector proporcional a
-- v). Por ejemplo, 
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    ghci> esAutovector v1 p1 
--    True
--    ghci> esAutovector v2 p1 
--    False
-- ---------------------------------------------------------------------

esAutovector :: (Fractional a, Eq a) => Vector a -> Matriz a -> Bool
esAutovector v p = proporcional (producto p v) v

-- (producto p v) es el producto de la matriz p por el vector v. Por
-- ejemplo, 
--    producto p1 v1  = array (1,3) [(1,0.0),(2,1.0),(3,-1.0)]
--    producto p1 v2  = array (1,3) [(1,1.0),(2,1.0),(3,2.0)]
producto :: (Fractional a, Eq a) => Matriz a -> Vector a -> Vector a 
producto p v =
    array (1,n) [(i, sum [p!(i,j)*v!j | j <- [1..n]]) | i <- [1..m]]
    where (_,n)     = bounds v
          (_,(m,_)) = bounds p

-- ---------------------------------------------------------------------
-- Ejercicio 9.3. Definir la función
--    autovalorAsociado :: (Fractional a, Eq a) => 
--                         Matriz a -> Vector a -> Maybe a
-- tal que si v es un autovector de p, calcule el autovalor asociado.
-- Por ejemplo,
--    ghci> let p1 = listArray ((1,1),(3,3)) [1,0,0,0,0,1,0,1,0] :: Matriz Float
--    ghci> let v1 = listArray (1,3) [0,-1,1] :: Vector Float
--    ghci> let v2 = listArray (1,3) [1,2,1] :: Vector Float
--    autovalorAsociado p1 v1 == Just (-1.0)
--    autovalorAsociado p1 v2 == Nothing
-- ---------------------------------------------------------------------

autovalorAsociado :: (Fractional a, Eq a) => 
                     Matriz a -> Vector a -> Maybe a
autovalorAsociado p v
    | esAutovector v p = Just (producto p v ! j / v ! j)
    | otherwise        = Nothing
    where (_,n) = bounds v
          j     = minimum [i | i <- [1..n], v!i /= 0]

-- ------------------------------------------------------------------
-- Ejercicio 10. Definir la función
--    sumaVecinos :: Matriz Int -> Matriz Int
-- tal que (sumaVecinos p) es la matriz obtenida al escribir en la 
-- posicion (i,j) la suma de los todos vecinos del elemento que ocupa 
-- el lugar (i,j) en la matriz p. Por ejemplo,
--    ghci> sumaVecinos (listArray ((1,1),(3,3)) [0,1,3, 1,2,0, 0,5,7])
--    array ((1,1),(3,3)) [((1,1),4),((1,2), 6),((1,3), 3),
--                         ((2,1),8),((2,2),17),((2,3),18),
--                         ((3,1),8),((3,2),10),((3,3), 7)]
-- ------------------------------------------------------------------

sumaVecinos :: Matriz Int -> Matriz Int
sumaVecinos p =  
    array ((1,1),(m,n)) 
          [((i,j), f i j) | i <- [1..m], j <- [1..n]] 
    where (_,(m,n)) = bounds p
          f i j = sum [p!(i+a,j+b) | a <- [-1..1], b <- [-1..1], 
                                     a /= 0 || b /= 0,
                                     inRange (bounds p) (i+a,j+b)]

-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Una matriz tridiagonal es aquella en la que sólo hay
-- elementos distintos de 0 en la diagonal principal o en las diagonales
-- por encima y por debajo de la diagonal principal. Por ejemplo, 
--    ( 1 2 0 0 0 0 )
--    ( 3 4 5 0 0 0 )
--    ( 0 6 7 8 0 0 )
--    ( 0 0 9 1 2 0 )
--    ( 0 0 0 3 4 5 )
--    ( 0 0 0 0 6 7 )
-- 
-- Definir la función 
--    creaTridiagonal :: Int -> Matriz Int
-- tal que (creaTridiagonal n) es la siguiente matriz tridiagonal
-- cuadrada con n filas y n columnas:
--    ( 1 1 0 0 0 0 ... 0  0  )
--    ( 1 2 2 0 0 0 ... 0  0  )
--    ( 0 2 3 3 0 0 ... 0  0  )
--    ( 0 0 3 4 4 0 ... 0  0  )
--    ( 0 0 0 4 5 5 ... 0  0  )
--    ( 0 0 0 0 5 6 ... 0  0  )
--    ( ..................... )
--    ( 0 0 0 0 0 0 ... n  n  )
--    ( 0 0 0 0 0 0 ... n n+1 )
-- Por ejemplo,
--    ghci> creaTridiagonal 4
--    array ((1,1),(4,4)) [((1,1),1),((1,2),1),((1,3),0),((1,4),0),
--                         ((2,1),1),((2,2),2),((2,3),2),((2,4),0),
--                         ((3,1),0),((3,2),2),((3,3),3),((3,4),3),
--                         ((4,1),0),((4,2),0),((4,3),3),((4,4),4)]
-- ----------------------------------------------------------------------------

creaTridiagonal :: Int -> Matriz Int
creaTridiagonal n =
    array ((1,1),(n,n))
          [((i,j),valores i j) | i <- [1..n], j <- [1..n]]
    where valores i j | i == j     = i
                      | i == j+1   = j
                      | i+1 == j   = i
                      | otherwise  = 0

-- ----------------------------------------------------------------------------
-- Ejercicio 11.2. Definir la función 
--    esTridiagonal :: Matriz Int -> Bool
-- tal que (esTridiagonal p) se verifica si la matriz p es tridiagonal. Por 
-- ejemplo,
--    esTridiagonal (creaTridiagonal 5)               ==  True
--    esTridiagonal (listArray ((1,1),(3,3)) [1..9])  ==  False
-- ----------------------------------------------------------------------------

esTridiagonal :: Matriz Int -> Bool
esTridiagonal p =
    and [p!(i,j) == 0 | i <- [1..m], j <- [1..n], (j < i-1 || j > i+1)]
    where (_,(m,n)) = bounds p