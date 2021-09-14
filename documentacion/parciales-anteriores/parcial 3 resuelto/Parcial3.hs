{-# LANGUAGE OverloadedStrings #-}

import Data.Matrix
import qualified Data.Vector as V
import CodeWorld
import Control.Parallel.Strategies

-- ----------------------------------------------------------------------
-- Ejercicio 1. 
-- ----------------------------------------------------------------------
-- La serie de Thue-Morse comienza con el término [0] y sus siguientes
-- términos se construyen añadiéndole al anterior su complementario. Los
-- primeros términos de la serie son:
--   [0]                   (es el primer término)
--   [0,1]                 (es [0] concatenado con su complementario [1])
--   [0,1,1,0]             (es [0,1] ++ [1,0])
--   [0,1,1,0,1,0,0,1]     (es [0,1,1,0] ++ [1,0,0,1])
--   [0,1,1,0,1,0,0,1,1,0,0,1,0,1,1,0] ...
-- ----------------------------------------------------------------------

-- Ejercicio 1.1. Define la lista
--    serieThueMorse :: [[Integer]]
-- tal que sus elementos son los términos de la serie de Thue-Morse. Por
-- ejemplo,
--   λ> take 4 serieThueMorse == [[0],[0,1],[0,1,1,0],[0,1,1,0,1,0,0,1]]

complementaria :: [Integer] -> [Integer]
complementaria = map (1-)

serieThueMorse :: [[Integer]]
serieThueMorse = iterate paso [0]
    where paso xs = xs ++ complementaria xs

-- Ejercicio 1.2. Cada término de la serie de Thue-Morse se puede obtener
-- del anterior sustituyendo los 1 por 1,0 y los 0 por 0,1. Define la
-- lista de los términos de la serie Thue-Morse
--   serieThueMorse2 :: [[Integer]]
-- empleando este método y usando plegado (foldr o foldl).

serieThueMorse2 :: [[Integer]]
serieThueMorse2 = iterate paso [0]
    where paso = foldr f []
          f x ys | x == 0 = [0,1] ++ ys
                 | x == 1 = [1,0] ++ ys
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2. 
-- ----------------------------------------------------------------------
-- Una matriz cuadrada se puede descomponer en cuatro sectores (o
-- submatrices de mismo tamaño) como sigue:
--                              +---+---+
--                              | 1 | 4 |        
--                              +---+---+
--                              | 2 | 3 |        
--                              +---+---+
-- Por ejemplo, en matrizEj2, el sector 1 es (fromLists [[1,2],[3,4]]),
-- el 2 es (fromLists [[9,10],[1,2]]), etc. El sector 1 de matrizEj1 es
-- (fromLists [[1]]), el sector 2 es (fromLists [[2]]), etc.

matrizEj1,matrizEj2 :: Matrix Int
matrizEj1 = fromLists [[1, 4],
                       [2, 3]]
matrizEj2 = fromLists [[1, 2, 5, 6],
                       [3, 4, 7, 8],
                       [9,10, 0, 1],
                       [1, 2, 2, 0]]

-- Por otro lado, Vamos a definir un árbol cuaternario (o quadtree) con
-- el siguiente tipo de dato argebráico:

data ArbolQ a = H a | N a (ArbolQ a) (ArbolQ a) (ArbolQ a) (ArbolQ a)
  deriving (Show,Eq)

-- Vamos a construir el quadtree para almacenar los máximos por sectores
-- de forma jerárquica, de tal forma que: la raiz será el máximo de la
-- matriz, el primer hijo el máximo del primer sector, el segundo hijo
-- el máximo del segundo sector, el tercer hijo el máximo del tercer
-- sector y el cuarto hijo el máximo del cuarto sector. El proceso se
-- repite para cada hijo hasta que el sector contenga tan solo un
-- elemento. Por simplicidad, asume que las dimensiones de la matriz son
-- potencias de dos.
-- ----------------------------------------------------------------------

-- Define la función (quadTree m) tal que devuelva el quadtree de la
-- matriz m, indicando un tipado lo más genérico posible para la función.
-- Por ejemplo,
--   λ> quadTree matrizEj1 == N 4 (H 1) (H 2) (H 3) (H 4)
--   λ> quadTree matrizEj2 == N 10 (N 4 (H 1) (H 3) (H 4) (H 2))
--                                 (N 10 (H 9) (H 1) (H 2) (H 10))
--                                 (N 2 (H 0) (H 2) (H 0) (H 1))
--                                 (N 8 (H 5) (H 7) (H 8) (H 6))

quadTree = Ord a => Matrix a -> ArbolQ a
quadTree m 
  | nf == 1 || nc == 1 = H v
  | otherwise = N v (quadTree matrizq1)
                    (quadTree matrizq2)
                    (quadTree matrizq3)
                    (quadTree matrizq4)
  where nf = nrows m 
        nc = ncols m 
        v = maximum $ toList m 
        matrizq1 = submatrix 1 (div nf 2) 1 (div nc 2) m
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 3. 
-- ----------------------------------------------------------------------
-- Construye un pequeño juego en CodeWorld donde hayan una circunferencia
-- estática y un circulo que el usuario pueda mover con el teclado. Por
-- defecto su color es negro, pero cuando el círculo esté dentro de la
-- circunferencia, ambos cambian su color a rojo. Se adjuntan en esta
-- carpeta dos capturas de pantalla mostrando cómo debe quedar.
-- ----------------------------------------------------------------------

-- Ejercicio 3.1. Declararemos los círculos por sus coordenadas (x,y) de
-- su centro y su radio r.
-- a) Define el tipo de dato Circulo con dos constructores: C y C1.
-- C debe tener asociado tres Double (que se usarán para x, y, r).
-- C1 solo tiene asociado dos valores Double (se usarán para x e y,
-- y se asume que el radio es r=1). Indicación: observa los ejemplos del
-- ejercicio 3.2.
-- b) Define también el tipo de dato sinónimo Estado, tal que sea una
-- tupla que contenga dos círculos y un booleano. Éste último usará para
-- indicar si el primero está contenido en el segundo.

data Circulo = C Double Double Double | C1 Double Double
data Estado = (Circulo, Circulo, Bool)


-- Ejercicio 3.2. Define la función (estaDentro c1 c2), tal que dados dos
-- círculos c1 y c2, indique si el primero está dentro del segundo.
-- Indicación: basta comparar el radio del segundo circulo con la suma
-- de las diferencias al cuadrado de las coordenadas x e y. Por ejemplo,
-- λ> estaDentro (C1 2.0 2.5) (C 5.0 5.0 2.5) == False
-- λ> estaDentro (C1 4.5 4.5) (C 5.0 5.0 2.5) == True

estaDentro :: Circulo -> Circulo -> Bool
estaDentro (C1 x y) (C i j r) = sqrt((x-i)**2 + (y-j)**2) <= r

-- Ejercicio 3.3. Define en CodeWorld el juego propuesto empleando los
-- tipos y funciones definidas anteriormente. El circulo debe moverse con
-- las flechas en el teclado, y en cada movimiento debe moverse 0.5 en el
-- eje de coordenadas. El circulo debe tener siempre radio 1 y comenzar
-- en (0,0). La circunferencia estática debe comenzar en la posición
-- (x,y) y tener radio r, según se indique en la llamada a la función
-- (juego x y r). Se puede usar la función main comentada abajo.

dch, izq, arr, abj :: Circulo -> Circulo
dch (C1 x y) = C1 (x+0.5) y
izq (C1 x y) = C1 (x-0.5) y
arr (C1 x y) = C1 x (y+0.5) 
dch (C1 x y) = C1 x (y-0.5) 

manejaEvento :: Event -> Estado -> Estado
manejaEvento (KeyPress "Right")(c.d._) = (dch c,d,estadentro (dch c) d)

juego :: Double -> Double -> Double -> IO ()
juego x y r = undefined

--main = juego 5 5 2.5

-- Ejercicio 3.4 Modifica el main anterior para que solicite por pantalla
-- las coordenadas x y, y el radio r antes de llamar a la función juego.
-- Por ejemplo,
--  λ> main
--  Introduzca x: 5
--  Introduzca y: 5
--  Introduzca r: 2.5
--  Open me on http://127.0.0.1:3000/
  
--main = undefined        

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 4.
-- ----------------------------------------------------------------------
-- En los gráficos en 3D, la triangulación de polígonos es una operación
-- crítica. Una triangulación de un polígono es una división del área
-- en un conjunto de triángulos, de forma que la unión de todos ellos es
-- igual al polígono original, y cualquier par de triángulos es
-- disjunto o comparte únicamente un vértice o un lado. En el caso de
-- polígonos convexos, la cantidad de triangulaciones posibles depende
-- únicamente del número de vértices del polígono.
-- 
-- Si llamamos T(n) al número de triangulaciones de un polígono de n
-- vértices, se verifica la siguiente relación de recurrencia:
--     T(2) = 1
--     T(n) = T(2)*T(n-1) + T(3)*T(n-2) + ... + T(n-1)*T(2)
-- 
-- Define la función
--    numeroTriangulaciones :: Int -> Integer
-- tal que (numeroTriangulaciones n) es el número de triangulaciones de
-- un polígono convexo de n vértices. Se piden tres implementaciones:
-- mediante recursión, programación dinámica y paralelismo. Por ejemplo,
--    numeroTriangulaciones 3  == 1
--    numeroTriangulaciones 5  == 5
--    numeroTriangulaciones 6  == 14
--    numeroTriangulaciones 7  == 42
--    numeroTriangulaciones 50 == 131327898242169365477991900
--    numeroTriangulaciones 100
--      ==  57743358069601357782187700608042856334020731624756611000
-- ----------------------------------------------------------------------

-- Ejercicio 4.1. Definir por recursión y/o listas por comprensión
numeroTriangulacionesR :: Int -> Integer
numeroTriangulacionesR = undefined

-- Ejercicio 4.2. Definir por programación dinámica

numeroTriangulacionesPD :: Int -> Integer
numeroTriangulacionesPD = undefined      

-- Ejercicio 4.3. Definir con paralelismo mediante la mónada eval, usando
-- la versión recursiva (ejercicio 4.1). Después, usando la función main
-- definida abajo, compila el programa y calcula la acelaración conseguida
-- usando dos procesadores.
-- Indicación: puedes usar la versión paralela del map siguiente

parMap' :: (a -> b) -> [a] -> Eval [b]
parMap' f [] = return []
parMap' f (a:as) = do
        b <- rpar (f a)
        bs <- parMap' f as
        return (b:bs)
        
numeroTriangulacionesP :: Int -> Integer
numeroTriangulacionesP = undefined

--main = print $ numeroTriangulacionesP 30

{- 
Tiempo 1 procesador:
Tiempo 2 procesadores:
Aceleración: 
-}

-- ----------------------------------------------------------------------
