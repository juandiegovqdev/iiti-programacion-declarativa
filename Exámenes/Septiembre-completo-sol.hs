-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Examen Septiembre                          12 de Septiembre de 2019
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · 1. Antes de continuar, cambie el nombre de este archivo por:
--                   Septiembre_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · 2. Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · 3. Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
--   4. Se recomienda entregar un fichero que cargue correctamente,
--   dejando comentado todo código con errores.
-- -------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

import TADPila
import CodeWorld
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import Text.CSV
import Data.Matrix

-- ---------------------------------------------------------------------
-- Ejercicio 1. [2 ptos]
-- -------------------------------------------------------------------
-- Se considera la función multFuncPrimerosNValidos
-- :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
-- tal que (multFuncPrimerosNValidos n f p xs) devuelve el producto de los
-- resultados de aplicar la función f a los primeros n elementos de xs
-- que cumplen el predicado p.
--
-- Por ejemplo:
--    multFuncPrimerosNValidos_1 2 (4+) even [1..7]  => 48
-- 
-- (Los dos primeros números pares de [1..7] son 2 y 4,
--  que al aplicarle (4+) quedan como 6 y 8, cuyo producto es 48)

-- Se pide definir la función:
-- 1. usando map y filter,
-- 2. por recursión,
-- 3. por recursión con acumulador,
-- 4. por plegado (a izquierda o derecha).
-- ---------------------------------------------------------------------


-- La definición con lista de comprensión (no pedida en el ejercicio) es
multFuncPrimerosNValidos_0 :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidos_0 n f p xs = product (take n [f x | x <- xs, p x])
 
-- La definición con map y filter es
multFuncPrimerosNValidos_1 :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidos_1 n f p xs = product (take n (map f (filter p xs)))
 
-- La definición por recursión es
multFuncPrimerosNValidos_2 :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidos_2 0 _ _ _ = 1
multFuncPrimerosNValidos_2 _ _ _ [] = 1
multFuncPrimerosNValidos_2 n f p (x:xs)
  | p x = f x * multFuncPrimerosNValidos_2 (n-1) f p xs
  | otherwise = multFuncPrimerosNValidos_2 n f p xs
 
-- La definición por plegado es
multFuncPrimerosNValidos_3 :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidos_3 n f p xs = foldr (\x y -> f x * y) 1 (take n (filter p xs))

-- La definición por acumulador es
multFuncPrimerosNValidos_4 :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidos_4 n f p xs = aux 1 (zip xs (replicate (length xs) n))
  where aux acc [] = acc
        aux acc ((x,y):xs) = aux nacc nxs
          where cumple = y > 0 && p x
                nacc = if cumple then (acc*f x) else acc
                nxs = if cumple then [(x,y-1) | (x,_)<- xs] else xs

-- La definición por plegado a la izquierda (no pedida, si ya teníamos foldr) es
multFuncPrimerosNValidos_5 :: (Num a, Num b) => Int -> (a -> b) -> (a -> Bool) -> [a] -> b
multFuncPrimerosNValidos_5 n f p xs = foldl (\acc x -> acc * f x) 1 (take n (filter p xs))

-- -------------------------------------------------------------------
-- Ejercicio 2. [1 pto]
-- -------------------------------------------------------------------
-- Desarrolle una animación usando CodeWorld, de modo que la escena
-- incluya los ejes de coordenadas, un rectángulo inmóvil con grosor,
-- y un círculo relleno de otro color que vaya girando alrededor
-- del rectángulo estático.
-- -------------------------------------------------------------------

anima = animationOf escena

escena :: Double -> Picture
escena t = cuadradoMovil t & fondo & ejes
-- Se ha decidido mantener los ejes de coordenadas

tam = 10

tamCuad :: Int
tamCuad = 1

fondo :: Picture
fondo = coloured azure $ cuadrado (2*(tam-3))

ejes :: Picture
ejes = coordinatePlane

cuadrado :: Double -> Picture
cuadrado n = coloured azure (thickRectangle 0.5 n n)

caminoCircular :: Picture -> Double -> Picture
caminoCircular d a =
  rotated a (translated 6 0 (rotated (-a) d))
  
cuadradoMovil :: Double -> Picture
cuadradoMovil t = caminoCircular (solidCircle 2) ((pi/3) * 0 + t)


-----------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]
-- -------------------------------------------------------------------
-- Desarrolle un programa principal que lea del archivo pasado
-- como argumento (si no se le pasa ninguno, de "atp_players.csv"),
-- lo parsee y posteriormente realice lo siguiente con las filas válidas
-- del fichero:

-- a) Imprimir por pantalla el número de jugadores de que consta el archivo.
--    A continuación, imprimalos nombres de los campos contenidos en el
--    archivo, de la siguiente forma:

--    "ID" (campo 1)
--    "nombre" (campo 2)
--    ...

-- b) Procesar los registros contenidos, de forma quevaya seleccionando
--    aquellos que sean españoles (ESP), zurdos (L) y nacidos en los 80,
--    y para cada uno de ellos imprima el nombre, apellidos y
--    fecha de nacimiento.

-----------------------------------------------------------------------

jugadores :: IO ()
jugadores = do
  args <- getArgs
  let filename = if null args then "atp_players.csv" else args!!0
  contents <- readFile filename

  let csv = parseCSV filename contents
      filas = case csv of
        (Left _) -> []
        (Right lineas) -> lineas
      filasValidas = filter (\x -> length x == 6) filas

  putStrLn $ "Hay " ++ show (length filasValidas) ++ " jugadores"
 
  procesaCabecera (head filasValidas)
  procesaContenido (tail filasValidas)

pasaALista :: Field -> [String]
pasaALista cadena = read cadena::[String]

procesaCabecera cab = do
  putStrLn $ "Para cada jugador, disponemos de los siguientes datos: "
  mapM_ procesaCampo (zip cab [1..])
    where procesaCampo (campo,pos) = putStrLn $ show pos ++ ": " ++ show campo

procesaCabecera' cab = do
  mapM_ procesaCampo (zip cab [1..])
    where procesaCampo (campo,pos) = putStrLn $ show pos ++ ": " ++ show campo
                        
procesaContenido csv = do
  putStrLn "El contenido es el siguiente:"
  let esp = filtraEsp csv
      zur = filtraZur esp
      och = filtraOch zur
  mapM_ procesaReg (take 20 och)
    where
      filtraEsp = filter (\[_,_,_,_,_,nac] -> nac == "ESP")
      filtraZur = filter (\[_,_,_,m,_,_] -> m == "L")
      filtraOch = filter (\[_,_,_,_,a,_] -> esOchenta (read (take 4 a) :: Int))
        where esOchenta a = a>= 1980 && a < 1990
              
      procesaReg [_,n,a,m,f,nac] = do
            putStrLn $ n ++ " " ++ a
            
procesaContenido' csv = do
  mapM_ procesaReg (take 20 csv)
    where procesaReg [y,make,model,bstyles] = do
            putStrLn $ show (length (pasaALista bstyles))


-- -------------------------------------------------------------------
-- Ejercicio 4. [1,5 ptos]
-- -------------------------------------------------------------------
-- La sucesión generalizada de Fibonacci de grado N
-- (N >= 1) se construye comenzando con el número 1 y calculando el
-- resto de términos como la suma de los N términos anteriores (si
-- existen). Por ejemplo,
-- + la sucesión generalizada de Fibonacci de grado 2 es:
--   1, 1, 2, 3, 5, 8, 13, 21, 34, 55
-- + la sucesión generalizada de Fibonacci de grado 4 es:
--   1, 1, 2, 4, 8, 15, 29, 56, 108, 208
-- + la sucesión generalizada de Fibonacci de grado 6 es:
--   1, 1, 2, 4, 8, 16, 32, 63, 125, 248
-- Defina la función (fibPila n k), que devuelve una pila con los
-- términos de la sucesión de Fibonacci de grado n menores que k.
-- Se pide usar sólo el TAD de Pila (no se permite el uso de listas).
-- Por ejemplo:
-- λ> fibPila 6 100
--   63|32|16|8|4|2|1|1|-
-- λ> fibPila 6 200
--   125|63|32|16|8|4|2|1|1|-
-- λ> fibPila 4 200
--   108|56|29|15|8|4|2|1|1|-

fibPila :: Int -> Int -> Pila Int
fibPila n k = fibPilaN (apila 1 (apila 1 vacia))
  where fibPilaN p | (sumaCima n p 0) > k = p
                   | otherwise = fibPilaN (apila (sumaCima n p 0) p)
        sumaCima n p m | (esVacia p) || (n==0) = m
                       | otherwise = sumaCima (n-1) (desapila p) (m + (cima p)) 
                       

-- ---------------------------------------------------------------------
-- Ejercicio 5. [1,5 ptos]
-- ---------------------------------------------------------------------
-- Dada la siguiente definición de árbol mediante listas 

data Arbol = N Int [Arbol]
  deriving (Eq, Show)

-- defina la función (aumentaNiveles a), tal que expanda el árbol a
-- añadiendo un nuevo hijo a cada nodo, cuyo valor es el resultado de
-- la suma de sus hermanos más el padre. A continuación se muestranPor
-- dos ejemplos, visualmente y evaluados en código:
--
-- Ej.1:    1         1          Ej.2:          1             1
--          |  ==>   / \                       / \    ==>    /|\
--          2       2   3                     2   3         2 3 6
--                  |                             |         | |\
--                  2                             4         2 4 7
--                                                            |
--                                                            4
-- Ej.1: λ> aumentaNiveles (N 1 [N 2 []])
--  N 1 [N 2 [N 2 []],N 3 []]
-- Ej.2: λ> aumentaNiveles (N 1 [N 2 [], N 3 [N 4 []]])
--   N 1 [N 2 [N 2 []],N 3 [N 4 [N 4 []],N 7 []],N 6 []]

valor (N x _) = x

aumentaNiveles (N x as) = N x ([ aumentaNiveles a | a <- as ]
                            ++ [N (x+sum [ valor a | a <- as ]) [] ])

-- ---------------------------------------------------------------------
-- Ejercicio 6. [1 pto]
-- ---------------------------------------------------------------------
-- Definir las siguientes funciones y tipos de datos:
---   * Definir el tipo 'Binario', tal que nos permita representar un
--      número binario mediante enteros. Debe poder mostrarse por pantalla.
--      Ver los ejemplos de las funciones para conocer los constructores.
--    * Definir la función (int2binario n), por ejemplo:
--       λ> int2binario 1  ==   B 1 BFin
--       λ> int2binario 110 ==  B 1 (B 1 (B 0 BFin))
--       λ> int2binario 121 ==  *** Exception: El valor de entrada no es binario
--    * Definir la función (binario2int b), por ejemplo:
--       λ> binario2int (B 1 (B 1 (B 0 BFin))) == 110
--       λ> binario2int (B 0 (B 1 (B 0 BFin))) == 10

data Binario = B Int Binario | BFin
  deriving (Show)

int2binario :: Int -> Binario
int2binario n = int2bin n (BFin)
int2bin x b | r > 1 = error "El valor de entrada no es binario"
            | d == 0 = B r b
            | otherwise = int2bin d (B r b)
  where r = rem x 10
        d = div x 10

binario2int :: Binario -> Int
binario2int b = bin2int b 0
bin2int BFin n = n
bin2int (B x b) n = (bin2int b (n*10+x))

-- ---------------------------------------------------------------------
-- Ejercicio 6. [1 pto]
-- ---------------------------------------------------------------------
-- Algunos algoritmos de compresión de imágenes hacen uso de los planos
-- de bits, o bitplanes. Sea una matriz m de números en binario, los
-- bitplanes son las matrices de bits correspondientes al bit n-ésimo de
-- cada elemento en m. Es decir, la matriz con el primer bit de todos los
-- elementos es el primer bitplane, la matriz con el segundo bit de todos
-- los elementos es el segundo bitplane, ... Supongamos una representación
-- little-endian (el bit menos significativo (en la posición 1) es el último,
-- el bit 2 es el antepenúltimo, etc.). Por ejemplo, los bitplanes de la matriz
--  ┌             ┐
--  │ 101   1  10 │
--  │   0  11 100 │
--  │  10 110   1 │
--  └             ┘
-- es la lista de matrices siguiente (del tercer bitplane al primero):
--  ┌       ┐   ┌       ┐   ┌       ┐
--  │ 1 0 0 │   │ 0 0 1 │   │ 1 1 0 │
--  │ 0 0 1 │   │ 0 1 0 │   │ 0 1 0 │
--  │ 0 1 0 │   │ 1 1 0 │   │ 0 0 1 │
--  └       ┘ , └       ┘ , └       ┘
--
-- Definir la función (bitplanes m), tal que reciba una matriz
-- de números en binario (por simplicidad de tipo Int, asume que solo
-- contiene 0s y 1s), y devuelva una lista de matrices con los bitplanes
-- desde el más significativo (el '1' más significativo de todos los
-- elementos de la matriz) hasta el menos significativo. La siguiente
-- matriz de ejemplo corresponde al anterior.

matrizEj :: Matrix Int
matrizEj = fromLists [[101, 1, 10], [0, 11, 100], [10, 110, 1]]

bitplanes :: Matrix Int -> [Matrix Int]
bitplanes m = [ fmap (bit b) m | b <- [numBitPlanes, numBitPlanes-1 .. 1]]
  where numBits x = length (show x)
        numBitPlanes = maximum (map numBits (toList m))

bit k x | n < k = 0
        | otherwise = read [(show x)!!(n-k)]
  where n = length (show x)
