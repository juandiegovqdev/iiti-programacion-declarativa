-- Programación Declarativa 2019/20
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 2a Convocatoria                               10 de Septiembre de 2020
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- Lugar ocupado:   laboratorio:            puesto:
-- ----------------------------------------------------------------------
-- INSTRUCCIONES PARA LA ENTREGA
-- 1. CAMBIA EL NOMBRE de este archivo por:          Septiembre_<uvus>.hs
--    donde "<uvus>" es tu UVUS.
-- 2. COMENTA LAS LÍNEAS CON ERRORES hasta que se pueda cargar el fichero
--    sin problemas. ESCRIBE tu nombre y apellidos en la cabecera.
-- 3. COMPRIME este archivo en un único fichero llamado EXACTAMENTE:
--      ENTREGA-<uvus>.tar.gz      (o bien)       ENTREGA-<uvus>.tar.xz
--    donde "<uvus>" es tu UVUS. No te olvides del guión después de
--    ENTREGA, y NO lo comprimas en un fichero .zip.
-- 4. REINICIA el equipo. En el menú de selección del sistema (con fondo
--    blanco), HAZ CLICK SOBRE "Enviar examen" al lado de sistema Ubuntu.
-- 5. Después de comprobar que se ha entregado, VUELVE A TU EQUIPO y
--    APÁGALO.
-- ----------------------------------------------------------------------
-- ORIENTACIONES
-- · Escribe la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrate de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada.
-- · Puedes añadir tantas funciones auxiliares (incluyendo el tipo
--   adecuadamente) como necesites.
-- ----------------------------------------------------------------------

import Data.Matrix
import Control.Monad
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)


-- ----------------------------------------------------------------------
-- Ejercicio 1. (3,5 puntos)
-- ----------------------------------------------------------------------
-- Muchos algoritmos de compresión de imágenes se basan en la transformada
-- discreta del coseno (DCT en inglés). La DCT se aplica sobre una lista
-- de elementos, y devuelve otra lista del mismo tamaño, donde cada 
-- elemento, en la posición k, se puede calcular con la siguiente fórmula
-- (se adjunta con el examen una imagen con la fórmula descrita mejor):
--
--      _____ n               __                        __
--      \                    |     Pi          1          |
-- Y_k = \         x_i * cos |    ____ * (i + ___) * k    |
--       /                   |__    n          2        __|
--      /____ i=1           
--
-- Donde x_i es el elemento en la posición i-ésima de la lista de entrada x,
-- Y_k es el elemento en la posición k-ésima de la salida, y n es el tamaño
-- de la lista de entrada x. Dicho en otras palabras: cada elemento en la 
-- posición k de la salida se calcula como el sumatorio sobre cada elemento
-- de la entrada multiplicado por el resultado del coseno sobre el número Pi 
-- divido por el tamaño n, el iterador i más un medio y k (la posición del 
-- elemento de salida).
--
-- Se pide definir una función que aplique la DCT sobre una lista de números
-- reales de tres formas distintas:
--   a) Basándose solo en listas por comprensión. (1pto)
--   b) Basándose solo en funciones de orden superior (libre elección, se
--      valorará la variedad de funciones elegidas). (1.25ptos)
--   c) Basándose solo en recursión. (1.25ptos)
-- Nota 1: el número Pi y el coseno en Haskell se pueden usar directamente
-- como pi y cos sin necesitar de importar ningún módulo.
-- Por ejemplo (se permite un error del 0.0001),
-- *Main> dctOS [0.0, 0.1, 0.7, 0.1, -0.9, 0.5]
-- [-0.12543519,0.27942222,-0.24141996,-0.74999976,1.3277059,-0.4999989]

-- a)
dctCS :: [Float] -> [Float]
dctCS xs = [sum [xs!!(i-1)*(cos (pi/n'*(n'+0.5)*(fromIntegral i)*(fromIntegral k))) | i<-[1..n]] | k <- [1..n]] 
    where n = length xs
          n' = fromIntegral n

-- b)
dctOS :: [Float] -> [Float]
dctOS xs = foldr (\k ys -> (sum (zipWith (*) xs (es k))):ys) [] [1..n]
    where n = length xs
          n' = fromIntegral n
          es k = map (cos . (\i -> pi/n'*(n'+0.5)*(fromIntegral i)*(fromIntegral k))) [1..n]

-- c)
dctRS :: [Float] -> [Float]
dctRS xs = dctRS' xs 1.0

dctRS' :: [Float] -> Float -> [Float]
dctRS' xs k 
    | k > n' = []
    | otherwise = (dctRF xs 1.0):(dctRS' xs (k+1))
    where n = length xs
          n' = fromIntegral n
          dctRF :: [Float] -> Float -> Float
          dctRF [] _ = 0.0
          dctRF (x:xs) i = x*(cos (pi/n'*(n'+0.5)*i*k)) + (dctRF xs (i+1))

-- ----------------------------------------------------------------------
-- Ejercicio 2. (3 puntos)
-- ----------------------------------------------------------------------    
-- En un árbol ternario los nodos tienen como máximo tres hijos.
-- Suelen ser útiles para almacenar valores en orden: a la izquierda de
-- cada valor solo hay valores menores, a la derecha solo mayores, y en el
-- centro solo el mismo valor. Algunos ejemplos de árboles ternarios
-- ordenados son:

--  ejA1:    5              ejA2:       4             ejA3:      4
--         / | \                       / \                       |
--        3  5  6                     3   7                      4
--       / \                             /|\                     |
--      1   4                           5 7 9                    4

-- Algunos ejemplos que no son árboles ternarios ordenados son:

--  ejA4:      4            ejA5:      6
--            /|\                     / \
--           3 1 2                   4   9
--                                      / \
--                                     3   10

-- a) Define un tipo de dato algebráico que nos permita crear árboles
--    ternarios. Recuerda que cada nodo puede tener como mucho tres hijos.
--    (0.5 ptos)

data ArbolTO a = N a (ArbolTO a) (ArbolTO a) (ArbolTO a) | H 
    deriving (Show,Eq)

-- b) Define los ejemplos 1, 2, 3, 4 y 5 con el tipo de dato definido.
--    (1pto, la nota es independiente del apartado a, pero puntuará si
--    es coherente con el tipo definido)

ejA1, ejA2, ejA3, ejA4, ejA5 :: ArbolTO Int
ejA1 = N 5 (N 3 (N 1 H H H) H (N 4 H H H)) (N 5 H H H) (N 6 H H H)
ejA2 = N 4 (N 3 H H H) H (N 7 (N 5 H H H) (N 7 H H H) (N 9 H H H))
ejA3 = N 4 H (N 4 H (N 4 H H H) H) H
ejA4 = N 4 (N 3 H H H) (N 1 H H H) (N 2 H H H)
ejA5 = N 6 (N 4 H H H) H (N 9 (N 3 H H H) H (N 10 H H H))

-- c) Define una función predicado, tal que reciba un árbol ternarnio e
--    indique si es un árbol ternario ordenado. Se valorará que la signatura
--    de la función sea lo más genérica posible. (1.5ptos, la nota es 
--    independiente del apartado a, pero puntuará si es coherente con el 
--    tipo definido). Por ejemplo,
-- > arbolTOrdenado ejA1 
-- True
-- > arbolTOrdenado ejA2
-- True
-- > arbolTOrdenado ejA3
-- True
-- > arbolTOrdenado ejA4
-- False
-- > arbolTOrdenado ejA5
-- False

arbolTOrdenado :: Ord a => ArbolTO a -> Bool
arbolTOrdenado H = True
arbolTOrdenado (N v i c d) = (todos (<v) i) && (todos (==v) c) && (todos (>v) d) && (arbolTOrdenado i) && (arbolTOrdenado d)
    where todos p H = True
          todos p (N v' i _ d) = p v' && todos p i && todos p d

-- ----------------------------------------------------------------------
-- Ejercicio 3. (3,5 puntos)
-- ----------------------------------------------------------------------
-- Vamos a aplicar la función DCT definida antes sobre una imagen. Para ello
-- vamos a cargar una matriz con los valores de los píxeles desde un fichero.
-- Sigue los siguientes pasos:

-- a) Si has realizado el ejercicio 1, iguala la función dct a alguna
-- que hayas definido arriba. Si no, déjalo como está para poder hacer 
-- este ejercicio (0ptos, no cuenta para la nota la implementación usada, pero
-- se valorará usar alguna versión del ejercicio 1 si se ha hecho):

--dct xs = xs
dct = dctCS
-- dct = dctOS
-- dct = dctRS

-- b) Define una función main, donde primero pida al usuario un nombre de
-- de fichero, y se recoja el nombre en una variable (0.5 ptos)
-- c) Si el fichero no existe, debe dar un error explicativo al usuario 
-- y terminar (0.5 ptos)
-- d) Si el fichero existe, cargar el contenido en una matriz de números
-- reales, donde cada fila sea una fila de la matriz. (0.75ptos)
-- e) Aplicar la DCT al contenido de la matriz. Para ello, hay que recorrer
-- la matriz por columnas para construir la lista requerida para la función
-- dct (0.75ptos).
-- f) Construir una matriz con la salida de la DCT, rellenando primero por
-- columnas (0.75ptos).
-- g) Terminar imprimiendo la matriz (0.25ptos).

-- Un ejemplo (usando el fichero imagen.txt adjunto al examen):
-- > main
-- Indica el nombre de fichero: img.txt
-- img.txt: openFile: does not exist (No such file or directory)
-- Error en el fichero, o no existe o no es accesible. Prueba de nuevo.
-- Indica el nombre de fichero: imagen.txt
-- ┌                                                                       ┐
-- │    -4.9320617   -0.75338423    -0.6184246      2.092227   -0.48702848 │
-- │     1.2309818     1.0691462    0.79687154   -0.69948834    0.97440386 │
-- │    0.37078878   -0.60895073    -0.6253055   -0.47244325    -2.1662965 │
-- │  4.7100604e-2    0.17165993      1.015995    0.21707146     2.1683674 │
-- │ -6.9366634e-2    0.15452802    -2.0144224    0.40451404  1.1743596e-4 │
-- └                                                                       ┘

mEj :: Matrix Float
mEj = fromLists [[0,  0,  0.5,1],
                 [0,  0.5,0.7,1],
                 [0.5,0.7,1,  1],
                 [0.7,1,  1,  1]]

procesaFichero :: [String] -> [[Float]]
procesaFichero [] = []
procesaFichero (ls:lss) = (generaFila ls):procesaFichero lss
  where generaFila ls = map read ws
        ws = words ls

dctM :: Matrix Float -> Matrix Float
dctM m = transpose $ fromList x y $ dct $ toList $ transpose m 
    where x = nrows m
          y = ncols m

main :: IO ()
main = do
  -- b)
  putStr "Indica el nombre de fichero: "
  fileName <- getLine
  -- c)
  input <- catch (readFile fileName)
             (\err -> print (err::SomeException) >> return "")
  if (null input) then do
    putStrLn "Error en el fichero, o no existe o no es accesible. Prueba de nuevo."
    main
  else do
    -- d)
    let m = fromLists (procesaFichero (lines input))
    -- e) y f)
    let mdct = dctM m
    -- g)
    print mdct
