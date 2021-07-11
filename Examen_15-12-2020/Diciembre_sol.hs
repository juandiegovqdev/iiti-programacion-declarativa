-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 2a Convocatoria                               12 de Diciembre de 2020
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

import Data.List as L
import Data.Matrix as M
import Data.Map as D
import Control.Monad
import Control.Exception (catch, SomeException)


-- ============================================================================
-- Ejercicio 1. (2,5 ptos) Define la función
--   maximoConsecutivos :: (Eq a) => [a] -> Integer
-- tal que '(maximoConsecutivos xs)' es el número máximo de elementos
-- consecutivos iguales en la lista 'xs'. Por ejemplo,
--   maximoConsecutivos [1..10]                    ==  1
--   maximoConsecutivos [1,1,2,2,2,3,4,4,4]        ==  3
--   maximoConsecutivos [1,1,2,2,2,3,4,4,4,4]      ==  4
--   maximoConsecutivos [1,1,2,2,2,2,2,3,4,4,4,4]  ==  5
--   maximoConsecutivos "abbcccddddeeeffg"         ==  4
-- ----------------------------------------------------------------------------

-- 1ª solución
maximoConsecutivos :: (Eq a) => [a] -> Integer
maximoConsecutivos xs =
  maximoConsecutivosAux xs 0 0

maximoConsecutivosAux :: (Eq a) => [a] -> Integer -> Integer -> Integer
maximoConsecutivosAux [] _ m = m
maximoConsecutivosAux [x] n m =
  max (n+1) m
maximoConsecutivosAux (x1:x2:xs) n m
  | x1 == x2 = maximoConsecutivosAux (x2:xs) (n+1) m
  | otherwise = maximoConsecutivosAux (x2:xs) 0 (max (n+1) m)

-- 2ª solución                
maximoConsecutivos2 [] = 0                
maximoConsecutivos2 xs =
  maximum (L.map L.genericLength (L.group xs))
-- ============================================================================

-- ============================================================================
-- Ejercicio 2. (2,5 ptos) Los árboles binarios con datos en nodos internos y
-- hojas se pueden representar con el siguiente tipo de dato:

data Arbol a = H a
             | N a (Arbol a) (Arbol a)
               deriving Show

-- Por ejemplo, el siguiente árbol binario:
--
--           1
--          / \
--         /   \
--        4     7
--       / \   / \
--      5   6 8   3
--     / \
--    2   9   
--
-- se representa como

ejArbol :: Arbol Integer
ejArbol =
  N 1 (N 4 (N 5 (H 2) (H 9)) (H 6)) (N 7 (H 8) (H 3))

-- Asumiendo que todos los elementos del árbol son distintos entre sí, definir
-- la función  
--   caminoEntreHojas :: (Eq a) => a -> a -> Arbol a -> [a]
-- tal que '(caminoEntreHojas a b t)' es la lista que contiene los nodos por
-- los que pasa el camino más corto desde la hoja con el valor 'a' hasta la hoja
-- con valor 'b' en el árbol 't', si es que ambos valores están presentes en el
-- árbol. Si alguno de los dos valores no está presente en el árbol el resultado
-- debe ser la lista vacía. Por ejemplo,
--   caminoEntreHojas 2 3 ejArbol  ==  [2,5,4,1,7,3]
--   caminoEntreHojas 3 2 ejArbol  ==  [3,7,1,4,5,2]
--   caminoEntreHojas 9 2 ejArbol  ==  [9,5,2]
--   caminoEntreHojas 6 8 ejArbol  ==  [6,4,1,7,8]
--   caminoEntreHojas 0 3 ejArbol  ==  []
--   caminoEntreHojas 2 0 ejArbol  ==  []
-- ----------------------------------------------------------------------------

-- 1ª solución
caminoEntreHojas :: Eq a => a -> a -> Arbol a -> [a]
caminoEntreHojas a b (H _) = []
caminoEntreHojas a b (N v i d)
  | ai && bi = caminoEntreHojas a b i
  | ad && bd = caminoEntreHojas a b d 
  | ai && bd = reverse (camino a i) ++ [v] ++ (camino b d)
  | ad && bi = reverse (camino a d) ++ [v] ++ (camino b i)
  | otherwise = []
  where ai = pertenece a i 
        ad = pertenece a d
        bi = pertenece b i
        bd = pertenece b d

pertenece :: Eq a => a -> Arbol a -> Bool
pertenece x (H v) = x == v
pertenece x (N v i d) = x == v || pertenece x i || pertenece x d

camino :: Eq a => a -> Arbol a -> [a]
camino x (H v) 
  | x == v = [x]
  | otherwise = []
camino x (N v i d)
  | ci /= [] = v:ci
  | cd /= [] = v:cd
  | otherwise = []
  where ci = camino x i
        cd = camino x d 

-- 2ª solución
caminoEntreHojas' :: Eq a => a -> a -> Arbol a -> [a]
caminoEntreHojas' a b arbol = unir (camino a arbol) (camino b arbol) (raiz arbol)

unir :: Eq a => [a] -> [a] -> a -> [a]
unir [] _ _ = []
unir _ [] _ = []
unir xs'@(x:xs) ys'@(y:ys) v
  | x == y = unir xs ys x
  | otherwise = (reverse xs') ++ [v] ++ ys'

raiz :: Arbol a -> a
raiz (H v) = v
raiz (N v _ _) = v

-- ============================================================================

-- ============================================================================
-- Ejercicio 3. (2,5 ptos) El problema de las N torres consiste en colocar 
-- N torres en un tablero con N filas y N columnas de forma que no haya dos
-- torres en la misma fila ni en la misma columna.  
--
-- Cada solución del problema de puede representar mediante una matriz
-- con ceros y unos donde los unos representan las posiciones ocupadas
-- por las torres y los ceros las posiciones libres. Por ejemplo, 
--    ( 0 1 0 )
--    ( 1 0 0 )
--    ( 0 0 1 )
-- representa una solución del problema de las 3 torres.
-- 
-- Definir la función
--    torres  :: Int -> [Matrix Int]
-- tal que (torres n) es la lista de las soluciones del problema de las
-- n torres. Por ejemplo,  
--       λ> torres 3
--       [( 1 0 0 )
--        ( 0 1 0 )
--        ( 0 0 1 )
--       ,( 1 0 0 )
--        ( 0 0 1 )
--        ( 0 1 0 )
--       ,( 0 1 0 )
--        ( 1 0 0 )
--        ( 0 0 1 )
--       ,( 0 1 0 )
--        ( 0 0 1 )
--        ( 1 0 0 )
--       ,( 0 0 1 )
--        ( 1 0 0 )
--        ( 0 1 0 )
--       ,( 0 0 1 )
--        ( 0 1 0 )
--        ( 1 0 0 )
--       ]
--   donde se ha indicado con 1 las posiciones ocupadas por las torres. 
-- Nota: puede ser útil la función permutations de Data.List
-- ---------------------------------------------------------------------

-- 1ª definición
-- =============
torres :: Int -> [M.Matrix Int]
torres n = [ tablero xs | xs <- L.permutations [1..n]] 
  where fila i = replicate (i-1) 0 ++ [1] ++ replicate (n-i) 0
        tablero = M.fromLists . L.map fila 

-- 2ª definición
-- =============

torres2 :: Int -> [M.Matrix Int]
torres2 n = 
  [permutacionAmatriz n p | p <- L.sort (L.permutations [1..n])]

permutacionAmatriz :: Int -> [Int] -> Matrix Int
permutacionAmatriz n p =
  M.matrix n n f
  where f (i,j) | (i,j) `elem` posiciones = 1
                | otherwise               = 0
        posiciones = zip [1..n] p    

-- 3ª definición
-- =============

torres3 :: Int -> [M.Matrix Int]
torres3 = L.map M.fromLists . L.permutations . M.toLists . M.identity

-- ============================================================================

-- ============================================================================
-- Ejercicio 4. (2,5 puntos) Define el siguiente programa:

-- a) Define una función main, donde primero pida al usuario un nombre de
-- de fichero, y se recoja el nombre en una variable (0.5 ptos)

-- b) Si el fichero no existe, debe dar un error explicativo al usuario 
-- y vuelva a a) (0.5 ptos)

-- c) Si el fichero existe, cargar el contenido en un diccionario Data.Map. Vamos a asumir que el 
-- formato es el correcto, y se trata de un fichero de configuración, con una línea por opción (1pto): 
-- OPCION VALOR
-- Por ejemplo:
-- usuario Grogu
-- modo Superusuario
-- pantalla Primaria
 
-- d) Pedir al usuario un nombre de OPCION y devolver el VALOR correspondiente según lo cargado
-- en el diccionaro. Acto seguido, el programa acaba. (0.5ptos).

-- Un ejemplo (usando el fichero ejemplo.txt adjunto al examen):
-- > main
-- Indica el nombre de fichero: opciones.txt
-- opciones.txt: openFile: does not exist (No such file or directory)
-- Error en el fichero, o no existe o no es accesible. Prueba de nuevo.
-- Indica el nombre de fichero: ejemplo.txt
-- Indica una opcion: usuario
-- Grogu

main :: IO()
main = do
  -- a)
  putStr "Indica el nombre de fichero: "
  fileName <- getLine
  -- b)
  input <- catch (readFile fileName)
             (\err -> print (err::SomeException) >> return "")
  if (L.null input) then do
    putStrLn "Error en el fichero, o no existe o no es accesible. Prueba de nuevo."
    main
  else do
    -- c)
    let dic = procesaFichero (lines input)
    -- d)
    putStr "Indica una opcion: "
    optionName <- getLine
    putStr $ dic D.! optionName

procesaFichero :: [String] -> D.Map String String
procesaFichero css = D.fromList [  par cs | cs <- css]
    where par cs = (head (words cs),last (words cs))