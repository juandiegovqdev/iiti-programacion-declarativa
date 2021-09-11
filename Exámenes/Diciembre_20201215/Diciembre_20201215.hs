import Data.List as L
import Data.Matrix as M
import Data.Map as D
import Control.Monad
import Control.Exception (catch, SomeException)

-- ============================================================================
-- Ejercicio 2. Los árboles binarios con datos en nodos internos y
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

caminoEntreHojas = undefined

-- ============================================================================

-- ============================================================================
-- Ejercicio 3. El problema de las N torres consiste en colocar 
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
-- Pista: puede ser útil la función permutations de Data.List
-- ---------------------------------------------------------------------

torres = undefined

-- ============================================================================

-- ============================================================================
-- Ejercicio 4. Define el siguiente programa:

-- a) Define una función main, donde primero pida al usuario un nombre de
-- de fichero, y se recoja el nombre en una variable (0.5 ptos)

-- b) Si el fichero no existe, debe dar un error explicativo al usuario 
-- y vuelva a a) (0.5 ptos)

-- c) Si el fichero existe, cargar el contenido en un diccionario Data.Map.
-- Vamos a asumir que el formato es el correcto, y se trata de un fichero de 
-- configuración, con una línea por opción (1pto): 
-- OPCION VALOR
-- Por ejemplo:
-- usuario Grogu
-- modo Superusuario
-- pantalla Primaria
 
-- d) Pedir al usuario un nombre de OPCION y devolver el VALOR correspondiente
-- según lo cargado en el diccionaro. Acto seguido, el programa acaba. (0.5ptos).

-- Un ejemplo (usando el fichero ejemplo.txt adjunto al examen):
-- > main
-- Indica el nombre de fichero: opciones.txt
-- opciones.txt: openFile: does not exist (No such file or directory)
-- Error en el fichero, o no existe o no es accesible. Prueba de nuevo.
-- Indica el nombre de fichero: ejemplo.txt
-- Indica una opcion: usuario
-- Grogu

main = undefined

-- ============================================================================
