-- Programación Declarativa 2020/21
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2                                      22 de Diciembre de 2020
-- ----------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- ----------------------------------------------------------------------

import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import System.Directory
import Text.CSV
import Text.Printf
import Data.List
import Test.QuickCheck

-- ------------------------------ --
-- PARTE A del examen, 40 minutos --
-- ------------------------------ --

-- ----------------------------------------------------------------------
-- Ejercicio 1. (2 puntos)
-- Define la función (parte ns xs), tal que dadas una lista de enteros 'ns' 
-- y una lista de elementos 'xs' (de cualquier tipo), devuelva una lista de
-- listas con los segmentos de 'xs' de tamaños según la lista 'ns'. Esto se ve
-- mejor con un ejemplo, si ns = [2,3] y xs = [1,2,3,4,5], entonces
-- devuelve una lista con los 2 primeros elementos de xs y después los 3
-- siguientes, es decir, parte ns xs = [[1,2],[3,4,5]]. Más ejemplos:
-- > parte [3,3,4] [1..10]
-- [[1,2,3],[4,5,6],[7,8,9,10]]
-- > parte [3,3] [1..10]  
-- [[1,2,3],[4,5,6]]
-- > parte [3,3] [1..5] 
-- [[1,2,3],[4,5]]

-- Ejercicio 1.a. (0,75 puntos) 
-- Define la función (parte n xs) con recursión

parte = undefined

-- Ejercicio 1.b. (0,75 puntos) 
-- Define la función (parte n xs) haciéndote valer del plegado por la
-- izquierda (foldl)

parte' = undefined

-- Ejercicio 1.c. (0,5 puntos)
-- Comprueba con QuickCheck que la longitud del resultado de evaluar
-- (parte ns xs) es igual a la longitud de ns, siempre y cuando se cumpla que:
--   * la suma de los elementos de ns es igual a la longitud de xs.
--   * todos los elementos de ns son mayores estrictos que 0

prop_parte = undefined

-- La comprobación es quickCheck prop_parte
-- *** Gave up! Passed only 9 tests; 1000 discarded tests.
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 2. (1,5 puntos)
-- Define la función
--   agrupa :: (Ord a) => [a] -> [a] -> ( [[a]] , [[a]] )
-- tal que (agrupa xs ys) reciba dos listas de elementos 'xs' e 'ys'
-- y devuelva un par cuyas dos componentes tendrán el resultado de aplicar
-- la siguiente operación a 'xs' e 'ys', respectivamente. Para ilustrar
-- la operación con un ejemplo, vamos a usar xs = [1,1,2,2,1], e 
-- ys = [3,2,2,1,2]. La operación a realizar es:
--  * Cambia de orden los elementos de xs y de ys, ordenándolos primero por
--    xs, y si hay igualdad, después por ys. Por ejemplo, el resultado
--    sería para xs, xs' = [1,1,1,2,2], y para ys, ys' = [2,2,3,1,2]. 
--    Observa que los elementos de ys cambian de lugar junto con los de xs.
--  * Calcula para xs' (el resultado de aplicar lo anterior a xs) los 
--    segmentos de elementos que son iguales de forma consecutiva. Por 
--    ejemplo, si xs' = [1,1,1,2,2], entonces [[1,1,1],[2,2]]
--  * Realiza en ys' los mismos segmentos (de igual tamaño y posiciones)
--    que los realizados para xs' en el punto anterior. Por ejemplo,
--    si tenemos ys' = [2,2,3,1,2] entonces obtenemos [[2,2,3],[2,1]].
--  * Devuelve en el par: primero la lista de listas obtenida en el
--    segundo punto, y segundo la list de listas del tercer punto.
-- Por simplicidad, asume que las longitudes de xs e ys son iguales.
-- Algunos ejemplos:
-- > agrupa [1,1,2,2,1] [3,2,2,1,2]
-- ([[1,1,1],[2,2]], [[2,2,3],[1,2]])
-- > agrupa [3,3,3,4] [1,2,3,4]
-- ([[3,3,3],[4]], [[1,2,3],[4]])
-- > agrupa ["S","S","N","L"] ["A","A","A","S"]  
-- ([["L"],["N"],["S","S"]], [["S"],["A"],["A","A"]])

agrupa :: (Ord a) => [a] -> [a] -> ([[a]],[[a]])
agrupa = undefined

-- ---------------------------------------------------------------------


-- ------------------------------ --
-- PARTE B del examen, 40 minutos --
-- ------------------------------ --

-- ---------------------------------------------------------------------
-- Ejercicio 3. (2 puntos)

-- Ejercicio 3.a. (0,5 puntos)
-- Declara el tipo Tabla con un solo constructor, T, tal que sirva
-- para representar una tabla de datos cuyos valores son de tipo 'a'
-- (polimórfico). El constructor T debe tener asociados los siguientes 
-- parámetros:
--  * un String, que será el nombre de la tabla.
--  * una lista de strings, que codificará el nombre de las columnas de 
--    la tabla.
--  * una lista de listas de cualquier tipo 'a', que almacenará los valores
--    de cada columna como listas.
--  * un Int, que corresponderá con el número de filas de la tabla.
-- El tipo se debe poder imprimir por pantalla. 

-- Tabla = ....

-- Ejercicio 3.b. (1,5 puntos)
-- Para este ejercicio vamos a usar la función agrupa que debiste definir
-- en el ejercicio 2. Si no la pudiste definir, puedes hacer uso de la 
-- definición siguiente de juguete (aunque no haga lo que debería, la 
-- puedes usar para este ejercicio sin afectar a la nota)

-- agrupa :: (Ord a) => [a] -> [a] -> ( [[a]] , [[a]] )
-- agrupa xs ys = ([xs], [ys])

-- Define la función 
--    (agrupaPor t colref coldest fagr),
-- tal que reciba una tabla 't' (de tipo Tabla y que contiene valores de 
-- tipo Ordenables), dos argumentos de tipo String, 'colref' y 'coldest', y 
-- una función de agregación 'fagr' (es decir, de tipo '[a] -> a', por ejemplo,
-- podrá ser sum, maximum, product, etc.).
-- La función toma de 't' la columna cuyo nombre es 'colref', la columna con nombre
-- 'coldest' y las agrupa con la función 'agrupa' (ver ejercicio 2). Sea (xss,yss)
-- la salida de 'agrupa', entonces vamos a resumir cada sublista de xss e yss de 
-- la siguiente forma: 
--   * para cada xs de xss obtenemos un valor x que es el primero de la lista
--   * para cada ys de yss obtenemos un valor y que es el resultado de aplicar la
--     función de agregación, fagr, a ys. Por ejemplo,
--     si el resultado de 'agrupa' es ([[1,1,1],[2,2]],[[1,2,3],[4,5]]), y 'fagr' es
--     la función sum, entonces obtenemos [1,2] y [6,9], respectivamente.
-- Finalmente, el resultado de 'agrupaPor' es otra tabla cuyas columnas son 'colref'
-- y 'coldest', y los valores son los resultados de aplicar la operación descrita
-- anteriormente. Veamos algunos ejemplos para entenderlo mejor:

-- Supongamos la siguiente tabla (descoméntala una vez hayas hecho el 3.a)
--tablaEj :: Tabla Int 
--tablaEj = T "origen" ["Edad","Altura","Salario"] [[25,25,24,25],[150,165,150,175],[1234,1435,1102,1421]] 4 

-- > agrupaPor tablaEj "Edad" "Salario" sum
-- T "agrupado" ["Edad","Salario"] [[24,25],[1102,4090]] 2
-- > agrupaPor tablaEj "Altura" "Salario" maximum
-- T "agrupado" ["Altura","Salario"] [[150,165,175],[1234,1435,1421]] 3

-- Si has usado la definición de agrupa de jueguete de arriba, los ejemplos son:
-- > agrupaPor tablaEj "Edad" "Salario" sum
-- T "agrupado" ["Edad","Salario"] [[25],[5192]] 1
-- > agrupaPor tablaEj "Altura" "Salario" maximum
-- T "agrupado" ["Altura","Salario"] [[150],[1435]] 1

agrupaPor = undefined
          
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 4. (1,5 puntos)
-- Los árboles binarios con valores distintos en nodos y en hojas se 
-- pueden representar mediante el tipo Arbol definido por 

data Arbol a b = H b 
                | N a (Arbol a b) (Arbol a b)
            deriving Show

-- Por ejemplo, un árbol con pares de enteros en los nodos y enteros en las
-- hojas:
--        (4,8)
--         / \ 
--        /   \
--       /     \
--    (4,2)   (8,5)
--     / \     / \
--    4   2   8   5 
--
-- se puede definir por
--    N (4,8) (N (4,2) (H 4) (H 2)) (N (8,5) (H 8) (H 5)) 

-- Un árbol como el anterior lo vamos a llamar Árbol de Máximos, ya que para
-- cada nodo interno hay un par que indica con la primera componente cuál es
-- el valor máximo de las hojas por el subárbol izquierdo, y en la segunda
-- componente cual es el máximo de las hojas del subárbol derecho. Define 
-- la función
--    maximosHojas :: Arbol Int Int -> Arbol (Int,Int) Int
-- tal que (maximosHojas a) calcule el árbol de máximos, tal y como se ha 
-- ilustrado arriba, para el árbol a. Por ejemplo,

ej1,ej2,ej3 :: Arbol Int Int 
ej1 = (N 1 (N 1 (H 4) (H 2)) (N 1 (H 8) (H 5)))
ej2 = (N 9 (N 4 (H 8) (H 4)) (N 8 (H 4) (H 9)))
ej3 = (N 29 (H 1) (N 524 (H 1) (H 1)))

-- > maximosHojas ej1
-- N (4,8) (N (4,2) (H 4) (H 2)) (N (8,5) (H 8) (H 5))
-- > maximosHojas ej2
-- N (8,9) (N (8,4) (H 8) (H 4)) (N (4,9) (H 4) (H 9))
-- > maximosHojas ej3
-- N (1,1) (H 1) (N (1,1) (H 1) (H 1))
-- ---------------------------------------------------------------------

maximosHojas :: Arbol Int Int -> Arbol (Int,Int) Int
maximosHojas = undefined


-- ------------------------------ --
-- PARTE C del examen, 30 minutos --
-- ------------------------------ --

-- ---------------------------------------------------------------------
-- Ejercicio 5. (3 puntos)
-- Escribir un programa compilable y ejecutable desde la terminal que reciba
-- como parámetros la ruta de un fichero de entrada CSV y el nombre de 2
-- columnas. Es decir, la llamada es:
--   ./programa [entrada_csv] [col1] [col2]
-- El programa debe:
--   a) cargar el contenido del fichero CSV en 'entrada_csv' en una variable
--     de tipo Tabla. Por simplicidad vamos a asumir que todas las columnas del
--     CSV siempre contienen valores numéricos. Tendrás que hacer la transformación
--     a valores numéricos a todos los valores. (1 punto)
--   b) calcular una tabla de agrupación con 'agrupaPor' (como en el ejercicio
--     3.b, si no lo has hecho, puedes usar la definición en Nota 1) para las
--     columnas 'col1', 'col2' y usando como función de agregación la media 
--     aritmética. (1 punto)
--   c) elige una de las siguientes opciones (hasta 1 punto):
--       * imprimir por pantalla la tabla resultado como se muestra en los 
--         ejemplos de abajo. (0,5 puntos)
--       * además de imprimir por pantalla la tabla, escribir en un fichero
--         con nombre out.csv el resultado resultado. (1 punto)
--
-- Se valorará más si se controlan los posible errores: existencia de ficheros, 
-- existencia de columnas en la tabla y número de parámetros en la llamada.
--
-- Nota 1:
-- Si no has podido definir la función agrupaPor del 3.b, usa la siguiente 
-- definición de juguete (sin repercutir en la nota):
-- agrupaPor _ c1 c2 _ = T "agrupado" [c1,c2] [[24,25],[1102,4090]] 2
 
-- Ejemplos de uso:
--
-- $ ./programa clima.csv Cielo Velocidad_Viento 
-- Cielo   Velocidad_Viento
-- 0.0     25.6
-- 1.0     22.0
-- 2.0     12.8
--
-- Si has usado la función agrupaPor de juguete de arriba, el ejemplo es
-- $ ./programa clima.csv Cielo Velocidad_Viento 
-- Cielo   Velocidad_Viento
-- 24      1102
-- 25      4090
--
-- Ejemplos valorables para máxima nota:
--
-- $ ./programa clima.csv Cielo
-- Los parámetros indicados no son correctos.
-- El formato es el siguiente:
--    programa [entrada csv] [col1] [col2]
-- donde
--   [entrada csv] -> es el archivo csv de entrada
--   [col1] y [col2] -> son la columnas de agrupación
--
-- $ ./programa clima.csv Cielo Vel
-- La columnas indicadas no existen en el CSV de entrada
--
-- $ ./programa clim.csv Cielo Temperatura
-- El fichero de entrada indicado no existe
--
-- ---------------------------------------------------------------------

-- Pueden serte útil estas funciones

-- Transponer los registros leídos de un CSV, de filas a columnas.
traspuesta :: [[a]] -> [[a]]
traspuesta registros = [[(registros!!f)!!c | f <-[0..(length registros)-1]]  | c <- [0..(length (head registros))-1]]

-- Imprimir mensajes de error capturados
control_error err = print "Se ha producido un error." >> print (err::SomeException) 
   
main :: IO ()
main = undefined

