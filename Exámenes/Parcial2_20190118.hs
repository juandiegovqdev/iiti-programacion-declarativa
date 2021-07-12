-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 2                                       17 de Enero de 2019
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · Antes de continuar, cambie el nombre de este archivo por:
--                   Parcial2_<uvus>.hs
--   donde <uvus> debe ser su usuario virtual.
-- · Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- -------------------------------------------------------------------

import Text.CSV
import PilaConTipoDeDatoAlgebraico
import Data.Matrix
import System.Environment (getArgs)

-- -------------------------------------------------------------------
-- Ejercicio 1. [1,75 ptos]
-- En relación con el juego del cuatro en raya...
--
-- 1. Defina los tipos siguientes:
--    * Un nuevo tipo de datos Ficha, que indique ficha Roja o Azul.
--    * Un tipo Columna, sinónimo de una lista de fichas.
--    * Un nuevo tipo de datos CuatroEnRaya,
--      con un constructor de lista de columnas.
-- 2. Defina la función colocaFicha, que reciba una ficha, el número de
--    columna en la que colocar la ficha (empezando en 1) y un tablero
--    de tipo 4 en raya, y devuelva el tablero actualizado.
--
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 2. [1,75 ptos]
-- Se plantea la necesidad de trabajar con árboles que adopten tipos
-- flexibles. Para ello:
--
-- 1. Defina un tipo polimórfico de árbol que acepte tres tipos de datos
-- cualesquiera (posiblemente distintos) y admita dos constructores:
--    * Uno para nodos, que reciba un elemento de tipo par de a y b,
--      y tres árboles hijos del mismo tipo del padre
--    * Otro para hojas, que contengan un lemento de tipo par de a y c
-- 2. Defina la función devuelveValidos, que reciba un predicado p y un
--    árbol de tipos, y devuelva un par de listas, la primera conteniendo
--    los datos de tipo b de los nodos que cumplan p y
--    la segunda de tipo c de las hojas que no cumplan p
--
-- ---------------------------------------------------------------------


-- -------------------------------------------------------------------
-- Ejercicio 3. [2 ptos]

-- 1. Defina, con sintaxis de registro, un nuevo tipo que contenga la
--    información sobre planetas que aparecen en las películas
--    de star wars:
--    * name, diameter, population, de tipo String
--    * residents, de tipo lista de String

-- 2. Haga que el tipo anterior disponga de un valor por defecto,
-- de modo que podamos posteriormente crear elementos del tipo
-- sin necesidad de proporcionar todos los datos solicitados

-- 3. Defina un tipo sinónimo de una lista de planetas

-- 4. Realice un programa principal que:
--    a) Importe el archivo "planets.json",
--    b) Para cada planeta, imprima por pantalla su nombre,
--       seguido de su radio (la mitad de su diámetro)
--    ** Nota: si no puede resolver este apartado,
--             puede optar por un ejercicio simplificado,
--             por 1.5 puntos, que en lugar de "planets.json"
--             procese "planet.json", conteniendo un único
--             planeta, y devolviendo el nombre del mismo
--             junto con el número de residentes ilustres
--             (ver residents)
-- -------------------------------------------------------------------


-----------------------------------------------------------------------
-- Ejercicio 4. [1 pto]
--
-- El siguiente programa carga el contenido del fichero "cars-2018.csv",
-- lo parsea y posteriormente procesa cabecera y cuerpo, que
-- en este momento se encuentran sin implementar.

-- Se pide la siguiente implementación para dichas funciones:
-- a) Procesar la cabecera, imprimiento por cada campo de la misma
--    su número y línea, de la siguiente forma:
--    1: "year"
--    2: "make"
--    ...
--
--   * Nota: por la mitad de la nota, puede imprimir únicamente
--            el nombre del campo.
--
-- b) Procesar los 20 primeros registros, imprimiendo por cada uno el año,
--    marca, modelo y el número de estilos principales
--    
--   * Nota: por la mitad de la nota, puede limitarse a
--           procesar todos los registros e imprimir año, marca y modelo
--          
-----------------------------------------------------------------------

coches :: IO ()
coches = do
  args <- getArgs
  let filename = if null args then "cars-2018.csv" else args!!0
  contents <- readFile filename

  let csv = parseCSV filename contents
      filas = case csv of
        (Left _) -> []
        (Right lineas) -> lineas
      filasValidas = filter (\x -> length x == 4) filas

  procesaCabecera (head filasValidas)
  procesaContenido (tail filasValidas)

procesaCabecera = undefined
procesaContenido = undefined

-- Función de ayuda:
pasaALista :: Field -> [String]
pasaALista cadena = read cadena::[String]

-- ---------------------------------------------------------------------
-- Ejercicio 5. [1,5 ptos]
-- Se denomina matriz dispersa aquella cuyos elementos son 
-- mayoritariamente nulos. La representación de matrices dispersas se
-- suele realizar de forma densa, es decir, dejando los elementos no
-- nulos pero anotando en qué posición original estaban. En este ejercicio
-- se pide construir la representación densa de una matriz p como sigue:
--   - una matriz q de nxm', donde n es el número de filas de p y m'
--     es el mayor número de elementos no nulos de las filas de p. Por
--     ejemplo, m' de matrizEj es 2, ya que la segunda fila tiene 2
--     elementos no nulos.
--   - los elementos de la matriz q son pares (Int,Double), donde el
--     primero del par es la columna donde aparecía el elemento, y el
--     segundo es el elemento en sí. Si la columna tiene menos elementos
--     no nulos que m', entonces se rellena con pares (0,0.0).
-- ---------------------------------------------------------------------

matrizEj :: Matrix Double
matrizEj = fromLists [[0.0,0.0,2.1],[1.6,0.0,-2.5],[0.5,0.0,0.0]]

-- Ejercicio 5.1. Definir la función (colsNoNulas i p), tal que devuelva
-- una lista de pares (j,v) por cada valor v no nulo en la fila i, siendo
-- j la columna donde aparece. Por ejemplo,
--   colsNoNulas 2 matrizEj == [(1,1.6),(3,-2.5)]


-- Ejercicio 5.2. Definir la función (completaLista n xs x), tal que 
-- devuelva una lista con n elementos, incluyendo primero los de xs,
-- y rellenando el resto con x. Por ejemplo,
--   completaLista 5 [3,4,2] 0 == [3,4,2,0,0]


-- Ejercicio 5.3. Definir la función (matrizDensa p), tal que devuelva
-- la matriz con representación densa, descrita anteriormente, de p.
-- Por ejemplo, toLists (matrizDensa matrizEj) ==
--      [[(3,2.1),(0,0.0)],[(1,1.6),(3,-2.5)],[(1,0.5),(0,0.0)]]


-- ---------------------------------------------------------------------
-- Ejercicio 6. [1 pto]
-- Representamos las pilas mediante el TAD definido en el fichero
-- incluido en la cabecera del enunciado. Empleando solo las funciones
-- del TAD (sin convertir los datos a listas), definir la función
-- (ultimoElemPila p pila), que devuelva justamente el último elemento de
-- la pila que cumpla el predicado p, o Nothing si ninguno lo cumple.
-- Por ejemplo,
--   ultimoElemPila even ejPila == Just 10
--   ultimoElemPila odd ejPila == Nothing
--   ultimoElemPila (<5) ejPila == Just 4

ejPila :: Pila Int
ejPila = foldr apila vacia [2,4,6,8,10]


-- ---------------------------------------------------------------------
-- Ejercicio 7.1 [0.5 ptos]
-- Un número entero n es libre de cuadrados si no existe un número primo
-- p tal que p^2 divide a n. Por ejemplo, 10 es libre de cuadrado porque
-- 10 = 2*5, pero 12 no lo es porque es divisible entre 2^2. Definir
-- la función (libresDeCuadrado m) que devuelva la lista de booleanos
-- que indique para cada número entre el 1 y m si es libre de cuadrado.
-- Por ejemplo,
--   libresDeCuadrado 10 ==
--          [True,True,True,False,True,True,True,False,False,True]


-- Ejercicio 7.2 [0.5 ptos] Paralelizar la definición de libresDeCuadrado
-- empleando la función parallel map vista en teoría. Indicar en un
-- comentario cuál es el speedup alcanzado cuando se compara la versión
-- secuencial (anterior) y paralela (aquí solicitada) con m=5000.



-- Nota: emplear esta función main para comprobar cada versión
{-main = do
    let oxs = parLibresDeCuadrado 5000 
    print $ length $ filter (\a -> a) oxs
    return (oxs) -}

-- speedup = 11,616s / 1,451s = 8x
