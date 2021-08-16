-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 3 Parcial                                      21 de Diciembre 2017
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- # Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- # Asegúrese de utilizar exactamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite
--   describiendo claramente su objetivo.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 2 (4'5 ptos).
-- -------------------------------------------------------------------
-- (2.1) Definir los tipos Jugador (con dos posibles valores para
-- poder distinguir quien tiene el turno en cada momento), Mesa (como
-- sinónimo de una lista de enteros, para guardar la cantidad de
-- estrellas en cada fila de la mesa) y Fila (con dos posibles
-- valores, para indicar que no se ha escogido ninguna fila aún o el
-- índice, un entero, de la fila que se ha escogido). Utilizar data
-- para la definición de los tipos solicitados (salvo Mesa).
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.2) Definir el tipo NimFilas, utilizando data, para la
-- información sobre el jugador que tiene el turno, la cantidad de
-- estrellas en cada fila y la fila escogida. Dicho tipo debe
-- pertenecer a la clase Show, por lo que hay que incluir una
-- definición de la función show.
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- Definición alternativa del tipo NimFilas si no se han resuelto los
-- ejercicios anteriores. 

-- type NimFilas = (Int, [Int], Int)

-- El primer elemento de la tupla será el jugador: 1 ó 2, el segundo
-- una lista con la cantidad de estrellas en cada fila de la mesa; y
-- el tercero: 0 si no se ha escogido fila o el índice de la fila
-- escogida.
-- -------------------------------------------------------------------


-- -------------------------------------------------------------------
-- (2.3) Definir una función,

-- inicioFilas :: Int -> NimFilas

-- tal que, dado un entero n, devuelva la información sobre el inicio
-- del juego: tiene el turno el primer jugador, sobre la mesa hay n
-- filas de estrellas completas y no se ha escogido ninguna fila.

-- Main> let mesaInicial = inicioFilas 5
-- Main> :t mesaInicial
-- mesaInicial :: NimFilas
-- Main> mesaInicial
-- 1: *
-- 2: **
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el primer jugador
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.4) Definir una función,

-- estrellasRestantesFilas :: NimFilas -> [Int]

-- tal que, dada la información sobre un instante de la partida,
-- devuelve una lista con la cantidad de estrellas que hay, sobre la
-- mesa, en cada una de las filas. 

-- Main> estrellasRestantesFilas mesaInicial
-- [1,2,3,4,5]
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.5) Definir una función,

-- jugadorFilas :: NimFilas -> Int

-- tal que, dada la información sobre un instante de la partida,
-- devuelve 1 si tiene el turno el primer jugador y 2 en caso
-- contrario.

-- Main> jugadorFilas mesaInicial
-- 1
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.6) Definir una función,

-- fila :: NimFilas -> Maybe Int

-- tal que, dada la información sobre un instante de la partida,
-- devuelve el índice de la fila escogida.

-- Main> fila mesaInicial
-- Nothing
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.7) Definir una función, utilizando guardas,

-- escogeFila :: NimFilas -> Int -> NimFilas

-- tal que, dada la información sobre un instante de la partida en el
-- que aún no se ha elegido ninguna fila y un entero i, devuelve la
-- información con la que continuaría la partida si el jugador que
-- tiene el turno escoge la fila i-ésima para eliminar estrellas de la
-- misma. Si ya hay una fila escogida, o no existe dicha fila, o en
-- dicha fila no quedan estrellas, la función debe devolver un error
-- (con un mensaje distinto según el motivo).

-- Main> escogeFila mesaInicial 8
-- Exception: escogeFila: índice incorrecto
-- Main> escogeFila mesaInicial 3
-- 1: *
-- 2: **
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el primer jugador
-- (que va a eliminar de la fila 3)
-- Main> escogeFila (escogeFila mesaInicial 3) 2
-- Exception: escogeFila: ya se ha escogido una fila
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.8) Definir una función,

-- eliminaEstrellasFilas :: NimFilas -> Int -> NimFilas

-- dada la información sobre un instante de la partida en el que ya
-- se ha escogido una fila y la cantidad de estrellas que se quieren
-- quitar de dicha fila, devuelve la información con la que se
-- continuaría la partida. 

-- Main> eliminaEstrellasFilas mesaInicial 2
-- Exception: eliminaEstrellasFilas: primero hay que elegir la fila
-- Main> eliminaEstrellasFilas (escogeFila mesaInicial 2) 8
-- Exception: eliminaEstrellasFilas: sólo se pueden coger 1, 2 ó 3
--            estrellas
-- Main> eliminaEstrellasFilas (escogeFila mesaInicial 2) 3
-- Exception: eliminaEstrellasFilas: en la fila escogida no hay
--            suficientes estrellas 
-- Main> eliminaEstrellasFilas (escogeFila mesaInicial 2) 1
-- 1: *
-- 2: *
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el segundo jugador
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (2.9) Añadir al inicio del fichero la definición del módulo
-- compartiendo el tipo NimFilas y las funciones solicitadas en los
-- ejercicios anteriores.
-- -------------------------------------------------------------------
