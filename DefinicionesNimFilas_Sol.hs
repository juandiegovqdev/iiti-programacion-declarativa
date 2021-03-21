module DefinicionesNimFilas_Sol
       (NimFilas, inicioFilas, estrellasRestantesFilas, jugadorFilas,
        fila, escogeFila, eliminaEstrellasFilas) where

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

data Jugador = Uno | Dos
             -- deriving Show

type Mesa = [Int]

data Fila = No | Si Int
          -- deriving Show

-- La definición de los tipos Jugador y Fila puede completarse
-- incluyéndolos en la clase Show y definiendo la función show.

-- instance Show Jugador where
--   show j = "Tiene el turno el " ++ showJugador j ++ " jugador" 

-- instance Show Fila where
--   show No = ""
--   show (Si n) = "\n(que va a eliminar de la fila " ++ show n ++ ")"

-- -------------------------------------------------------------------
-- (2.2) Definir el tipo NimFilas, utilizando data, para la
-- información sobre el jugador que tiene el turno, la cantidad de
-- estrellas en cada fila y la fila escogida. Dicho tipo debe
-- pertenecer a la clase Show, por lo que hay que incluir una
-- definición de la función show.
-- -------------------------------------------------------------------

-- Solución:

data NimFilas = NF Jugador Mesa Fila
              -- deriving Show
instance Show NimFilas where
  show (NF j m f) =
-- El tipo Mesa ya pertenece a la clase Show (por ser [Int]).
    -- show m ++ "\n" ++
    showMesa m ++
-- En el caso en el que el tipo Jugador se haya incluido en la clase Show.
   -- show j ++ 
    "Tiene el turno el " ++ showJugador j ++ " jugador" ++
-- En el caso en el que el tipo Fila se haya incluido en la clase Show.
   -- show f ++ 
    showFila f

showMesa :: Mesa -> String
showMesa est = concat [show i ++ ": " ++ replicate c '*' ++ "\n" |
                       (i, c) <- zip [1..] est]

showJugador :: Jugador -> String
showJugador Uno = "primer"
showJugador _ = "segundo"

showFila :: Fila -> String
showFila No = ""
showFila (Si n) = "\n(que va a eliminar de la fila " ++ show n ++ ")"

-- > NF Uno [1..5] (Si 6)
-- 1: *
-- 2: **
-- 3: ***
-- 4: ****
-- 5: *****
-- Tiene el turno el primer jugador
-- (que va a eliminar de la fila 6)
-- > NF Dos [1,2,2,0,3] No
-- 1: *
-- 2: **
-- 3: **
-- 4: 
-- 5: ***
-- Tiene el turno el segundo jugador

-- -------------------------------------------------------------------
-- Definición alternativa del tipo NimFilas si no se han resuelto los
-- ejercicios anteriores. 

-- type NimFilas = (Int, [Int], Int)

-- El primer elemento de la tupla será el jugador: 1 ó 2, el segundo
-- una lista con la cantidad de estrellas en cada fila de la mesa; y
-- el tercero: 0 si no se ha escogido fila o el índice de la fila
-- escogida.

-- Nota:
-- En las soluciones de los ejercicios restantes se incluirá una
-- versión de las mismas para este tipo.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (2.3) Definir una función,

inicioFilas :: Int -> NimFilas

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

inicioFilas n = NF Uno [1..n] No

-- Para el tipo alternativo sería:
-- inicioFilas n = (1, [1..n], 0)

-- -------------------------------------------------------------------
-- (2.4) Definir una función,

estrellasRestantesFilas :: NimFilas -> [Int]

-- tal que, dada la información sobre un instante de la partida,
-- devuelve una lista con la cantidad de estrellas que hay, sobre la
-- mesa, en cada una de las filas. 

-- Main> estrellasRestantesFilas mesaInicial
-- [1,2,3,4,5]
-- -------------------------------------------------------------------

-- Solución:

estrellasRestantesFilas (NF _ m _) = m

-- Para el tipo alternativo sería:
-- estrellasRestantesFilas (_, m, _) = m

-- -------------------------------------------------------------------
-- (2.5) Definir una función,

jugadorFilas :: NimFilas -> Int

-- tal que, dada la información sobre un instante de la partida,
-- devuelve 1 si tiene el turno el primer jugador y 2 en caso
-- contrario.

-- Main> jugadorFilas mesaInicial
-- 1
-- -------------------------------------------------------------------

-- Solución:

jugadorFilas (NF Uno _ _) = 1
jugadorFilas _ = 2

-- Para el tipo alternativo sería:
-- jugadorFilas (j, _, _) = j

-- -------------------------------------------------------------------
-- (2.6) Definir una función,

fila :: NimFilas -> Maybe Int

-- tal que, dada la información sobre un instante de la partida,
-- devuelve el índice de la fila escogida.

-- Main> fila mesaInicial
-- Nothing
-- -------------------------------------------------------------------

-- Solución:

fila (NF _ _ No) = Nothing
fila (NF _ _ (Si n)) = Just n

-- Para el tipo alternativo sería:
-- fila (_, _, 0) = Nothing
-- fila (_, _, n) = Just n

-- -------------------------------------------------------------------
-- (2.7) Definir una función, utilizando guardas,

escogeFila :: NimFilas -> Int -> NimFilas

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

escogeFila (NF j m No) i
  | not (i `elem` [1..(length m)]) =
    error "escogeFila: índice incorrecto"
  | m !! (i - 1) == 0 =
    error "escogeFila: no quedan estrellas en la fila escogida"
  | otherwise = (NF j m (Si i))
escogeFila _ _ = error "escogeFila: ya se ha escogido una fila"

-- Para el tipo alternativo sería:
-- escogeFila (j, m, 0) i
--   | not (i `elem` [1..(length m)]) =
--     error "escogeFila: índice incorrecto"
--   | m !! (i - 1) == 0 =
--     error "escogeFila: no quedan estrellas en la fila escogida"
--   | otherwise = (j, m, i)
-- escogeFila _ _ = error "escogeFila: ya se ha escogido una fila"

-- -------------------------------------------------------------------
-- (2.8) Definir una función,

eliminaEstrellasFilas :: NimFilas -> Int -> NimFilas

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

eliminaEstrellasFilas (NF _ _ No) c =
  error "eliminaEstrellasFilas: primero hay que elegir la fila"
eliminaEstrellasFilas (NF j m (Si i)) c
  | not (c `elem` [1..3]) =
      error "eliminaEstrellasFilas: sólo se pueden coger 1, 2 ó 3 estrellas"
  | m !! (i - 1) < c =
    error "eliminaEstrellasFilas: en la fila escogida no hay suficientes estrellas"
--  | otherwise = (NF (contrario j) (quita c i (zip m [1..])) No)
  | otherwise = (NF (contrario j) (elimina c i m) No)

contrario :: Jugador -> Jugador
contrario Uno = Dos
contrario _ = Uno

-- Para el tipo alternativo sería:
-- eliminaEstrellasFilas (_, _, 0) c =
--   error "eliminaEstrellasFilas: primero hay que elegir la fila"
-- eliminaEstrellasFilas (j m i) c
--   | not (c `elem` [1..3]) =
--       error "eliminaEstrellasFilas: sólo se pueden coger 1, 2 ó 3 estrellas"
--   | m !! (i - 1) < c =
--     error "eliminaEstrellasFilas: en la fila escogida no hay suficientes estrellas"
-- --  | otherwise = ((contrario j), (quita c i (zip m [1..])), 0)
--   | otherwise = ((contrario j), (elimina c i m), 0)

-- contrario :: Int -> Int
-- contrario 1 = 2
-- contrario _ = 1

-- ---------------------------
-- Las siguientes funciones auxiliares son independientes de la
-- definición del tipo NimFila

-- ---------------------------
-- La función auxiliar quita, dada una cantidad c, un índice i y una
-- lista ls de pares de la forma (entero, posición); devuelve la lista
-- de enteros que resulta al restar c al entero i-ésimo de ls sin
-- modificar los restantes.
-- Por ejemplo:
-- elimina 3 1 (zip [4,5,8] [1..]) == [1,5,8]
-- elimina 3 2 (zip [4,5,8] [1..]) == [4,2,8]
-- elimina 3 3 (zip [4,5,8] [1..]) == [4,5,5]

-- quita :: Int -> Int -> [(Int, Int)] -> [Int]

-- ---------------------------
-- Primera versión: recursiva.
-- ---------------------------
-- quita c i [] = []
-- quita c i ((x, j):ls)
--   | i == j = (x-c):(quita c i ls)
--   | otherwise = x:(quita c i ls)

-- ---------------------------
-- Segunda versión: plegado de la anterior.
-- ---------------------------
-- quita c i =
--   foldr (\(x, j) fls -> if i == j then (x-c):fls else x:fls) []

-- ---------------------------
-- La función auxiliar elimina, dada una cantidad c, una posición i de
-- una lista de números enteros y dicha lista ls; devuelve la lista que
-- resulta al restar c al elemento i-ésimo de ls sin modificar los
-- restantes.
-- Por ejemplo:
-- elimina 3 1 [4,5,8] == [1,5,8]
-- elimina 3 2 [4,5,8] == [4,2,8]
-- elimina 3 3 [4,5,8] == [4,5,5]

elimina :: Int -> Int -> [Int] -> [Int]

-- ---------------------------
-- Primera versión: recursiva, tanto en el índice como en la lista.
-- ---------------------------
-- elimina c 1 (x:ls) = (x-c):ls
-- elimina c i (x:ls) = x:(eliminaAux c (i-1) ls)

-- ---------------------------
-- Segunda versión: utilizando listas por compresión.
-- ---------------------------
-- elimina c i ls =
--   [if i == j then (x-c) else x | (x, j) <- zip ls [1..]]

-- ---------------------------
-- Tercera versión: aplicativa
-- ---------------------------
elimina c i ls = take (i-1) ls ++ (x-c):rs
  where (x:rs) = drop (i-1) ls

-- -------------------------------------------------------------------
-- (2.9) Añadir al inicio del fichero la definición del módulo
-- compartiendo el tipo NimFilas y las funciones solicitadas en los
-- ejercicios anteriores.
-- -------------------------------------------------------------------
