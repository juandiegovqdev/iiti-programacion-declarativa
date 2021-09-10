{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import DefinicionesNim

-- -------------------------------------------------------------------
-- Ejercicio 1
-- -------------------------------------------------------------------
-- (1.1) Definir los tipos Eleccion (con dos valores posibles para
-- indicar si tras la última selección de estrellas se ha podido
-- aplicar el cambio o no, al no haber suficientes estrellas) y Estado
-- (para guardar la información de la partida en cada instante y como
-- ha sido la última elección, datos de tipo Nim y Eleccion
-- respectivamente). Utilizar data para la definición de los tipos
-- solicitados.
-- -------------------------------------------------------------------

-- Solución:

data Eleccion = Correcta | Insuficientes
data Estado = E Nim Eleccion

-- -------------------------------------------------------------------
-- Definición alternativa del tipo Estado si no se ha resuelto el
-- ejercicio anterior. 

-- type Estado = (Nim, Bool)

-- El primer elemento de la tupla será la información sobre la
-- partida y el segundo la elección (True si ha sido correcta, False
-- en caso contrario).

-- Nota:
-- En las soluciones de los ejercicios restantes se incluirá una
-- versión de las mismas para este tipo.
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (1.2) Definir una función, utilizando listas por compresión,

pintaEstado :: Estado -> Picture

-- tal que, dado un estado e, devuelva un dibujo con la representación
-- de la situación de la partida contenida en e (ver ejemplos en el
-- enunciado).
-- -------------------------------------------------------------------

pintaEstado (E n _)
  | estrellasRestantes n == 0 = pintaGanador (jugador n)
pintaEstado (E n Correcta) = pintaComun n
pintaEstado (E n _) = pintaComun n & pintaIncorrecta

-- Para el tipo alternativo sería:
-- pintaEstado (n, _)
--   | estrellasRestantes n == 0 = pintaGanador (jugador n)
-- pintaEstado (n, True) = pintaComun n
-- pintaEstado (n, _) = pintaComun n & pintaIncorrecta

-- Las siguientes funciones auxiliares son independientes de la
-- definición del tipo Estado

pintaGanador :: Int -> Picture
pintaGanador 1 = text "¡Gana el jugador 2!"
pintaGanador 2 = text "¡Gana el jugador 1!"

pintaComun :: Nim -> Picture
pintaComun n = 
    translated 0 2.5 (pintaEstrellas r) &
    translated 0 1.5 (pintaTurno j) &
    text "Elige la cantidad de estrellas" &
    translated 0 (-1) (scaled 0.6 0.6 (text "Usa las teclas 1, 2 ó 3"))    
  where r = estrellasRestantes n
        j = jugador n

pintaIncorrecta :: Picture
pintaIncorrecta = translated 0 (-2) (scaled 0.6 0.6 (text "Elige menos"))

pintaEstrellas :: Int -> Picture
pintaEstrellas n =
  pictures [translated ((-10.5) + (fromIntegral x)) 0 (text "*") |
            x <- [1..n]]

pintaTurno :: Int -> Picture
pintaTurno 1 = text "Turno del jugador 1"
pintaTurno 2 = text "Turno del jugador 2"

-- -------------------------------------------------------------------
-- (1.3) Definir tres estados

est1, est2, est3 :: Estado

-- con la información que se incluye en la descripción de las figuras
-- 1, 2 y 3 (respectivamente) del enunciado. Las siguientes
-- expresiones deben dar lugar a unas imágenes parecidas a las del
-- enunciado.

-- Main> drawingOf (pintaEstado est1)
-- Main> drawingOf (pintaEstado est2)
-- Main> drawingOf (pintaEstado est3)
-- -------------------------------------------------------------------

-- Solución:

est1 = (E (eliminaEstrellas (inicio 4) 3) Correcta)
est2 = (E (eliminaEstrellas (inicio 4) 3) Insuficientes)
est3 =
  (E (eliminaEstrellas (eliminaEstrellas (inicio 4) 3) 1) Correcta)

-- Para el tipo alternativo sería:
-- est1 = (eliminaEstrellas (inicio 4) 3, True)
-- est2 = (eliminaEstrellas (inicio 4) 3, False)
-- est3 = (eliminaEstrellas (eliminaEstrellas (inicio 4) 3) 1, True)

-- -------------------------------------------------------------------
-- (1.4) Definir una función, utilizando guardas y patrones, 

manejaEvento :: Event -> Estado -> Estado

-- tal que, que dado un evento de teclado con los números 1, 2 ó 3 y
-- un estado e (en el que aún queden estrellas), devuelva el estado
-- que resulta al eliminar dicha cantidad de fichas si hay bastantes ó
-- la misma situación del juego que la contenida en e pero con una
-- selección insuficiente si no hay bastantes. Para cualquier otro
-- evento o si no quedan estrellas, el estado no cambia. Las
-- siguientes expresiones deben dar lugar a unas imágenes parecidas a
-- las 2 y 3 del enunciado.

-- fig2 = drawingOf (pintaEstado (manejaEvento (KeyPress "3") est1))
-- fig3 = drawingOf (pintaEstado (manejaEvento (KeyPress "1") est2))
-- -------------------------------------------------------------------

-- Solución:

manejaEvento _ e@(E n _) | estrellasRestantes n == 0 = e
manejaEvento (KeyPress "1") (E n _) =
  (E (eliminaEstrellas n 1) Correcta)
manejaEvento (KeyPress "2") (E n _)
  | r > 1 = (E (eliminaEstrellas n 2) Correcta)
  | otherwise = (E n Insuficientes)
  where r = estrellasRestantes n
manejaEvento (KeyPress "3") (E n _)
  | r > 2 = (E (eliminaEstrellas n 3) Correcta)
  | otherwise = (E n Insuficientes)
  where r = estrellasRestantes n
manejaEvento _ e = e

-- Para el tipo alternativo sería:
-- manejaEvento _ e@(n, _) | estrellasRestantes n == 0 = e
-- manejaEvento (KeyPress "1") (n, _) = ((eliminaEstrellas n 1), True)
-- manejaEvento (KeyPress "2") (n, _)
--   | r > 1 = ((eliminaEstrellas n 2), True)
--   | otherwise = (n, False)
--   where r = estrellasRestantes n
-- manejaEvento (KeyPress "3") (n, _)
--   | r > 2 = ((eliminaEstrellas n 3), True)
--   | otherwise = (n, False)
--   where r = estrellasRestantes n
-- manejaEvento _ e = e

-- -------------------------------------------------------------------

nim =
  interactionOf (E (inicio 20) Correcta)
  (\_ e -> e) manejaEvento pintaEstado 