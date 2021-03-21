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

{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import DefinicionesNim

-- -------------------------------------------------------------------
-- Ejercicio 1 (3'5 ptos).
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

-- -------------------------------------------------------------------
-- Definición alternativa del tipo Estado si no se ha resuelto el
-- ejercicio anterior. 

-- type Estado = (Nim, Bool)

-- El primer elemento de la tupla será la información sobre la
-- partida y el segundo la elección (True si ha sido correcta, False
-- en caso contrario).
-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- (1.2) Definir una función, utilizando listas por compresión,

-- pintaEstado :: Estado -> Picture

-- tal que, dado un estado e, devuelva un dibujo con la representación
-- de la situación de la partida contenida en e (ver ejemplos en el
-- enunciado).
-- -------------------------------------------------------------------

-- Solución:


-- -------------------------------------------------------------------
-- (1.3) Definir tres estados

-- est1, est2, est3 :: Estado

-- con la información que se incluye en la descripción de las figuras
-- 1, 2 y 3 (respectivamente) del enunciado. Las siguientes
-- expresiones deben dar lugar a unas imágenes parecidas a las del
-- enunciado.

-- Main> drawingOf (pintaEstado est1)
-- Main> drawingOf (pintaEstado est2)
-- Main> drawingOf (pintaEstado est3)
-- -------------------------------------------------------------------

-- Solución:


-- -------------------------------------------------------------------
-- (1.4) Definir una función, utilizando guardas y patrones, 

-- manejaEvento :: Event -> Estado -> Estado

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

-- -------------------------------------------------------------------
