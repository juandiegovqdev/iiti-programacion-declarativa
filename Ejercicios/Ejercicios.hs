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
-- Ejercicio 3 (2 ptos).
-- -------------------------------------------------------------------
-- Una lista de números enteros se llama alternada si sus elementos
-- son alternativamente par/impar o impar/par.
-- -------------------------------------------------------------------
-- (3.1) Definir la función, utilizando el plegado foldl,
--    alternada :: [Int] -> Bool
-- tal que (alternada xs) reconoce si xs es una lista alternada.
-- Por ejemplo: 
--    alternada [1,2,3]     == True
--    alternada [1,2,3,4]   == True
--    alternada [8,1,2,3,4] == True
--    alternada [8,1,2,3]   == True
--    alternada [8]         == True
--    alternada [7]         == True
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- Generalizando, una lista es alternada respecto de un predicado p si
-- sus elementos verifican alternativamente el predicado p.
-- -------------------------------------------------------------------
-- (3.2) Definir la función, utilizando el plegado foldr,
--    alternadaG :: (a -> Bool) -> [a] -> Bool
-- tal que (alternadaG p xs) compruebe si xs es una lista alternada
-- respecto de p. Por ejemplo,
--    alternadaG (>0) [-2,1,3,-9,2]  == False
--    alternadaG (>0) [-2,1,-3,9,-2] == True
--    alternadaG (<0) [-2,1,-3,9,-2] == True
--    alternadaG even [8,1,2,3]      == True
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (3.3) Redefinir la función alternada usando alternadaG y comprobar
-- con QuickCheck que ambas definiciones coinciden.
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
-- (3.4) Un número de la suerte es un número natural que se genera por
-- una criba como se indica a continuación:
-- 
-- Se comienza con la lista de los números enteros a partir de 1:
--    1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25...
-- Se eliminan los números de dos en dos
--    1,  3,  5,  7,  9,   11,   13,   15,   17,   19,   21,   23,   25...
-- Como el segundo número que ha quedado es 3, se eliminan los números
-- restantes de tres en tres:  
--    1,  3,      7,  9,         13,   15,         19,   21,         25...
-- Como el tercer número que ha quedado es 7, se eliminan los números
-- restantes de siete en siete:   
--    1,  3,      7,  9,         13,   15,               21,         25...
-- 
-- Este procedimiento se repite indefinidamente y los supervivientes son
-- los números de la suerte:  
--    1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79
--
-- Definir la sucesión
--    numerosDeLaSuerte :: [Int]
-- cuyos elementos son los números de la suerte. Por ejemplo,
--    ghci> take 20 numerosDeLaSuerte
--    [1,3,7,9,13,15,21,25,31,33,37,43,49,51,63,67,69,73,75,79]
--    ghci> numerosDeLaSuerte !! 1500
--    13995
-- -------------------------------------------------------------------

-- Solución:

-- -------------------------------------------------------------------
