-- Programación Declarativa
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- 2 Parcial                                      29 de Noviembre 2017
-- -------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- -------------------------------------------------------------------
-- AVISOS IMPORTANTES
-- · Antes de continuar, cambie el nombre de este archivo por:
--                   <uvus>Ejercicio2.hs
--   donde <uvus> debe ser su usuario virtual.
-- · Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrese de utilizar exactamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite
--   describiendo claramente su objetivo.
-- -------------------------------------------------------------------

import Test.QuickCheck
import CodeWorld

-- -------------------------------------------------------------------

-- -------------------------------------------------------------------
-- Ejercicio 1.
-- -------------------------------------------------------------------
-- (1.1) Definir una función

anillos :: [Double] -> Picture
anillos ls = undefined

-- tal que, dada una lista numérica, devuelva un dibujo de anillo
-- concéntricos siendo los números de la lista los distintos radios.
-- drawingOf (anillos [1,7,5,3]) == Figura (1)

-- -------------------------------------------------------------------
-- (1.2) Definir una función

anillosColor :: [Double] -> [Color] -> Picture
anillosColor = undefined

-- tal que, dada una lista numérica (con valores crecientes) y otra de
-- colores (ambas de la misma longitud), devuelva un dibujo de anillos
-- concéntricos cada uno de ellos del color indicado. Utiliza las
-- listas por comprensión para resolver el ejercicio.

-- drawingOf (anillosColor [1,3,5,7] [red, green, magenta, azure])
-- Figura (2)

-- -------------------------------------------------------------------
-- Ejercicio 2.
-- -------------------------------------------------------------------
-- (2.1) Definir una función, utilizando recursión,

alterna :: [a] -> [a] -> [a]
alterna = undefined

-- tal que (alterna xs ys) es la lista obtenida intercalando los
-- elementos de xs e ys. Por ejemplo,
--   alterna [5,1] [2,7,4,9]     == [5,2,1,7,4,9]
--   alterna [5,1,7] [2..10]     == [5,2,1,3,7,4,5,6,7,8,9,10]
--   alterna [2..10] [5,1,7]     == [2,5,3,1,4,7,5,6,7,8,9,10]
--   alterna [2,4..12] [1,5..28] == [2,1,4,5,6,9,8,13,10,17,12,21,25]

-- -------------------------------------------------------------------
-- (2.2) Comprobar con QuickCheck que el número de elementos de
-- (alterna xs ys) es la suma de los números de elementos de xs e ys.

-- -------------------------------------------------------------------
-- Ejercicio 3. Definir la función

resultadoPositivo :: (Num b, Ord b) => (a -> b) -> [a] -> [a]
resultadoPositivo = undefined

-- tal que (resultadoPositivo f xs) es la lista de los elementos de la
-- lista xs tales que el valor de la función f sobre ellos es
-- positivo. Por ejemplo,
-- resultadoPositivo head [[-1,2],[-9,4],[2,3]]       == [[2,3]]
-- resultadoPositivo sum [[1,2],[9],[-8,3],[],[3,-5]] == [[1,2],[9]]
-- resultadoPositivo (+ 3.5) [1,2,-9,-8.3,-3,-1]      == [1,2,-3,-1]
-- Definir esta función
-- 1) por recursión (utilizando guardas),
-- 2) por plegado (con 'foldr'),
-- 3) por recursión terminal

-- -------------------------------------------------------------------
-- Ejercicio 4. Definir la función

alternos :: (a -> b) -> (a -> b) -> [a] -> [b]
alternos = undefined

-- tal que (alternos f g xs) es la lista obtenida aplicando
-- alternativamente las funciones f y g a los elementos de la lista
-- xs. Por ejemplo, 
-- alternos (+1) (*3) [1,2,3,4,5]                   == [2,6,4,12,6]
-- alternos (take 2) reverse ["todo","para","nada"]
--   == ["to","arap","na"]

-- -------------------------------------------------------------------
-- Ejercicio 5. Definir la función

buscaCrucigrama :: Char -> Int -> Int -> [String] -> [String]
buscaCrucigrama = undefined

-- tal que (buscaCrucigrama x i l ps) es la lista de los elementos de
-- la lista de palabras ps, que tienen longitud l y tienen la letra x
-- en la posición 'i' (comenzando en 0). Por ejemplo,
-- buscaCrucigrama 'c' 1 7 ["ocaso", "casa", "ocupado"] == ["ocupado"]
-- Definir esta función utilizando filter y una función anónima.

-- -------------------------------------------------------------------
-- Ejercicio 6. Definir la función

tresDiferentes :: (Eq a) => a -> a -> a -> Bool
tresDiferentes = undefined

-- tal que (tresDiferentes x y z) se verifica si los elementos x, y y
-- z son distintos. Por ejemplo,
--    tresDiferentes 3 5 2  ==  True
--    tresDiferentes 3 5 3  ==  False

-- -------------------------------------------------------------------
-- Ejercicio 7. Los resultados de las votaciones a delegado en un
-- grupo de clase se recogen mediante listas de asociación.
-- -------------------------------------------------------------------
-- (7.1) Definir el tipo Resultados de tal forma que la siguiente
-- definición sea correcta.

-- votos :: Resultados
-- votos =  [("Ana Perez",10),("Juan Lopez",7), ("Julia Rus", 27),
--           ("Pedro Val",1), ("Pedro Ruiz",27),("Berta Gomez",11)]
 
-- -------------------------------------------------------------------
-- (7.2) Define la función

-- mayorV :: Resultados -> Int
-- mayorV = undefined

-- tal que (mayorV xs) devuelve el número de votos obtenido por los
-- ganadores de la votación cuyos resultados se recogen en xs. Por
-- ejemplo,
--     mayorV votos == 27

-- b) Define la función 

-- ganadores :: Resultados -> [String]
-- ganadores = undefined

-- tal que (ganadores xs) es la lista de los estudiantes con mayor
-- número de votos en xs. Por ejemplo,
--     ganadores votos == ["Julia Rus","Pedro Ruiz"]

-- -------------------------------------------------------------------
-- Ejercicio 8.
-- -------------------------------------------------------------------
-- (8.1) Definir

falsos :: [Bool]
falsos = undefined

-- una lista infinita en la que todos los elementos son False.
-- take 6 falsos == [False,False,False,False,False,False]
-- take 11 falsos ==
-- [False,False,False,False,False,False,False,False,False,False,False]

-- -------------------------------------------------------------------
-- (8.2) Definir la función

indicesVerdaderos :: [Int] -> [Bool]
indicesVerdaderos = undefined

-- tal que (indicesVerdaderos xs) es la lista infinita de booleanos
-- tal que sólo son verdaderos los elementos cuyos índices pertenecen
-- a la lista estrictamente creciente xs. Por ejemplo,
-- take 6 (indicesVerdaderos [1,4])
-- == [False,True,False,False,True,False]
-- take 6 (indicesVerdaderos [0,2..])
-- == [True,False,True,False,True,False]
-- take 3 (indicesVerdaderos [])      == [False,False,False]
-- take 6 (indicesVerdaderos [1..])
-- == [False,True,True,True,True,True]
-- -------------------------------------------------------------------
