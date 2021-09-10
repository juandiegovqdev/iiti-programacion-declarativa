-- --------------------------------------------------------------------
-- EJERCICIO 1. Define, utilizando funciones de orden superior, el valor 
-- de la suma de todos cuadrados impares menores que 10000. Llamala p.
-- --------------------------------------------------------------------

p = undefined

-- --------------------------------------------------------------------
-- EJERCICIO 2. Define por plegado una funcion sumbaP tal que 
-- (sumbaP f xss) devuelve la lista formada por las listas de xss tales
-- que, al evaluar f sobre ellas, devuelve un valor positivo.
-- 
-- Usala para calcular p1, que sera la lista de listas de  
-- [[1 2 -3], [4,-5,-1],[4,1,2,-5,-6]]
-- que cumplen que la suma de los elementos que ocupan posiciones pares
-- es negativa o cero.

-- sumbaP head [[1,2],[0,9,4],[-2,3]] == [[1,2]]
-- sumbaP sum [[1,2],[0,9,4],[-8,3],[],[-1,1],[3,4,5]] == 
-- [[1,2],[0,9,4],[3,4,5]]
--- --------------------------------------------------------------------

sumbaP = undefined
                                                
p1 = undefined

-- --------------------------------------------------------------------
-- EJERCICIO 3. Definir una función 
--    cumplen :: (a -> Bool) -> [[a]] -> [a]
-- tal que (cumlen p xss) devuelve la lista de xss que tiene más
-- elementos que cumplen p. Por ejemplo,
--
-- cumplen even [[1..5], [2,4..20], [3,2,1,4,8]] == 
-- [2,4,6,8,10,12,14,16,18,20]
-- --------------------------------------------------------------------

cumplen :: (a -> Bool) -> [[a]] -> [a]
cumplen = undefined

-- --------------------------------------------------------------------
-- EJERCICIO 4.
-- Se define el tipo de dato:
type Var = Char
data Form = V  Var | No Form | O Form Form | Y Form Form deriving Show
-- para representar las fórmulas de la lógica proposicional.
--
-- A)   Define la funcion:
-- sust :: (Var, Var) -> Form -> Form
-- tal que (sust (x,y) g) devuelve la formula que resulta de sustituir
-- cada ocurrencia  de la variable x por la variable y en la formula g.
-- por ejemplo: 
-- al sustituir 'p' por 'q' en la formula (-p) \/ (r \/ p) resulta 
-- (-q) \/ (r \/ q).
-- 
-- B)  Representa la formula (llamala formulita) del ejemplo segun 
-- el tipo de dato Form. ¿Que devuelve la funcion sust al sustituir 
-- 'p' por 'q' en dicha formula?
--
-- C)  Define una funcion negacion, tal que (negacion g) devuelva la 
-- negacion de la formula g, sabiendo que la negacion de la
-- negacion es la propia formula, la negacion de la disyuncion
-- es la conjuncion de las disyunciones, la negacion de la
-- conjuncion es la disyuncion de las negaciones y la negacion
-- de una variable es la formula negada.
--
-- D)  Calcula la negacion de la formula (-p) \/ (r \/ p)
-- --------------------------------------------------------------------
sust :: (Var, Var) -> Form -> Form
sust = undefined

formulita :: Form 
formulita = undefined

negacion :: Form -> Form
negacion = undefined