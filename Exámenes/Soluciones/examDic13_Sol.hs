-- ------------------------------------------------------------------
-- EJERCICIO 1.1: (0,5 PUNTOS)
-- Definir el tipo Cuadrado para representar un par de números
-- naturales, el tamaño de la base y la altura de un cuadrado.
-- SOLUCIÓN: 

type Cuadrado = (Int, Int)

-- ------------------------------------------------------------------
-- EJERCICIO 1.2: (0,5 PUNTOS)
-- Definir el tipo ArbolC para representar árboles con ramificación
-- binaria que en sus hojas almacenan cuadrados y con nodos sin 
-- información.
-- SOLUCIÓN: 

data ArbolC = HojaC Cuadrado | RamasC ArbolC ArbolC

-- ------------------------------------------------------------------
-- EJERCICIO 1.3: (1 PUNTO)
-- Definir la función hojaMayor para calcular el cuadrado de mayor
-- tamaño almacenado en un árbol.
-- SOLUCIÓN: 

hojaMayorC :: ArbolC -> Int
hojaMayorC (HojaC (x, y)) = x*y
hojaMayorC (RamasC  x y) = max (hojaMayorC x) (hojaMayorC y)

-- ------------------------------------------------------------------
-- EJERCICIO 1.4: (1 PUNTO)
-- Calcular el valor de hojaMayorC aplicada al arbol:
                       --         .
                       --       /   \
                       --     /       \
                       -- (1,2)          .
                       --              /   \
                       --            /       \
                       --           .           .
                       --         /   \       /  \
                       --     (2,3)  (1,3) (4,5) (1,2)    
-- SOLUCIÓN:                           
-- *Main> hojaMayorC (RamasC (HojaC (1,2)) 
--                           (RamasC (RamasC (HojaC (2, 3))(HojaC (1,3))) 
--                                   (RamasC (HojaC (4,5)) (HojaC (1,2)))))
-- 20

-- ------------------------------------------------------------------
-- EJERCICIO 2.1: (1 PUNTO)
-- Dado el siguiente tipo de árboles.

data ArbolL a = HojaL [a] | NodoL [ArbolL a]

-- Definir una funcion elementos tal que (elementos ar) devuelva una
-- lista con la unión de los elementos que aparecen en un arbol ar.

elementos :: ArbolL a -> [a]
elementos (HojaL x) = x
elementos (NodoL xs) = concat (map elementos xs)

-- ------------------------------------------------------------------
-- EJERCICIO 2.2: (1 PUNTO)
-- Calcular los elementos del arbol:
                     --           .
                     --       /   |   \
                     --      /    |     \
                     --     /     |       \
                     -- [1,2]     .         .
                     --           |       /   \
                     --           |      /     \   
                     --           |     /       \
                     --          [ ] [1,2,3] [1,2,3]
-- SOLUCION:
-- elementos (NodoL [HojaL [1,2], 
--                   NodoL [], 
--                   NodoL [HojaL [1,2,3], HojaL [1,2,3]]])

-- [1,2,1,2,3,1,2,3]

-- --------------------------------------------------------------------- 
-- EJERCICIO 3: (1,5 PUNTOS)
-- Definir una funcion peta tal que (peta xss) devuelve un par formado
-- por la lista de los primeros elementos de las listas no vacias de xss 
-- y la lista de los ultimos elementos de las listas no vacias de xs.
-- Ejemplo:
-- peta [[1,2],[2,3,4],[],[3,4,5,6],[],[9]] == ([1,2,3,9],[2,4,6,9])

peta [] = ([],[])
peta xss = (map head yss ,map last yss)
  where yss = borra [] xss
borra xs [] = []
borra xs (zs:zss) |xs == zs = borra xs zss
                  |otherwise = zs:(borra xs zss)
                               
-- ---------------------------------------------------------------------
-- EJERCICIO 4: (1,5 PUNTOS)
-- Supongamos que queremos convertir la sucesión ordenada
-- de las páginas en las que aparece un concepto en la entrada de dicho
-- concepto en el índice del libro. Por ejemplo, la sucesión 1, 3, 4, 6,
-- 7, 8, 9, 12, 13, 14 se transforma en 1, 3-4, 6-9, 12-14. Definir la
-- función 
--    indice :: [Int] -> [[Int]]
-- tal que (indice xs) la lista obtenida aplicando la transformación
-- anterior a la sucesión xs. Por ejemplo,
--    indice [1,3,4,6,7,8,9,12,13,14]  => [[1],[3,4],[6,9],[12,14]]
-- ---------------------------------------------------------------------


indice :: [Int] -> [[Int]]
indice []  = []
indice [x] = [[x]]
indice (x:y:xs) | x+1 == y  = [x,last ys]:(tail yss)
                | otherwise = [x]:yss
                where yss = indice (y:xs)
                      ys  = head yss