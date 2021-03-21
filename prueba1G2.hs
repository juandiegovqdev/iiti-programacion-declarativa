-- ---------------------------------------------------------------------------
-- PD. Grado en Informatica. Tecnologias Informaticas. CURSO 2013-14
-- PRUEBA 1 DE EVALUACION ALTERNATIVA (6 NOVIEMBRE 2013). GRUPO DE TARDE
-- ---------------------------------------------------------------------------
-- ---------------------------------------------------------------------------
-- NOMBRE Y APELLIDOS:
-- GRUPO:
-- EMAIL:
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- EJERCICIO 1. 
-- Se dice que un número natural, n, es reversible si su última cifra NO es 0 
-- y la suma de n y el número que se obtiene escribiendo las cifras de n 
-- en orden inverso tiene todas sus cifras impares.. Por ejemplo,
-- 36 es reversible porque 36 + 63 = 99 y 99 tiene todas sus cifras impares
-- 243 no es reversible porque 243+342 = 585 y 585 no tiene todas sus cifras 
-- impares. 
-- Definir una función esReversible que determine si un número natural es, 
-- o no, reversible.
-- Nota: haga uso de tantas funciones auxiliares como necesite (por ejemplo, 
-- para descomponer un número en cifras).
-- Así, esReversible 36 == True y esReversible 243 == False.



-- ---------------------------------------------------------------------------
-- EJERCICIO 2.
-- Una matriz puede representarse como una lista de listas, donde cada
-- una de las listas representa una fila de la matriz. Por ejemplo:
-- [[1, 0, 2], [0, 3, -1]].
-- Esta matriz no tiene por qué ser numérica.
-- Definir una función que devuelva una lista con los elementos de la diagonal 
-- principal de una matriz cuadrada; es decir, con el mismo número de filas 
-- que de columnas.
-- Así, diagonal [[1,0,-7,5],[3,-1,4,2],[5,6,2,-3],[-8,0,9,4]] == [1,-1,2,4].



-- ---------------------------------------------------------------------------
-- EJERCICIO 3. COMPRENSION
-- Definir una función productoMatricesC que permita calcular el producto de 
-- dos matrices cualesquiera con dimensiones adecuadas. 
-- Utilizar para ello las listas por compresión.
-- Lógicamente, estas matrices deberán ser numéricas.
-- Ejemplo:
-- productoMatricesC [[1,0,-2],[0,3,-1]] [[0,3],[-2,-1],[0,4]] == [[0,-5],[-6,-7]].



-- ---------------------------------------------------------------------------
-- EJERCICIO 4. RECURSION
-- Definir una función recursiva traspuestaR que permita calcular la 
-- traspuesta de una matriz.
-- Esta matriz no tiene por qué ser numérica.
-- Ejemplo:
-- traspuestaR [[0,3],[-2,-1],[0,4]] == [[0,-2,0],[3,-1,4]].


