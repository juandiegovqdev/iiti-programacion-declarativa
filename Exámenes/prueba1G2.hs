-- ---------------------------------------------------------------------------
-- PD. Grado en Informatica. Tecnologias Informaticas. CURSO 2013-14
-- PRUEBA 1 DE EVALUACION ALTERNATIVA (6 NOVIEMBRE 2013). GRUPO DE TARDE
-- ---------------------------------------------------------------------------
-- EJERCICIO 1. 
-- Se dice que un n�mero natural, n, es reversible si su �ltima cifra NO es 0 
-- y la suma de n y el n�mero que se obtiene escribiendo las cifras de n 
-- en orden inverso tiene todas sus cifras impares.. Por ejemplo,
-- 36 es reversible porque 36 + 63 = 99 y 99 tiene todas sus cifras impares
-- 243 no es reversible porque 243+342 = 585 y 585 no tiene todas sus cifras 
-- impares. 
-- Definir una funci�n esReversible que determine si un n�mero natural es, 
-- o no, reversible.
-- Nota: haga uso de tantas funciones auxiliares como necesite (por ejemplo, 
-- para descomponer un n�mero en cifras).
-- As�, esReversible 36 == True y esReversible 243 == False.
-- ---------------------------------------------------------------------------



-- ---------------------------------------------------------------------------
-- EJERCICIO 2.
-- Una matriz puede representarse como una lista de listas, donde cada
-- una de las listas representa una fila de la matriz. Por ejemplo:
-- [[1, 0, 2], [0, 3, -1]].
-- Esta matriz no tiene por qu� ser num�rica.
-- Definir una funci�n que devuelva una lista con los elementos de la diagonal 
-- principal de una matriz cuadrada; es decir, con el mismo n�mero de filas 
-- que de columnas.
-- As�, diagonal [[1,0,-7,5],[3,-1,4,2],[5,6,2,-3],[-8,0,9,4]] == [1,-1,2,4].
-- ---------------------------------------------------------------------------



-- ---------------------------------------------------------------------------
-- EJERCICIO 3. COMPRENSION
-- Definir una funci�n productoMatricesC que permita calcular el producto de 
-- dos matrices cualesquiera con dimensiones adecuadas. 
-- Utilizar para ello las listas por compresi�n.
-- L�gicamente, estas matrices deber�n ser num�ricas.
-- Ejemplo:
-- productoMatricesC [[1,0,-2],[0,3,-1]] [[0,3],[-2,-1],[0,4]] == [[0,-5],[-6,-7]].
-- ---------------------------------------------------------------------------



-- ---------------------------------------------------------------------------
-- EJERCICIO 4. RECURSION
-- Definir una funci�n recursiva traspuestaR que permita calcular la 
-- traspuesta de una matriz.
-- Esta matriz no tiene por qu� ser num�rica.
-- Ejemplo:
-- traspuestaR [[0,3],[-2,-1],[0,4]] == [[0,-2,0],[3,-1,4]].
-- ---------------------------------------------------------------------------


