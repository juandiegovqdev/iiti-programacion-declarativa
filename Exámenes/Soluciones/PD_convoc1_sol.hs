-- --------------------------------------------------------------------
-- Ejercicio 1 . 
-- A) Definir una funcion (ramaFinal ar) que, dado un arbol
-- que responde al tipo:
data Arbol a =  N a [Arbol a]
                deriving Show
-- devuelva la rama situada mas a la derecha del arbol, es decir, 
-- la lista de los valores que est�n a la derecha.
-- Un arbol de este tipo, se caracteriza por que de cada nodo sale una
-- lista de subarboles. Cada lista puede tener  longitud
-- distinta, de forma que unos nodos pueden tener cero, uno, dos, tres o
-- mas hijos.
-- Por ejemplo, observa el siguiente arbol:
arbolito = N 3 
                  [(N 4 []),
                   (N 5  
                         [
                          (N 1 [(N 2 []), (N 4 [])]),
                          (N 0 []),
                          (N 1 [(N 3 [])])
                         ]
                   ),
                  (N 7 [(N 4 []), (N 4 [(N 1 [])])])
                  ]
-- La representacion de arbolito es:
--                            3
--                        /   |   \
--                      /     |     \
--                     4      5       7
--                          / | \    / \
--                         /  |  \  |   |  
--                       1    0   1 4   4
--                      / \       |     |
--                     2   4      3     1
-- la funcion devolvera:
-- ramaFinal arbolito == [3,7,4,1]
--
ramaFinal :: Arbol a -> [a]
ramaFinal (N x []) = [x]
ramaFinal (N x ars) = x:ramaFinal (last ars)
--
-- B) Para el mismo tipo de arbol, definir una funcion (ramas ar) que
-- devuelva la lista de ramas del arbol. Por ejemplo:
-- ramas arbolito = [[3,4],
--                  [3,5,1,2],[3,5,1,4],[3,5,1,1],
--                  [3,5,0,3],[3,5,0,1],[3,5,0,5],
--                  [3,5,1],
--                  [3,7,4],[3,7,4],[3,7,4]]
ramas (N x []) = [[x]]
ramas (N x ars) = [x:zs|ys<-ars, zs<-ramas ys]
-- C) Para el mismo tipo de arbol, definir una funcion (mini ar) que
-- devuelva el menor valor almacenado en el arbol ar. Para el ejemplo
-- arbolito: 
-- mini arbolito == 0
mini (N x []) = x
mini (N x ars) = minimum ( x: (map mini ars))

-- --------------------------------------------------------------------
-- Ejercicio 2. 
--  Define POR COMPRENSION Y RECURSION la funcion:
rep :: Int -> [a] -> [a]
-- que repite cada elemento de la lista origen desde 1 hasta n veces
-- respectivamente. Por ejemplo:
-- rep 2 [1,2,3] = [1,2,2]
-- rep 4 [1,2,3] = [1,2,2,3,3,3]
-- rep 3 "abc" = "abbccc"
-- rep 6 [] = []
-- POR COMPRENSION
rep n ys = concat[repite i y |(i,y) <- zip [1 .. n] ys]
repite n x = [x|i<-[1 .. n]]
-- POR RECURSION
rep1 0 _ = []
rep1 _ [] = []
rep1 n xs |n>length xs = rep1 (n-1) xs
          |otherwise = rep1 (n-1) (init xs) ++ (repite n (xs!!(n-1)))
-- --------------------------------------------------------------------
-- Ejercicio 3. 
-- Definir la funcion 
cortainter :: Int -> [a] -> [a]
-- tal que (cortainter n xs) corta la lista por la posicion n y devuelve la
-- lista que resulta de intercalar las listas resultantes. Por ejemplo,
-- cortainter 2 [1,2,3,4,5,6] == [1,3,2,4,5,6]
-- cortainter 4 [1,2,3,4,5,6] == [1,5,2,6,3,4]
-- cortainter 4 [1,2,3] == [1,2,3]
-- cortainter 0 [1,2,3,4,5,6] == [1,2,3,4,5,6]
--
cortainter n xs = inter (take n xs) (drop n xs)
    where inter [] xs = xs
          inter xs [] = xs
          inter (x:xs) (y:ys) = x:y:(inter xs ys)  
-- --------------------------------------------------------------------
-- Ejercicio 4. 
-- Define una funcion 
cuantosaplic :: [(a -> Bool)] -> [a] -> [Int]
-- que recibe una lista de predicados ps y una lista de valores xs y devuelve
-- la lista de las veces que cada predicado p de ps se verifica en los valores
-- de la lista.Por ejemplo:
-- cuantosaplic [(==1), (>1), (<1),even] [1,2,3,0,0,2,4]== [1,4,2,5],
--  ya que en la lista [1,2,3,0,0,2,4] hay un elemento igual a 1, 4
--  elementos mayores que 1, 2 elementos menores que 1 y 5 pares.
-- cuantosaplic [(==1), (>1), (<1),even] [] == [0,0,0,0]
-- cuantosaplic [] [1,2,3,0,0,2,4] == []
cuantosaplic  [] xs = []
cuantosaplic  (p:ps) xs = length (filter (== True)(map p xs))
                          :cuantosaplic ps xs
-- --------------------------------------------------------------------

