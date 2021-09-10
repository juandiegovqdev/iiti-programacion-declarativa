-- --------------------------------------------------------------------
-- Ejercicio 1. Definir POR COMPRENSION Y RECURSION la funcion
--   igualDistancia :: Eq a => a -> [a] -> Bool
-- tal que (igualDistancia x xs) se verifica si hay instancias de x en
-- xs que esten a igual distancia del principio que del final de la lista.
-- Por ejemplo, 
-- igualDistancia 2 [3,2,5,2,7]          ==  True ya que el dos est� a
-- un elemento del principio y a un elemento del final de la lista.
-- igualDistancia 3  [1,3,3,4,7] ==  True
-- igualDistancia 3  [1,3,4] == True
-- igualDistancia 5  [1,3,4] == False
-- igualDistancia 3  [1,3,2,3,5,3,4,3,7] == True
-- igualDistancia 5  [1,3,2,3,5,3,4,3,7] == True
-- igualDistancia 2  [1,3,2,3,5,3,4,3,7] == False
-- igualDistancia 'a' "galan" == True
-- igualDistancia 'n' "galan" == False
-- RECURSION
igualDistancia _ [] = False
igualDistancia y [x] = x==y
igualDistancia y (x:z:xs) = (y == x && last (z:xs) == y ) 
                            || igualDistancia y (init (z:xs))
-- COMPRENSION
igD x xs = not(null [n | n<- [0 .. length xs - 1]
                    ,xs!!n == x 
                    ,xs!!n == (reverse xs)!!n])
--  --------------------------------------------------------------------
-- Ejercicio 2. ARBOLES BINARIOS.
-- Consideremos el tipo de dato 
data Arbol1 a = H1 a | N1 a (Arbol1 a) (Arbol1 a) deriving Show
-- y el tipo de dato:
data Arbol2 a = H2 (a,a) | N2 (a,a) (Arbol2 a) (Arbol2 a) deriving Show
-- Ambos tipos de arboles tienen datos almacenados en hojas y nodos,
-- pero en el primero son de tipo a, por ejemplo, booleanos, enteros,
-- strings, etc. y, en el segundo, almacenamos pares de booleanos, pares
-- de enteros, pares de strings, etc.
-- Definir la funcion 
parImpar :: Num a =>  Arbol1 a -> Arbol2 a
-- tal que, dado un arbol de tipo Arbol1, lo convierte en un arbol de
-- tipo Arbol2 cambiando cada elemento x del primero por el par obtenido
-- con el anterior y el siguiente de x. 
-- Por ejemplo: la funci�n parImpar aplicada al arbol 
ar1 = N1 3     (H1 1)     (N1 6     (N1 5     (H1 2)     (H1 5))     (H1 3))
-- devuelve el siguiente:
ar2 = N2 (2,4) (H2 (0,2)) (N2 (5,7) (N2 (5,6) (H2 (1,3)) (H2 (4,6))) (H2 (2,4)))
parImpar (H1 x) = H2 (x-1,x+1)
parImpar (N1 n ai ad) = N2 (n-1,n+1) (parImpar ai) (parImpar ad)

-- ---------------------------------------------------------------------
-- Ejercicio 3. EVALUACION PEREZOSA Y LISTAS INFINITAS.
-- Consideremos la sucesion definida a partir de un
-- numero d, de forma que cada elemento es la suma del anterior mas el
-- producto de sus digitos no nulos. Por ejemplo,
-- Si d = 1, la sucesion es 1,2,4,8,16,22,26,38,62,74,102,104, ...
-- Si d = 15, la sucesion es 15,20,22,26,38,62,74,102,104,108,...

-- Definir, la funcion
--   sucesion:: Integer -> [Integer]
-- tal que (sucesion d) es la sucesion anterior, empezando por d.
sucesion x = x: [y+ prodNN y|y<- sucesion x]
prodNN x = product (digitosNN x)
digitosNN x | x==0 = [1]
            | x<10 = [x]
            | otherwise = digitosNN (mod x 10)  ++ digitosNN (div x 10)

-- --------------------------------------------------------------------
-- --------------------------------------------------------------------
-- RESUELVE SOLO UNO DE LOS EJERCICIOS SIGUIENTES: EL 4A O BIEN EL 4B:
-- --------------------------------------------------------------------
-- Ejercicio 4A. Un grafo se puede representar mediante una lista de
-- pares. Cada par contiene en la primera componente un vertice y en la
-- segunda, la lista de los vertices conectados con ese vertice. 
-- Por ejemplo, el grafo 
--    1 ----- 2
--    | \     |
--    |  3    |
--    | /     |
--    4 ----- 5
-- se representa por
--    ejGrafo = [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3,5]),(5,[2,4])]   
-- Definir la funcion
--    conexiones :: Ord a => [(a,[a])] -> [(a,a)]
-- tal que (conexiones g) es la lista de conexiones del grafo g. 
-- Por ejemplo, 
--    conexiones ejGrafo  ==  [(1,2),(1,3),(1,4),(2,5),(3,4)]
-- Observar que cada conexion (x,y) es la misma que la (y,x), por tanto
-- solo debe aparecer una vez.
ejGrafo = [(1,[2,3,4]),(2,[1,5]),(3,[1,4]),(4,[1,3]),(5,[2,4])]   

conexiones :: Ord a => [(a,[a])] -> [(a,a)]
conexiones g = concat[[(z,y)|y<-xs, z<y] | (z,xs)<- g]
-- ---------------------------------------------------------------------
-- Ejercicio 4B. PROLOG.
-- COPIA LA SOLUCI�N EN UN FICHERO DE EXTENSION .pl para comprobar.
-- Define el predicado duplica(L1,L2) que se verifique si la lista L2
-- se obtiene duplicando los elementos de L1. Por ejemplo,
--    ?- duplica([a,b,c],L).
--    L = [a,a,b,b,c,c] ;
--    No
-- ---------------------------------------------------------------------
-- duplica([], []).
-- duplica([X|L1], [X,X|L2]) :- duplica(L1, L2).
-- ---------------------------------------------------------------------

