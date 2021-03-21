-- Programación Declarativa 2019/20
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- Parcial 1 Solución                               24 de Octubre de 2019
-- ----------------------------------------------------------------------
-- Apellidos: Doe
-- Nombre: John
-- UVUS: dojo
-- ----------------------------------------------------------------------
-- INSTRUCCIONES
-- · Antes de continuar, cambie el nombre de este archivo por:
--                   Parcial1_<codigo>_<uvus>.hs
--   donde "<uvus>" es su usuario virtual de la Universidad de Sevilla, y
--   "<codigo>" es el código alfanumérico de arriba.
-- · Escriba la solución de cada ejercicio en el hueco reservado para
--   ello.
-- · Asegúrese de utilizar correctamente el nombre y el tipo indicado
--   para cada función solicitada. Puede añadir tantas funciones
--   auxiliares (incluyendo el tipo adecuadamente) como necesite,
--   describiendo su objetivo.
-- · Una vez finalizado el examen, comprima el fichero .hs en un .tar.gz
--   con el siguiente nombre:
--                  ENTREGA-<uvus>.tar.gz
--   dejándolo en el escritorio. Reiniciar el equipo y en el menú de
--   selección del sistema elija "Enviar examen".
-- · Después, ponga sus apellidos, nombre y UVUS en el enunciado en papel,
--   entréguela al profesor, y compruebe que se ha entregado sin problema.
-- ----------------------------------------------------------------------

import Test.QuickCheck
import Data.List

-- ----------------------------------------------------------------------
-- Ejercicio 1.1 (2 puntos)
-- Definir el operador infijo (*&) tal que reciba y devuelva números
-- reales de doble precisión. El cálculo realizado es la multiplicación
-- de los dos argumentos redondeado al siguiente entero. El operador debe
-- tener la misma precedencia que la multiplicación (*), y tener la
-- asociatividad necesaria (derecha o izquierda) para que los siguientes
-- ejemplos sean correctos:
-- λ> 2.3 *& 2.1 == 5.0 
-- λ> 5.0 *& 0.5 *& 0.5 == 2.0
-- λ> 5.0 *& (0.5 *& 0.5) == 5.0
--
-- Nota: la función 'ceiling' puede ser de ayuda.

(*&) :: Double -> Double -> Double
a *& b = fromIntegral (ceiling (a*b))
infixl 7 *&   -- es asociativo por la izquierda, precedencia de (*) = 7
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 2.1 (2 puntos)
-- Definir la función (intervalos a b xs), donde xs es una lista de posiciones
-- en una recta (números reales) que puede venir desordenada, a y b son dos
-- valores reales que definen un intervalo en dicha recta. Necesitamos que la
-- función devuelva una terna donde la primera lista sean los elementos de xs
-- por debajo de a, la segunda lista sean los elementos de xs que estén entre a
-- y b (incluídos), y la tercera lista los mayores que b. Las tres listas
-- resultantes deben estar ordenadas. Visualmente, queremos (zs,ys,ks) tal que:
--
--  -x-x--a-x-xx---b---x-x
--        |        |    
--    zs  |   ys   |  ks  
--
-- Por ejemplo:
-- λ> intervalos 3 5 [1..10] == ([1,2],[3,4,5],[6,7,8,9,10])
-- λ> intervalos 3 5 [10,9..1] ==  ([1,2],[3,4,5],[6,7,8,9,10])
-- λ> intervalos 0.0 10.0 [9.5,10.0,-3,0.3,5.0,-10.56,114.5] ==
-- ([-10.56,-3.0],[0.3,5.0,9.5,10.0],[114.5])

-- Solución con recursión
  
intervalos :: Ord a => a -> a -> [a] -> ([a],[a],[a])
intervalos a b xs = intervalos' a b (sort xs) ([],[],[])

intervalos' :: Ord a => a -> a -> [a] -> ([a], [a], [a]) -> ([a], [a], [a])
intervalos' _ _ [] p = p
intervalos' a b (x:xs) (ys,zs,ks)
  | x < a = intervalos' a b xs (ys++[x],zs,ks)
  | a <=x && x <= b = intervalos' a b xs (ys,zs++[x],ks)
  | otherwise = intervalos' a b xs (ys,zs,ks++[x])

-- Solución con comprensión:
intervalosC :: Ord a => a -> a -> [a] -> ([a],[a],[a])
intervalosC a b xs = (ys,zs,ks)
  where os = sort xs
        ys = [x | x <- os, x < a]
        zs = [x | x <- os, a <= x && x <= b]
        ks = [x | x <- os, b < x]

-- Solución con orden superior:
intervalosO ::(Ord a) => a -> a -> [a] -> ([a],[a],[a])
intervalosP a b xs = (takeWhile (<a) xs', takeWhile (<=b) xs'', xs''')
  where xs' = sort xs
        xs''= dropWhile (<a) xs'
        xs'''= dropWhile (<=b) xs''

-- Ejercicio 2.2. (1 punto)
-- Comprobar con quickCheck si se cumple lo siguiente: xs es igual al intervalo
-- comprendido entre el mínimo y el máximo de xs calculado mediante la función
-- intervalos.

prop_intervalos :: Ord a => [a] -> Bool
prop_intervalos xs = (xs == zs)
  where (_,zs,_) = intervalos (minimum xs) (maximum xs) xs

-- Falsificable con [1.0,0.0]. En efecto, devuelve listas vacias. Si queremos
-- comprobar si están los mismos elementos, entonces habría que ordenar xs
-- en la comprobación.
-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------
-- Ejercicio 3. (2,5 puntos)
-- La función (zigzag xs) sobre una lista cambia el orden de los elementos de
-- xs tal que el primero de xs va al comienzo, el segundo de xs al final, el
-- tercero en la segunda posición, el cuarto en la penúltima posición, etc. Es
-- decir,
-- λ> zigzag [1..5] == [1,3,5,4,2]
-- λ> zigzag [1..10] == [1,3,5,7,9,10,8,6,4,2]
-- λ> zigzag "abcdefghi" == "acegihfdb"
-- Se pide definir dicha función de tres formas distintas:
-- 1) Usando recursión (zigzagR xs)
-- 2) Usando comprensión (zigzagC xs)
-- 3) Usando orden superior (zigzagO xs)

-- Solución recursión 1
zigzag :: [a] -> [a]
zigzag xs = zigzagR xs 0
  where zigzagR [] _ = []
        zigzagR (x:xs) i
          | i==0 = x:zigzagR xs ((i+1) `mod` 2)
          | i==1 = (zigzagR xs ((i+1) `mod` 2)) ++ [x]
          
-- Solución recursión 2
zigzag' :: [a] -> [a]
zigzag' [] = []
zigzag' (x:xs) = x: reverse (zigzag' xs)

-- Solución comprensión (idea: primero las posiciones impares, y luego las pares en orden
-- inverso)
zigzagC :: [a] -> [a]
zigzagC xs = posicionesImpares ++ reverse (posicionesPares)
  where posicionesImpares =  [ x | (x,i) <- zip xs [1..], odd i]
        posicionesPares =  [ x | (x,i) <- zip xs [1..], even i]

-- Solución orden superior (idea: primero las posiciones impares, y luego las pares en orden
-- inverso)
zigzagOS :: [a] -> [a]
zigzagOS xs = map fst (posicionesImpares ++ posicionesPares)
  where zs = zip xs [1..]
        posicionesImpares = filter (odd.snd) zs   -- una forma de hacer el
                            -- filter con composición de funciones
        posicionesPares = filter (\(x,i) -> even i) zs  -- otra forma de hacer
                            -- el filter con funciones lambda

-- ----------------------------------------------------------------------

-- ----------------------------------------------------------------------  
-- Ejercicio 4.1. (1,5 puntos)
-- Definir la función (maxListas xss) tal que reciba una lista de listas xss, y devuelva una lista
-- de pares (i,j), donde i es el índice de la sublista no vacía i-ésima dentro
-- de xss y j es la posición donde sucede el máximo de la sublista i-ésima, por
-- ejemplo, 
-- λ> maxListas [[4,2],[7],[2,8,1],[10,20]] == [(1,1),(2,1),(3,2),(4,2)]
-- λ> maxListas [[1,2,3],[],[0,4,1],[10]] == [(1,3),(3,2),(4,1)]

maxListas :: Ord a => [[a]] -> [(Int,Int)]
maxListas xss = [ (i,j) | (xs,i) <- zip xss [1..], (x,j) <- zip xs [1..], x == maximum xs ]

-- Ejercicio 4.2. (1 punto)
-- Definir la función (listaIncrementales xs) que reciba una lista de pares
-- como la calculada en el ejercicio anterior. Dado que cada par (i,j) indica
-- el índice de la lista, i, y la posición de su máximo, j, la función debe
-- devolver True si no falta ninguna lista i (porque era vacía), y la posición
-- donde sucede el máximo en la lista i es siempre mayor o igual que la
-- posición donde sucede en la lista i-1. Por ejemplo,
-- λ> listasIncrementales [(1,1),(2,3),(3,3),(4,5)] == True
-- λ> listasIncrementales [(1,1),(2,3),(4,3),(5,5)] == False
-- λ> listasIncrementales [(1,1),(2,3),(3,3),(4,2)] == False

listasIncrementales :: [(Int,Int)] -> Bool
listasIncrementales xs = and [ (i==i'-1) && (j<=j') | ((i,j),(i',j')) <- zip xs (tail xs) ]
-- ----------------------------------------------------------------------
