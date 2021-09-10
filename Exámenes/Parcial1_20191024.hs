import Test.QuickCheck

-- ----------------------------------------------------------------------
-- Ejercicio 1.1
-- Definir el operador infijo (*&) tal que reciba y devuelva números
-- reales de doble precisión. El cálculo realizado es la multiplicación
-- de los dos argumentos redondeado al siguiente entero. El operador debe
-- tener la misma precedencia que la multiplicación (*), y tener la
-- asociatividad necesaria (derecha o izquierda) para que los siguientes
-- ejemplos sean correctos:
-- λ> 2.3 *& 2.1 == 5.0 
-- λ> 5.0 *& 0.5 *& 0.5 == 2.0
-- λ> 5.0 *& (0.5 *& 0.5) == 5.0
-- Nota: la función 'ceiling' puede ser de ayuda.
-- ----------------------------------------------------------------------



-- ----------------------------------------------------------------------
-- Ejercicio 2.1 
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

-- Ejercicio 2.2.
-- Comprobar con quickCheck si se cumple lo siguiente: xs es igual al intervalo
-- comprendido entre el mínimo y el máximo de xs calculado mediante la función
-- intervalos.
-- ----------------------------------------------------------------------

intervalos :: Int -> Int -> [Int] -> ([Int], [Int], [Int])
intervalos c b xs = (menores, entreIntervalo, mayores)
    where menores = menoresQue c xs
          entreIntervalo = entreNumeros c b (drop (length menores) xs)
          mayores = mayoresQue (length menores + length entreIntervalo) xs

menoresQue :: Int -> [Int] -> [Int]
menoresQue _ [] = []
menoresQue a (x:xs)
    | x < a = x : menoresQue a xs
    | otherwise = []

entreNumeros :: Int -> Int -> [Int] -> [Int]
entreNumeros _ _ [] = []
entreNumeros a b (x:xs)
    | x >= a && x <= b = x : entreNumeros a b xs
    | otherwise = []

mayoresQue :: Int -> [Int] -> [Int]
mayoresQue 0 xs = xs
mayoresQue c (x:xs) = mayoresQue (c-1) xs

-- ----------------------------------------------------------------------
-- Ejercicio 3. 
-- La función (zigzag xs) sobre una lista cambia el orden de los elementos de
-- xs tal que el primero de xs va al comienzo, el segundo de xs al final, el
-- tercero en la segunda posición, el cuarto en la penúltima posición, etc. Es
-- decir,
-- λ> zigzag [1..5] == [1,3,5,4,2]
-- λ> zigzag [1..10] == [1,3,5,7,9,10,8,6,4,2]
-- λ> zigzag "abcdefghi" == "acegihfdb"
-- Se pide definir dicha función de tres formas distintas, usando un tipado
-- polimórfico:
-- ----------------------------------------------------------------------

-- 1) Usando recursión (zigzagR xs)
zigzagR :: [Int] -> [Int]
zigzagR xs = zigzagROdd xs ++ reverse (zigzagREven xs)

zigzagREven :: [Int] -> [Int]
zigzagREven [] = []
zigzagREven (x:xs)
    | even x = x : zigzagREven xs 
    | otherwise = zigzagREven xs

zigzagROdd :: [Int] -> [Int]
zigzagROdd [] = []
zigzagROdd (x:xs)
    | odd x = x : zigzagROdd xs 
    | otherwise = zigzagROdd xs

-- 2) Usando comprensión (zigzagC xs)
zigzagC :: [Int] -> [Int]
zigzagC xs = [x | x <- xs, odd x] ++ reverse [x | x <- xs, even x]

-- 3) Usando orden superior (zigzagO xs)
zigzagO :: [Int] -> [Int]
zigzagO xs = filter odd xs ++ reverse (filter even xs)

-- ----------------------------------------------------------------------  
-- Ejercicio 4.1. 
-- Definir la función (maxListas xss) tal que reciba una lista de listas xss,
-- y devuelva una lista de pares (i,j), donde i es el índice de la sublista no
-- vacía i-ésima dentro de xss y j es la posición donde sucede el máximo de la
-- sublista i-ésima, por ejemplo, 
-- λ> maxListas [[4,2],[7],[2,8,1],[10,20]] == [(1,1),(2,1),(3,2),(4,2)]
-- λ> maxListas [[1,2,3],[],[0,4,1],[10]] == [(1,3),(3,2),(4,1)]

-- TODO: Intentar de nuevo
maxListas :: (Ord a, Num a) => [[a]] -> [(a,a)]
maxListas [] = []
maxListas ((x:xs):xss) = (obtenerIndice xs x 1 1, obtenerMayor xs x) :  maxListas xss

obtenerMayor :: Ord a => [a] -> a -> a
obtenerMayor [] b = b
obtenerMayor (x:xs) b
    | b < x = obtenerMayor xs x
    | otherwise = obtenerMayor xs b

obtenerIndice :: (Ord a, Num a) => [a] -> a -> a -> a -> a
obtenerIndice [] _ indF _ = indF + 1
obtenerIndice (x:xs) a indF ind 
    | a < x = obtenerIndice xs x (indF+1) (ind+1)
    | otherwise = obtenerIndice xs a (indF) (ind+1)

-- Ejercicio 4.2. 
-- Definir la función (listaIncrementales xs) que reciba una lista de pares
-- como la calculada en el ejercicio anterior. Dado que cada par (i,j) indica
-- el índice de la lista, i, y la posición de su máximo, j, la función debe
-- devolver True si no falta ninguna lista i (porque era vacía), y la posición
-- donde sucede el máximo en la lista i es siempre mayor o igual que la
-- posición donde sucede en la lista i-1. Por ejemplo,
-- λ> listasIncrementales [(1,1),(2,3),(3,3),(4,5)] == True
-- λ> listasIncrementales [(1,1),(2,3),(4,3),(5,5)] == False
-- λ> listasIncrementales [(1,1),(2,3),(3,3),(4,2)] == False
-- ----------------------------------------------------------------------

listasIncrementales :: [(Int, Int)] -> Bool
listasIncrementales [] = True
listasIncrementales ((a, b):(c, d):xs)
    | c == (a+1) && b <= d = listasIncrementales ((c, d):xs)
    | otherwise = False
listasIncrementales ((c, d):_) = True
