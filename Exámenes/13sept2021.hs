-- Ejercicio 1
distanciaHamming :: [Integer] -> [Integer] -> Int
distanciaHamming _ [] = 0
distanciaHamming [] _ = 0
distanciaHamming (x:xs) (y:ys)
    | x == y    = distanciaHamming xs ys
    | otherwise = 1 + distanciaHamming xs ys 

-- Ejercicio 2
-- cotaCardinalC :: Float -> Float -> Float


-- Ejercicio 3
-- distanciaLee :: Integer -> 

-- Ejercicio 4


-- Ejercicio 5


-- Ejercicio 6


-- Ejercicio 7


-- Ejercicio 8


-- Ejercicio 9


-- Ejercicio 10

