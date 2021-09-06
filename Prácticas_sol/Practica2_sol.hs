-- ---------------------------------------------------------------------
-- Introducción                                                       --
-- ---------------------------------------------------------------------

-- En esta relación se presentan ejercicios con definiciones elementales
-- (no recursivas) de funciones que usan condicionales, guardas o
-- patrones. 
-- 
-- Estos ejercicios se corresponden con el tema 4

-- ---------------------------------------------------------------------
-- Librerías auxiliares                                             --
-- ---------------------------------------------------------------------

import Test.QuickCheck

-- ---------------------------------------------------------------------
-- Ejercicio 1. Definir la función 
--    divisionSegura :: Double -> Double -> Double
-- tal que (divisionSegura x y) es x/y si y no es cero y 9999 en caso
-- contrario. Por ejemplo,
--    divisionSegura 7 2  ==  3.5
--    divisionSegura 7 0  ==  9999.0
-- ---------------------------------------------------------------------

divisionSegura :: Double -> Double -> Double
divisionSegura _ 0 = 9999
divisionSegura x y = x/y

-- ---------------------------------------------------------------------
-- Ejercicio 2.1. La disyunción excluyente xor de dos fórmulas se
-- verifica si una es verdadera y la otra es falsa. Su tabla de verdad
-- es
--    x     | y     | xor x y
--    ------+-------+---------
--    True  | True  | False 
--    True  | False | True
--    False | True  | True
--    False | False | False
--    
-- Definir la función 
--    xor1 :: Bool -> Bool -> Bool
-- tal que (xor1 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad. Usar 4 ecuaciones, una por cada lónea
-- de la tabla. 
-- ---------------------------------------------------------------------

xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 True False = True
xor1 False True = True
xor1 False False = False

-- ---------------------------------------------------------------------
-- Ejercicio 2.2. Definir la función 
--    xor2 :: Bool -> Bool -> Bool
-- tal que (xor2 x y) es la disyunción excluyente de x e y, calculada a
-- partir de la tabla de verdad y patrones. Usar 2 ecuaciones, una por
-- cada valor del primer argumento. 
-- ---------------------------------------------------------------------

xor2 :: Bool -> Bool -> Bool
xor2 True x = not x
xor2 False x = x

-- ---------------------------------------------------------------------
-- Ejercicio 2.3. Definir la función 
--    xor3 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada 
-- a partir de la disyunción (||), conjunción (&&) y negación (not). 
-- Usar 1 ecuación. 
-- ---------------------------------------------------------------------

xor3 :: Bool -> Bool -> Bool
xor3 x y = (x && not y) || (y && not x)

-- ---------------------------------------------------------------------
-- Ejercicio 2.4. Definir la función 
--    xor4 :: Bool -> Bool -> Bool
-- tal que (xor3 x y) es la disyunción excluyente de x e y, calculada
-- a partir de desigualdad (/=). Usar 1 ecuación.
-- ---------------------------------------------------------------------

xor4 :: Bool -> Bool -> Bool
xor4 x y = x /= y

-- ---------------------------------------------------------------------
-- Ejercicio 2.5. Comprobar con QuickCheck que las cuatros definiciones
-- de xor son equivalentes.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_xor_equivalentes :: Bool -> Bool -> Bool
prop_xor_equivalentes x y =
  xor1 x y == xor2 x y && xor2 x y == xor3 x y && xor3 x y == xor4 x y

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 3. Las dimensiones de los rectángulos puede representarse 
-- por pares; por ejemplo, (5,3) representa a un rectóngulo de base 5 y 
-- altura 3. 
-- 
-- Definir la función 
--    mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
-- tal que (mayorRectangulo r1 r2) es el rectángulo de mayor área ente
-- r1 y r2. Por ejemplo,  
--    mayorRectangulo (4,6) (3,7)  ==  (4,6)
--    mayorRectangulo (4,6) (3,8)  ==  (4,6)
--    mayorRectangulo (4,6) (3,9)  ==  (3,9)
-- ---------------------------------------------------------------------

mayorRectangulo :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
mayorRectangulo r1@(b1,a1) r2@(b2,a2)
  | b1*a1 >= b2*a2 = r1
  | otherwise = r2

-- ---------------------------------------------------------------------
-- Ejercicio 4.1. Definir la función 
--    intercambia :: (a,b) -> (b,a)
-- tal que (intercambia p)  es el punto obtenido intercambiando las
-- coordenadas del punto p. Por ejemplo, 
--    intercambia (2,5)  ==  (5,2)
--    intercambia (5,2)  ==  (2,5)
-- ---------------------------------------------------------------------

intercambia :: (a,b) -> (b,a)
intercambia (x,y) = (y,x)

-- ---------------------------------------------------------------------
-- Ejercicio 4.2. Comprobar con QuickCheck que la función intercambia es
-- idempotente; es decir, si se aplica dos veces es lo mismo que no
-- aplicarla ninguna.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_intercambia :: (Int,Int) -> Bool
prop_intercambia p = intercambia (intercambia p) == p

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 5.1. Definir la función 
--    distancia :: (Double,Double) -> (Double,Double) -> Double
-- tal que (distancia p1 p2) es la distancia entre los puntos p1 y
-- p2. Por ejemplo, 
--    distancia (1,2) (4,6)  ==  5.0
-- ---------------------------------------------------------------------
 
distancia :: (Double,Double) -> (Double,Double) -> Double
distancia (x1,y1) (x2,y2) = sqrt ((x1-x2)**2 + (y1-y2)**2)

-- ---------------------------------------------------------------------
-- Ejercicio 5.2. Comprobar con QuickCheck que se verifica la propiedad
-- triangular de la distancia; es decir, dados tres puntos p1, p2 y p3,
-- la distancia de p1 a p3 es menor o igual que la suma de la distancia
-- de p1 a p2 y la de p2 a p3.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_triangular :: (Double,Double) -> (Double,Double) -> (Double,Double)
                -> Bool
prop_triangular p1 p2 p3 =
  distancia p1 p3 <= (distancia p1 p2 + distancia p2 p3)

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 6.1. Definir una función 
--    ciclo :: [a] -> [a]
-- tal que (ciclo xs) es la lista obtenida permutando cíclicamente los
-- elementos de la lista xs, pasando el último elemento al principio de
-- la lista. Por ejemplo, 
--    ciclo [2,5,7,9]  == [9,2,5,7]
--    ciclo []         == []
--    ciclo [2]        == [2]
-- ---------------------------------------------------------------------

ciclo :: [a] -> [a]
ciclo [] = []
ciclo [x] = [x]
ciclo xs = last xs : init xs

-- ---------------------------------------------------------------------
-- Ejercicio 6.2. Comprobar que la longitud es un invariante de la
-- función ciclo; es decir, la longitud de (ciclo xs) es la misma que la
-- de xs.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_ciclo :: [Int] -> Bool 
prop_ciclo xs = length xs == length (ciclo xs)

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función 
--    numeroMayor :: (Num a, Ord a) => a -> a -> a
-- tal que (numeroMayor x y) es el mayor número de dos cifras que puede
-- construirse con los dígitos x e y. Por ejemplo,  
--    numeroMayor 2 5 ==  52
--    numeroMayor 5 2 ==  52
-- ---------------------------------------------------------------------

numeroMayor :: (Num a, Ord a) => a -> a -> a
numeroMayor x y
  | x > y = x*10 + y
  | otherwise = y*10 + x

-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función 
--    numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
-- tal que (numeroDeRaices a b c) es el número de raďż˝ces reales de la
-- ecuación a*x^2 + b*x + c = 0. Por ejemplo,
--    numeroDeRaices 2 0 3    ==  0
--    numeroDeRaices 4 4 1    ==  1
--    numeroDeRaices 5 23 12  ==  2
-- ---------------------------------------------------------------------

numeroDeRaices :: (Num t, Ord t) => t -> t -> t -> Int
numeroDeRaices a b c
  | r < 0 = 0
  | r == 0 = 1
  | otherwise = 2
    where r = b^2 - 4*a*c

-- ---------------------------------------------------------------------
-- Ejercicio 9.1. Definir la función 
--    raices :: Double -> Double -> Double -> [Double]
-- tal que (raices a b c) es la lista de las raďż˝ces reales de la
-- ecuación ax^2 + bx + c = 0. Por ejemplo, 
--    raices 1 3 2    ==  [-1.0,-2.0]
--    raices 1 (-2) 1 ==  [1.0,1.0]
--    raices 1 0 1    ==  []
-- ---------------------------------------------------------------------

raices :: Double -> Double -> Double -> [Double]
raices a b c 
    | d >= 0    = [(-b+e)/t,(-b-e)/t]
    | otherwise = []
    where d = b**2 - 4*a*c
          e = sqrt d
          t = 2*a

-- ---------------------------------------------------------------------
-- Ejercicio 9.2. Definir el operador
--    (~=) :: (Fractional a, Ord a) => a -> a -> Bool
-- tal que (x ~= y) se verifica si x e y son casi iguales; es decir si
-- el valor absoluto de su diferencia es menor que una milďż˝sima. Por
-- ejemplo, 
--    12.3457 ~= 12.3459  ==  True
--    12.3457 ~= 12.3479  ==  False
-- ---------------------------------------------------------------------

(~=) :: (Fractional a, Ord a) => a -> a -> Bool
x ~= y = abs (x-y) < 0.001

-- ---------------------------------------------------------------------
-- Ejercicio 10. En geometría, la fďż˝rmula de Herón, descubierta por
-- Herón de Alejandría, dice que el ďż˝rea de un trióngulo cuyo lados
-- miden a, b y c es la raďż˝z cuadrada de s(s-a)(s-b)(s-c) donde s es el
-- semiperďż˝metro 
--    s = (a+b+c)/2
-- 
-- Definir la función 
--    area :: Double -> Double -> Double -> Double 
-- tal que (area a b c) es el ďż˝rea del trióngulo de lados a, b y c. Por
-- ejemplo, 
--    area 3 4 5  ==  6.0
-- ---------------------------------------------------------------------

area :: Double -> Double -> Double -> Double 
area a b c = sqrt (s*(s-a)*(s-b)*(s-c))
    where s = (a+b+c)/2
        
-- ---------------------------------------------------------------------
-- Ejercicio 11.1. Los intervalos cerrados se pueden representar mediante
-- una lista de dos números (el primero es el extremo inferior del
-- intervalo y el segundo el superior). 
-- 
-- Definir la función 
--    interseccion :: Ord a => [a] -> [a] -> [a]
-- tal que (interseccion i1 i2) es la intersección de los intervalos i1 e
-- i2. Por ejemplo,
--    interseccion [] [3,5]     ==  []
--    interseccion [3,5] []     ==  []
--    interseccion [2,4] [6,9]  ==  []
--    interseccion [2,6] [6,9]  ==  [6,6]
--    interseccion [2,6] [0,9]  ==  [2,6]
--    interseccion [2,6] [0,4]  ==  [2,4]
--    interseccion [4,6] [0,4]  ==  [4,4]
--    interseccion [5,6] [0,4]  ==  []
-- ---------------------------------------------------------------------

interseccion :: Ord a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion _ [] = []
interseccion [a1,b1] [a2,b2]
    | a <= b    = [a,b]
    | otherwise = []
    where a = max a1 a2
          b = min b1 b2
interseccion _ _ = error "Imposible"

-- ---------------------------------------------------------------------
-- Ejercicio 11.2. Comprobar con QuickCheck que la intersección de
-- intervalos es conmutativa.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_interseccion :: Int -> Int -> Int -> Int -> Bool
prop_interseccion a1 b1 a2 b2 =
  interseccion [a1,b1] [a2,b2] == interseccion [a2,b2] [a1,b1]

-- La comprobación es

-- ---------------------------------------------------------------------
-- Ejercicio 12.1. Los números racionales pueden representarse mediante
-- pares de números enteros. Por ejemplo, el número 2/5 puede
-- representarse mediante el par (2,5). 
-- 
-- Definir la función 
--    formaReducida :: (Int,Int) -> (Int,Int) 
-- tal que (formaReducida x) es la forma reducida del número racional
-- x. Por ejemplo, 
--    formaReducida (4,10)  ==  (2,5)
-- ---------------------------------------------------------------------

formaReducida :: (Int,Int) -> (Int,Int) 
formaReducida (0,_) = (0,1)
formaReducida (a,b) = (x * signum (a*b), y)
    where c = gcd a b
          x = abs (a `div` c)
          y = abs (b `div` c)

-- ---------------------------------------------------------------------
-- Ejercicio 12.2. Definir la función 
--    sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (sumaRacional x y) es la suma de los números racionales x e
-- y, expresada en forma reducida. Por ejemplo, 
--    sumaRacional (2,3) (5,6)  ==  (3,2)
--    sumaRacional (3,5) (-3,5) ==  (0,1)
-- ---------------------------------------------------------------------
 
sumaRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaRacional (a,b) (c,d) = formaReducida (a*d+b*c, b*d)
 
-- ---------------------------------------------------------------------
-- Ejercicio 12.3. Definir la función
--    productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
-- tal que (productoRacional x y) es el producto de los números
-- racionales x e y, expresada en forma reducida. Por ejemplo, 
--    productoRacional (2,3) (5,6)  ==  (5,9)
-- ---------------------------------------------------------------------
 
productoRacional :: (Int,Int) -> (Int,Int) -> (Int,Int)
productoRacional (a,b) (c,d) = formaReducida (a*c, b*d)
 
-- ---------------------------------------------------------------------
-- Ejercicio 12.4. Definir la función
--    igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
-- tal que (igualdadRacional x y) se verifica si los números racionales
-- x e y son iguales. Por ejemplo, 
--    igualdadRacional (6,9) (10,15)  ==  True
--    igualdadRacional (6,9) (11,15)  ==  False
--    igualdadRacional (0,2) (0,-5)   ==  True
-- ---------------------------------------------------------------------
 
igualdadRacional :: (Int,Int) -> (Int,Int) -> Bool
igualdadRacional (a,b) (c,d) = 
    a*d == b*c
    
-- ---------------------------------------------------------------------
-- Ejercicio 12.5. Comprobar con QuickCheck la propiedad distributiva
-- del producto racional respecto de la suma.
-- ---------------------------------------------------------------------

-- La propiedad es
prop_distributiva :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Property
prop_distributiva x@(_,d1) y@(_,d2) z@(_,d3) =
  d1>0 && d2>0 && d3>0 ==> igualdadRacional prodSuma sumaProd
  where prodSuma = productoRacional x (sumaRacional y z)
        sumaProd = sumaRacional (productoRacional x y) (productoRacional x z)

-- La comprobación es
