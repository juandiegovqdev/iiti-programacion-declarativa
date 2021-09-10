import Test.QuickCheck
import Data.Char

-- ----------------------------------------------------------------------
-- Ejercicio 1 (1,5 puntos)
-- Definir el operador infijo (~>=) tal que reciba números de cualquier 
-- tipo de la clase fractional, y devuelva un Booleano indicando si el 
-- primer argumento es aproximadamente igual al segundo con una precisión
-- del 0.001, o bien mayor que él. Se debe dar explícitiamente la misma
-- precedencia que a la comparación (>=) y asociatividad por la derecha.
-- Por ejemplo:
-- > 2 ~>= 2.001 == True
-- > 2 ~>= 2.001 && 1.01 ~>= 1 == True
-- > 1-2.1 ~>= 2.3 || 2 ~>= 2.01 == False
-- ----------------------------------------------------------------------

(~>=) :: (Ord a,Fractional a) => a -> a -> Bool   -- además de fractional, necesitamos ord
x ~>= y = (abs(x-y) <= 0.001) || (x>y)
infixr 4 ~>=   -- precedencia como >= y asociatividad por la derecha

-- ---------------------------------------------------------------------
-- Ejercicio 2 (3 puntos). El centro de masas de un sistema discreto de cuerpos
-- es el punto del espacio que se comporta como si en él estuviera aplicada
-- la resultante de las fuerzas externas al sistema.
--
-- Representaremos un conjunto de n cuerpos en un espacio mediante una lista
-- de n pares de la forma (mi,(ai,bi,ci)) donde (ai,bi,ci) es la posición
-- en el espacio tridimensional, y mi la masa puntual. Las coordenadas 
-- del centro de masas (a,b,c) se calculan como
--    a = (a1*m1+a2*m2+ ... an*mn)/(m1+m2+...mn)
--    b = (b1*m1+b2*m2+ ... bn*mn)/(m1+m2+...mn)
--    c = (c1*m1+c2*m2+ ... cn*mn)/(m1+m2+...mn)
--
-- Ejercicio 2.1. (1,5 puntos)
-- Define la función (centroDeMasas xs) tal que reciba una lista de
-- cuerpos como descrito arriba, y devuelva la coordenada del centro de masas.
-- La función debe ser polimórfica, aceptando tipos de clase Floating.
-- Si la lista está vacía, debe dar un error como el mostrado abajo.
-- Por ejemplo:
--  > centroDeMasas [(3.8,(-1,3,0.9)),(5.9,(0,0,0)),(0.9,(1.5,3,0))] 
--  (-0.2311320754716981,1.3301886792452828,0.32264150943396225)
--  > centroDeMasas []
--  *** Exception: No hay cuerpos
-- ---------------------------------------------------------------------

centroDeMasas :: Floating a => [(a,(a,a,a))] -> (a,a,a)
centroDeMasas xs 
    | null xs = error "No hay cuerpos"          -- si la lista es vacía
    | otherwise = (sum [a*m | (m,(a,_,_)) <- xs] / mt,  -- Primera componente
                   sum [b*m | (m,(_,b,_)) <- xs] / mt,  -- Segunda componente
                   sum [c*m | (m,(_,_,c)) <- xs] / mt)  -- Tercera componente
      where mt = sum [m | (m,_) <- xs]          -- suma de la masa total

-- ---------------------------------------------------------------------
-- Ejercicio 2.2 (1,5 punto)
-- Comprueba con quickcheck la siguiente propiedad: para todo conjunto
-- de n cuerpos, con n>1, cuyas masas son positivas (estrictamente mayor que
-- cero), las coordenadas del centro de masas están dentro del rango de
-- las coordenadas de los cuerpos; es decir, la componente a de la coordenada
-- del centro de masas está entre el mínimo y el máximo (ambos inclusive)
-- de las componentes ai de los cuerpos (ídem para las componentes b y c).
-- Por ejemplo:
-- > quickCheck prop_masas
-- *** Gave up! Passed only 26 tests; 1000 discarded tests.
-- ---------------------------------------------------------------------

prop_masas ::  (Ord a,Floating a) => [(a,(a,a,a))]  -> Property
prop_masas xs = length xs > 1 && and [m>0| (m,_) <- xs] ==>       -- condición para hacer la comprobación
                 a >= minimum as && a <= maximum as && b >= minimum bs && b <= maximum bs && c >= minimum cs && c <= maximum cs 
    where (a,b,c) = centroDeMasas xs
          as = [a | (_,(a,_,_)) <- xs]  -- Sacamos primeras coordenadas
          bs = [b | (_,(_,b,_)) <- xs]  -- Sacamos segundas coordenadas
          cs = [c | (_,(_,_,c)) <- xs]  -- Sacamos terceras coordenadas


-- ---------------------------------------------------------------------
-- Ejercicio 3. (3 puntos)
-- Dada una señal codificada en una lista de números reales,
-- necesitamos buscar los máximos locales, los cuales se calculan como
-- sigue: dado un valor n, un máximo local es aquel elemento que es 
-- mayor que los n elementos anteriores y los n posteriores a él en la
-- lista; además, el primer y último elemento de la lista no pueden ser
-- un máximo local.
-- Define la función (maximosLocales n xs) tal que devuelva una lista
-- con los máximos locales en xs según el valor n. Los máximos locales
-- se darán mediante pares (i,v), donde i es la posición en la lista
-- (comenzando a contar por 1), y v es el valor. 
-- Por ejemplo:
--  > maximosLocales 3 [0.1,0.2,0.1,0.04,0.1,0.2,0.25,0.2]
--  [(2,0.2),(7,0.25)]
--  > maximosLocales 4 [0.1,0.2,0.1,0.04,0.1,0.2,0.25,0.2]
--  [(7,0.25)]
--  > maximosLocales 3 [0.1,0.2]
--  []
--  > maximosLocales 3 []       
--  []
-- ---------------------------------------------------------------------

maximosLocales :: Ord a => Int -> [a] -> [(Int,a)]
maximosLocales n [] = []
maximosLocales n (x:xs) = mlaux n 2 [x] xs                       -- llamamos la función auxiliar

mlaux :: Ord a => Int -> Int -> [a] -> [a] -> [(Int,a)]
mlaux _ _ _ [] = []
mlaux _ _ _ [_] = []                                             -- tanto si la lista es vacía o tiene solo un elemento
mlaux n i ps (x:xs)                                              
    | x > maximum (ps ++ take n xs) = (i,x):mlaux n (i+1) ps' xs -- si x es mayor que los elementos anteriores (ps) y posteriores (take n xs)
    | otherwise = mlaux n (i+1) ps' xs                           -- si no es mayor
    where ps' = (x:take (n-1) ps)                                -- los n elementos anteriores que propagamos en la llamada recursiva

-- ---------------------------------------------------------------------
-- Ejercicio 4. (2,5 puntos)
-- Define la función (digitosConDecimales n), tal que reciba un número
-- real (de simple precisión) y devuelva un par donde ambas componentes
-- son una lista de números enteros. La función devuelve la lista de los
-- digitos del número n, poniendo los dígitos de la parte entera en la 
-- primera componente del par, y los dígitos de los decimales en la segunda
-- componente. 
-- Nota: Se conseguirá 2,5 puntos si se desarrolla usando solo funciones 
-- de orden superior cuando haya que recorrer listas. Si no, si se emplea
-- recursión o comprensión, la nota máxima será un 1,5.
-- Pista: si n=10, y n es un Float, entonces show n == "10.0"
-- Por ejemplo:
--  > digitosConDecimales 3.1415
--  ([3],[1,4,1,5])
--  > digitosConDecimales 10 
--  ([1,0],[0])
--  > digitosConDecimales (-10)            -- descartamos el signo para los negativos
--  ([1,0],[0])
-- ---------------------------------------------------------------------

digitosConDecimales :: Float -> ([Int],[Int])
digitosConDecimales n = (digitos es,digitos ds)  -- la solución final
    where cs = filter (/='-') (show n)           -- quitamos el signo de negativo
          es = takeWhile (/='.') cs              -- tomamos la parte entera (hasta el punto)
          ds = tail (dropWhile (/='.') cs)       -- tomamos la parte decimal (después del punto, quitándolo)
          digitos ss = map digitToInt ss         -- una función sencilla para convertir a dígitos