-- PD. Práctica 8.5 Solución parcial
-- Tipos: definición y uso de tipos (exámenes antiguos)
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================

import Data.Default

-- ---------------------------------------------------------------------
-- Ejercicios 1 y 2. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P1-P5.pdf
-- ---------------------------------------------------------------------

-- Ejercicio 1
type PilaDeDiscos = [Int]
type Varilla = String

data TorreDeHanoi = Torre PilaDeDiscos PilaDeDiscos PilaDeDiscos
                    deriving Show

moverDisco :: TorreDeHanoi -> Varilla -> Varilla -> TorreDeHanoi
moverDisco (Torre (x:xs) pc pd) "I" "C" = Torre xs (x:pc) pd
moverDisco (Torre (x:xs) pc pd) "I" "D" = Torre xs pc (x:pd)
moverDisco (Torre pi (x:xs) pd) "C" "I" = Torre (x:pi) xs pd
moverDisco (Torre pi (x:xs) pd) "C" "D" = Torre pi xs (x:pd)
moverDisco (Torre pi pc (x:xs)) "D" "I" = Torre (x:pi) pc xs
moverDisco (Torre pi pc (x:xs)) "D" "C" = Torre pi (x:pc) xs

-- Ejercicio 2:
type Termino a = ( a , a )
data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
                   deriving Show

grado :: Polinomio Int -> Int
grado PolCero = 0
grado (Pol (c,g) p) = max g (grado p)

-- ---------------------------------------------------------------------
-- Ejercicios 3 y 4. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P2-P6.pdf
-- ---------------------------------------------------------------------

-- Ejercicio 3
type Orilla = (Int, Int)
type Barca = String

data MisionerosYCanibales = MYC Orilla Orilla Barca

atravesarRio :: MisionerosYCanibales -> Int -> Int ->
                MisionerosYCanibales
atravesarRio (MYC (mi,ci) (md,cd) "Derecha") m c = 
    MYC (mi+m,ci+c) (md-m,cd-c) "Izquierda"
atravesarRio (MYC (mi,ci) (md,cd) "Izquierda") m c = 
    MYC (mi-m,ci-c) (md+m,cd+c) "Derecha"

-- Ejercicio 4
coeficiente :: Polinomio Int -> Int -> Int
coeficiente PolCero _ = 0
coeficiente (Pol (c,g) p) n
    | g==n = c
    | otherwise = coeficiente p n

-- ---------------------------------------------------------------------
-- Ejercicios 5 y 6. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P3-P7.pdf
-- ---------------------------------------------------------------------

-- Ejercicio 5
type Jarra = (Int,Int)
type Vertido = String

data ProblemaDeLasJarras = PJ Jarra Jarra

verter :: ProblemaDeLasJarras -> Vertido -> ProblemaDeLasJarras
verter (PJ (c1,m1) (c2,m2)) "1 a 2" = PJ (0,m1) (min (c2+c1) m2,m2)
verter (PJ (c1,m1) (c2,m2)) "2 a 1" = PJ (min (c1+c2) m1, m1) (0,m2)

-- Ejercicio 6
--type Termino a = ( a , a )
--data Polinomio a = PolCero | Pol ( Termino a ) ( Polinomio a )
--                   deriving Show

valor :: Polinomio Int -> Float -> Float
valor PolCero _ = 0.0
valor (Pol (x,b) p) n = xr * n ^ br
  where xr = fromIntegral x
        br = fromIntegral b


-- ---------------------------------------------------------------------
-- Ejercicios 7 y 8. Realice los dos primeros ejercicios del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_control_3/examen-P4-P8.pdf
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicios 9 y 10. Realice los ejercicios 6 y 7 del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Febrero_final/examen.pdf
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 11 y 12. Realice los ejercicios 6 y 7 del examen cuyo
-- enunciado se encuentra en la url:
-- http://www.cs.us.es/cursos/pd-2011/examenes/Septiembre/examen.pdf
-- ---------------------------------------------------------------------

--ej6
type Serpiente = [(Int,Int)] -- (x,y)
type Movimiento = String
data PuzleSerpiente = PSerpiente Serpiente Int 
        deriving Show

ej6 :: PuzleSerpiente
ej6 = PSerpiente [(3,3),(3,2),(3,1)] 3

moverSerpiente :: PuzleSerpiente -> Movimiento -> PuzleSerpiente
moverSerpiente (PSerpiente xs i) m 
    | m == "Arriba" =    PSerpiente ((x, y+1):(init xs))     (i+1)
    | m == "Abajo" =     PSerpiente ((x, y-1):(init xs))     (i+1)
    | m == "Izquierda" = PSerpiente ((x-1, y):(init xs))     (i+1)
    | m == "Derecha" =   PSerpiente ((x+1, y):(init xs))     (i+1)
    where (x,y) = head xs

-- -------------------------------------------------------------------
-- Ejercicio 13. (parcial 2 del curso 2018/19)
--
-- 1. Defina, con sintaxis de registro, un nuevo tipo que contenga la
--    información sobre planetas que aparecen en las películas
--    de star wars:
--    * name, diameter, population, de tipo String
--    * residents, de tipo lista de String
--
-- 2. Haga que el tipo anterior disponga de un valor por defecto,
-- de modo que podamos posteriormente crear elementos del tipo
-- sin necesidad de proporcionar todos los datos solicitados
--
-- 3. Defina un tipo sinónimo de una lista de planetas
--
-- -------------------------------------------------------------------

data SWPlanet = SWP {
  name::String, diameter::String, population::String, residents::[String]
  } deriving (Show)

instance Default SWPlanet where
  def = SWP {
    name=def, diameter=def, population=def,residents=def
    }

type SWPlanets = [SWPlanet]

