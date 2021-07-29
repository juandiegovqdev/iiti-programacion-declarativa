-- PD-Práctica 8.1 
-- Tipos: Definiciones básicas de tipos de datos algebráicos
-- =====================================================================

-- ---------------------------------------------------------------------
-- Ejercicio 1. Haciendo uso de la decraración de tipos (type) define un
-- tipo nuevo, Punto2D, para los Puntos del Plano (de 2 dimensiones).
-- ---------------------------------------------------------------------

type Punto2D = (Float, Float)

-- ---------------------------------------------------------------------
-- Ejercicio 2. Usando el tipo Punto2D, define un vector delimitado por
-- un par de puntos, Vector2D.
-- ---------------------------------------------------------------------

type Vector2D = (Punto2D, Punto2D)

-- ---------------------------------------------------------------------
-- Ejercicio 3. Definir la función vector2Dcoor, que reciba un Vector2D
-- y devuelva un par que describa sus coordenadas. Si el vector está
-- formado por los puntos p1 y p2, entonces el calculo del par se
-- calcula como sigue:
--   * la primera componente es la diferencia de la primera componente
--     de p2 menos del p1.
--   * la segunda componente es la diferencia de la segunda componente
--     de p2 menos del p1.
-- ---------------------------------------------------------------------

vector2Dcoor :: Vector2D -> (Float, Float)
vector2Dcoor ((x1, x2), (y1, y2)) = (y1 - x1, y2 - x2) 

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función productoEscalar, tal que reciba dos
-- vectores Vector2D, y devuelva su producto escalar, calculado como
-- sigue: la suma de la multiplicación de las componentes de las
-- coordenadas de los vectores.
-- ---------------------------------------------------------------------

productoEscalar :: Vector2D -> Vector2D -> Float
productoEscalar v1 v2 = x1 * x2 + y1 * y2
    where (x1, x2) = vector2Dcoor v1
          (y1, y2) = vector2Dcoor v2

-- ---------------------------------------------------------------------
-- Ejercicio 5. Definir la función norma, tal que reciba un vector tipo
-- Vector2D y devuelva el módulo del vector, definido como la raíz
-- cuadrada del producto escalar del vector por sí mismo.
-- ---------------------------------------------------------------------

raizCuadrada :: Float -> Float
raizCuadrada x = until aceptable mejorar 1
    where mejorar y = 0.5*(y+x/y)
          aceptable y = abs(y*y-x) < 0.00001

norma :: Vector2D -> Float 
norma v = raizCuadrada (productoEscalar v v)

norma' :: Vector2D -> Float 
norma' v = sqrt $ productoEscalar v v

-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función paralelos, que reciba dos vectores
-- tipo Vector2D y devuelva si los vectores son paralelos. El cálculo
-- necesario para ello es comprobar que el valor absoluto del producto
-- escalar de los dos vectores dividido por la multiplicación de la
-- norma de cada uno, sea igual a 1.
-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Ejercicio 7. Define tipos de datos (data) para almacenar información
-- sobre el calendario: días de la semana, meses, y estaciones del año.
-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Ejercicio 8. Haciendo uso del tipo Maybe, define una función de
-- división segura (que al dividir por 0 no lance una excepción).
-- ---------------------------------------------------------------------



-- ---------------------------------------------------------------------
-- Ejercicio 9. Haciendo uso del tipo Maybe, define una función que
-- devuelva las raices de una ecuación de segundo grado.
-- ---------------------------------------------------------------------


