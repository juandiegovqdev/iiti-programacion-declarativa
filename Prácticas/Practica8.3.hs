-- PD-Práctica 8.3
-- Expresiones Aritméticas con tipos de datos algebráicos
-- Departamento de Ciencias de la Computación e I.A.
-- Universidad de Sevilla
-- =====================================================================



-- ---------------------------------------------------------------------
-- Ejercicio 1. Las expresiones aritméticas básicas pueden
-- representarse usando el siguiente tipo de datos  
--    data Expr1 = C1 Int 
--               | S1 Expr1 Expr1 
--               | P1 Expr1 Expr1  
--               deriving Show
-- Por ejemplo, la expresión 2*(3+7) se representa por
--    P1 (C1 2) (S1 (C1 3) (C1 7))
-- 
-- Definir la función evalua, tal que (evalua e) es el valor de la 
-- expresión aritmética e. Por ejemplo, 
--    evalua (P1 (C1 2) (S1 (C1 3) (C1 7)))  ==  20
-- ---------------------------------------------------------------------

data Expr1 = C1 Int 
           | S1 Expr1 Expr1 
           | P1 Expr1 Expr1  
           deriving Show

-- ---------------------------------------------------------------------
-- Ejercicio 2. Definir la función aplica, tal que (aplica f e) es la 
-- expresión obtenida aplicando la función f a cada uno de los números 
-- de la expresión e. Por ejemplo, 
--    ghci> aplica (+2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 5) (C1 7)) (P1 (C1 8) (C1 9))
--    ghci> aplica (*2) (S1 (P1 (C1 3) (C1 5)) (P1 (C1 6) (C1 7)))
--    S1 (P1 (C1 6) (C1 10)) (P1 (C1 12) (C1 14))
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 3. Las expresiones aritméticas construidas con una
-- variable (denotada por X), los números enteros y las operaciones de
-- sumar y multiplicar se pueden representar mediante el tipo de datos
-- Expr2 definido por     
--    data Expr2 = X
--               | C2 Int
--               | S2 Expr2 Expr2
--               | P2 Expr2 Expr2
-- Por ejemplo, la expresión "X*(13+X)" se representa por
-- "P2 X (S2 (C2 13) X)".
-- 
-- Definir la función evaluaE, tal que (evaluaE e n) es el valor de la 
-- expresión e cuando se sustituye su variable por n. Por ejemplo,
--    evaluaE (P2 X (S2 (C2 13) X)) 2  ==  30
-- ---------------------------------------------------------------------
 
data Expr2 = X
           | C2 Int
           | S2 Expr2 Expr2
           | P2 Expr2 Expr2

-- ---------------------------------------------------------------------
-- Ejercicio 4. Definir la función numVars, tal que (numVars e) es el 
-- número de variables en la expresión e. Por ejemplo, 
--    numVars (C2 3)                 ==  0
--    numVars X                      ==  1
--    numVars (P2 X (S2 (C2 13) X))  ==  2
-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------
-- Ejercicio 5. Las expresiones aritméticas con variables genéricas 
-- pueden representarse usando el siguiente tipo de datos  
--    data Expr3 = C3 Int 
--               | V3 Char 
--               | S3 Expr3 Expr3 
--               | P3 Expr3 Expr3  
--               deriving Show
-- Por ejemplo, la expresión 2*(a+5) se representa por
--    P3 (C3 2) (S3 (V3 'a') (C3 5))
-- 
-- Definir la función evaluaG, tal que (evaluaG e c) es el valor de la 
-- expresión e en el contexto c (es decir, el valor de la expresión 
-- donde las variables de e se sustituyen por los valores según se 
-- indican en el contexto c). Por ejemplo,
--    ghci> evaluaG (P3 (C3 2) (S3 (V3 'a') (V3 'b'))) [('a',2),('b',5)]
--    14
-- ---------------------------------------------------------------------

data Expr3 = C3 Int 
           | V3 Char 
           | S3 Expr3 Expr3 
           | P3 Expr3 Expr3  
           deriving Show
                   
-- ---------------------------------------------------------------------
-- Ejercicio 6. Definir la función sumas, tal que (sumas e) es el 
-- número de sumas en la expresión e. Por ejemplo, 
--    sumas (P3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  1
--    sumas (S3 (V3 'z') (S3 (C3 3) (V3 'x')))  ==  2
--    sumas (P3 (V3 'z') (P3 (C3 3) (V3 'x')))  ==  0
-- ---------------------------------------------------------------------
                   
-- ---------------------------------------------------------------------
-- Ejercicio 7. Definir la función sustitucion, tal que 
-- (sustitucion e s) es la expresión obtenida sustituyendo las variables 
-- de la expresión e según se indica en la sustitución s. Por ejemplo, 
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'x'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (C3 7))
--    ghci> sustitucion (P3 (V3 'z') (S3 (C3 3) (V3 'y'))) [('x',7),('z',9)]
--    P3 (C3 9) (S3 (C3 3) (V3 'y'))
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Ejercicio 8. Definir la función reducible, tal que (reducible e) se 
-- verifica si e es una expresión reducible; es decir, contiene alguna 
-- operación en la que los dos operandos son números. Por ejemplo,
--    reducible (S3 (C3 3) (C3 4))               == True
--    reducible (S3 (C3 3) (V3 'x'))             == False
--    reducible (S3 (C3 3) (P3 (C3 4) (C3 5)))   == True
--    reducible (S3 (V3 'x') (P3 (C3 4) (C3 5))) == True
--    reducible (S3 (C3 3) (P3 (V3 'x') (C3 5))) == False
--    reducible (C3 3)                           == False
--    reducible (V3 'x')                         == False
-- ---------------------------------------------------------------------
