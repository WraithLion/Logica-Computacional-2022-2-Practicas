--Leonardo Aguirre Muñoz
module Practica1 where

{- =================================================================================================

Ejercicio 1. Completar el cuerpo de la función discriminante que calcula el discriminante de una
ecuación de segundo grado.
================================================================================================= -}

-- Función que calcula el discriminante de una ecuación cuadrática.
discriminante :: Float -> Float -> Float -> Float
discriminante a b c = b*b - (4*a*c)

{- =================================================================================================

Ejercicio 2. Completar el cuerpo de las siguientes funciones usando asingaciones locales let o where
según se indica.
================================================================================================= -}

-- Usando la primitiva let, completar el cuerpo de la función para que calcule el área de un
-- triángulo usando la fórmula de Herón.

-- Función que calcula el área de un triángulo usando la fórmula de
-- Herón.
areaHeron :: Float -> Float -> Float -> Float
areaHeron a b c =
    let semiperimetro = (a+b+c)/2
    in sqrt(semiperimetro*(semiperimetro-a)*(semiperimetro-b)*(semiperimetro-c))

-- Usando la primitiva where, completar el cuerpo de las siguientes funciones que calculan las
-- raíces de una ecuación de segundo grado, usar la función que calcula el discriminante
-- (Ejercicio 1) como auxiliar.

-- Función que calcula la raíz de una ecuación de segundo grado.
raiz1 :: Float -> Float -> Float -> Float
raiz1 a b c
    | discriminant >0 =  ((-1)*b)+sqrt(discriminant)/(2*a)
    | discriminant > 0 = ((-1)*b)-sqrt(discriminant)/(2*a)
    | discriminant ==0 = ((-1)*b)/(2*a)
    | discriminant <0 = (-1)
    where discriminant= discriminante a b c

-- Función que calcula la raíz de una ecuación de segundo grado.
--raiz2 :: Float -> Float -> Float -> Float
--raiz2 a b c = {- Aquí va su código. -}

{- =================================================================================================

Ejercicio 3. Completar el cuerpo de las siguientes funciones usando condicionales if o guardias
según se indica.
================================================================================================= -}

-- Dados dos números enteros, positivos, regresar True si su último dígito es el mismo. Por ejemplo
-- 27 y 57, cumplen esta condición. Completar el ejercicio usando el condicional if.

-- Función que indica si el último dígito de dos números es igual .
ultimoDigito :: Int -> Int -> Bool
ultimoDigito x y =do
    if mod x 10 == mod y 10
       then True
       else False

-- Se tienen dos monos diabólicos, a y b y los parámetros aSonrie y bSonrie que indican si cada mono
-- está sonriendo respectivamente. Si ningún mono sonríe o ambos se encuentran sonriendo, hay peligro,
-- en caso contrario todo está bien. Completar el cuerpo de la función que indica, dado el estado de los
-- parámetros, si hay peligro o no. Usar guardias para resolver este ejercicio.

-- Función que indica si hay peligro si se sabe si lo monos están
-- sonriendo o no.
hayPeligro :: Bool -> Bool -> Bool
hayPeligro aSonrie bSonrie
    | aSonrie ==True && bSonrie == True =True
    | aSonrie ==False && bSonrie==False=True
    | otherwise  =False
