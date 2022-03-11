--Leonardo Aguirre Muñoz
module Practica2 where

{- =================================================================================================

Ejercicio 1. Las dimensiones de un rectángulo pueden representarse mediante pares; por ejemplo, el par
(5,3) representa al rectángulo de base 5 y altura 3. Completar el cuerpo de la función mayorRectangulo
que obtiene el rectángulo de mayor área entre los rectángulos recibidos como parámetro.
================================================================================================= -}

-- Función que obtiene el rectángulo de mayor área entre dos
-- rectángulos .
mayorRectangulo :: (Float, Float) -> (Float, Float) -> (Float, Float)
mayorRectangulo a b
    |fst(a)*snd(a) > fst(b)*snd(b)= a
    |fst(a)*snd(a) < fst(b)*snd(b) = b
    |fst(a)*snd(a)== fst(b)*snd(b) = a

{- =================================================================================================

Ejercicio 2. Completar el cuerpo de la función cuadrante que obtiene el cuadrante de un punto p. La
imagen anexa al repo muestra el número de cuadrantes de acuerdo a las coordenadas de un punto.
================================================================================================= -}

-- Función que obtiene el cuadrante de un punto .
cuadrante :: (Float, Float) -> String
cuadrante (x ,y)
    |fst(x,y) > 0 && snd(x,y) > 0 ="1er Cuadrante"
    |fst(x,y) < 0 && snd(x,y) > 0 ="2do Cuadrante"
    |fst(x,y) < 0 && snd(x,y) < 0 ="3er Cuadrante"
    |fst(x,y) > 0 && snd(x,y) < 0 ="4to Cuadrante"
    |fst(x,y) == 0 && snd(x,y) == 0 ="No es posible determinar el cuadrante del centro del plano cartesiano"
    |otherwise  = "Deben ser valores distintos de cero para determinar el cuadrante"

{- =================================================================================================

Ejercicio 3. Completar el cuerpo de la función copias que dado un número n y un elemento a, genera
una lista con n copias del elemento a. Usar rangos y listas por comprensión para resolver este ejercicio.
================================================================================================= -}

-- Función que genera una lista con n copias del elemento a.
copias :: Int -> a -> [a]
copias 0 a= []
copias n a = a:[] ++ copias (n-1) a


{- =================================================================================================

Ejercicio 4. Completar el cuerpo de la función apariciones que dada una lista, regresa una nueva lista
de la forma (k,x), donde k es el número de apariciones consecutivas de x en la lista original.
================================================================================================= -}

-- Función que obtiene el número de apariciones consecutivas de los
-- elementos de una lista .
rep ::[a] -> a -> Int
rep [] a=0
rep l a
    |a==a=1+rep(l) a
    |otherwise= rep(l) a
--apariciones :: Eq a => [a] -> [(Int, a)]
--apariciones l =

{- =================================================================================================

Ejercicio 5. Completar el cuerpo de la función eliminaRepetidos que dada una lista, elimina aquellos
elementos repetidos, dejando únicamente la primera aparición. Usar la función filter y lambdas para
resolver este ejercicio.
================================================================================================= -}

-- Función que elimina elementos repetidos . Deja la primera aparición .
--eliminaRepetidos :: Eq a => [a] -> [a]
--eliminaRepetidos l = {- Aquí va su código. -}

{- =================================================================================================

Ejercicio 6. Completar el cuerpo de las funciones reversar y reversal que obtienen la reversa de una
lista usando las funciones de plegado foldr, foldl y lambdas respectivamente.
================================================================================================= -}

-- Función que obtiene la reversa de una lista .
--reversar :: [a] -> [a]
--reversar l = {- Aquí va su código. -}
-- Función que obtiene la reversa de una lista .
--reversal :: [a] -> [a]
--reversal l = {- Aquí va su código. -}
