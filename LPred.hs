module LPred where

import Data.List
--CONTENIDO
{-
    >1< DEFINICIÓN DE TIPOS
    >2< FUNCIONES DE ESTRUCTURA
    >3< LISTAS DE VARIABLES
    >4< SUSTITUCIONES DE VARIABLES
    >5< RECTIFICACION SENCILLA DE FORMULAS
    >6< SUSTITUCIONES MÚLTIPLES DE VARIABLES
    >7< EQUIVALENCIAS
    >8< SUSTITUCIONES DE TÉRMINOS
    >9< EJERCICIOS
-}



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------- >1< DEFINICIÓN DE TIPOS-------------------------------------
--------------------------------------------------------------------------------
-- Definición del conjunto TERM .
data TERM = Var String
          | Fun String [TERM]
        deriving (Eq)


-- Implementación de la función Show para impr
instance Show TERM where
   show (Var v) = v
   show (Fun n ls) = n ++ "(" ++ aux ls ++ ")"

-- Función auxiliar .
-- Agrupa en formato cadena los argumentos de una función separados por coma.
aux :: [TERM] -> String
aux [] = ""
aux [x] = show x
aux (x:xs) = show x ++ ", " ++ aux xs

-- Definición del conjunto ATOM .
data ATOM = Cte Bool
    | Prd String [ TERM ]
    | Eq TERM TERM
    deriving (Eq)


instance Show ATOM where
    show (Cte b) = show b
    show (Prd n ls) = n ++ "(" ++ aux ls ++ ")"
    show (Eq p q) = show q ++ " = " ++ show q

-- Definición del lenguaje PO de la Lógica de Primer Orden
data PO = FA ATOM
        | Neg PO
        | Conj PO PO
        | Disy PO PO
        | Impl PO PO
        | Syss PO PO
        | Pt String PO
        | Ex String PO
        deriving (Eq)
--instance Eq PO where 
    -- (==) (FA a) (FA b)  = a == b

instance  Show PO where
    show (FA a) = show a
    show (Neg p) = "no " ++ show p
    show ( Conj p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
    show ( Disy p q) = "(" ++ show p ++ " | " ++ show q ++ ")"
    show ( Impl p q) = "(" ++ show p ++ " -> " ++ show q ++ ")"
    show ( Syss p q) = "(" ++ show p ++ " <-> " ++ show q ++ ")"
    show (Pt x p) = "Pt " ++ x ++ " " ++ show p
    show (Ex x p) = "Ex " ++ x ++ " " ++ show p

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------- >2< FUNCIONES DE ESTRUCTURA --------------------------------
--------------------------------------------------------------------------------


-- Función que obtiene los subtérminos de un término.
subterminos :: TERM -> [TERM]
subterminos ( Var v) = [( Var v)]
subterminos ( Fun c []) = [( Fun c [])]
subterminos ( Fun f ls) = [( Fun f ls )] ++ subtAux ls


-- Función auxiliar que obtiene los subtérminos de una lista.
subtAux :: [TERM] -> [TERM]
subtAux [] = []
subtAux (x:xs) = union ( subterminos x) ( subtAux xs)


-- Función que calcula el peso de una fórmula.
peso :: PO -> Int
peso (FA a) = 0
peso (Neg p) = 1 + peso p
peso ( Conj p q) = 1 + peso p + peso q
peso ( Disy p q) = 1 + peso p + peso q
peso ( Impl p q) = 1 + peso p + peso q
peso ( Syss p q) = 1 + peso p + peso q
peso (Pt _ p) = 1 + peso p
peso (Ex _ p) = 1 + peso p


-- Función que calcula la unión de dos listas 
unionAux :: Eq a => [a] -> [a] -> [a] 
unionAux l1 l2 = union (nub l1) l2 

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------- >3< LISTAS DE VARIABLES----------------------------------------
--------------------------------------------------------------------------------

-- Funcíón que dada una lista de términos devuelve la lista de variables sin 
-- repetición que figuran en esa lista.
varLstAux :: [TERM] -> [String]
varLstAux [] = []
varLstAux ((Var x):lst) = unionAux [x] (varLstAux lst)
varLstAux ((Fun _ t):lst) = unionAux (varLstAux lst) (varLstAux t)

-- Funcion que devuelve una lista con todas las variables y repetidas para contar las
--variables libres que estén fuera del alcance del cuantificador
varLstAux1 :: [TERM] -> [String]
varLstAux1 [] = []
varLstAux1 ((Var x):lst) = [x] ++varLstAux1 lst
varLstAux1 ((Fun _ t):lst) = varLstAux1 lst ++ varLstAux1 t

-- Función que devuelve la lista de variables sin repetición que figuran en una
-- fórmula sin contemplar las variables de ligado.
varLst :: PO -> [String]
varLst (FA(Cte a)) = []
varLst (FA (Prd _ b)) = varLstAux b
varLst (Neg f1) = varLst f1
varLst (Conj f1 f2) = varLst f1 ++ varLst f2
varLst (Disy f1 f2) = varLst f1 ++ varLst f2
varLst (Impl f1 f2) = varLst f1 ++ varLst f2
varLst (Syss f1 f2) = varLst f1 ++ varLst f2
varLst (Pt _ f1) = varLst f1
varLst (Ex _ f1) = varLst f1

-- Función que devuelve la lista de variables sin repetición que figuran en una
-- fórmula contemplando las variables de ligado
varTot :: PO -> [String]
varTot (FA(Cte a)) = []
varTot (FA (Prd _ b)) = varLstAux b
varTot (Neg f1) = varLst f1
varTot (Conj f1 f2) = varLst f1 ++ varLst f2
varTot (Disy f1 f2) = varLst f1 ++ varLst f2
varTot (Impl f1 f2) = varLst f1 ++ varLst f2
varTot (Syss f1 f2) = varLst f1 ++ varLst f2
varTot (Pt a f1) = unionAux (varLig f1) (varLst f1)
varTot (Ex a f1) = unionAux (varLig f1) (varLst f1)


--Función que devuelve una lista de variables libres en una fórmula
varLib :: PO -> [String]
varLib (FA(Cte a))= []
varLib (FA(Prd _  b))= varLstAux1 b
varLib (Neg f1) = varLib f1
varLib (Conj f1 f2) = varLib f1 ++varLib f2
varLib (Disy f1 f2) = varLib f1 ++varLib f2
varLib (Impl f1 f2) = varLib f1 ++varLib f2
varLib (Syss f1 f2) = varLib f1 ++varLib f2
varLib (Pt a f1)=quitarElemento (varLib f1) a
varLib (Ex a f1)=quitarElemento (varLib f1) a

quitarElemento::Eq a=>[a]->a->[a]
quitarElemento [] e = []
quitarElemento (x:xs) e
    | x == e = quitarElemento xs e
    | otherwise = x:( quitarElemento xs e )

-- Función que devuelve una lista de variables de ligado  sin repetición que
-- figuran en una fórmula. Esto es, variables que se encuentran en
-- cuantificadores
varLig :: PO -> [String]
varLig (FA(Cte a))= []
varLig (FA(Prd _  b))= []
varLig (Neg f1) = varLig f1
varLig (Conj f1 f2) = varLig f1 ++varLig f2
varLig (Disy f1 f2) = varLig f1 ++varLig f2
varLig (Impl f1 f2) = varLig f1 ++varLig f2
varLig (Syss f1 f2) = varLig f1 ++varLig f2
varLig (Pt a f1)=varLig f1 ++ [a]
varLig (Ex a f1)=varLig f1 ++ [a]

-- Función que dada una lista de restricciones (variables que no se pueden usar)
-- y una variable a sustituir, devuelve una nueva variable que no figure en
-- la lista de restricciones
newVar:: [String] -> String -> String
newVar restr s = newVarAux restr s 0

-- Función que dada una lista de restricciones (variables que no se pueden usar)
-- y una variable a sustituir, devuelve una nueva variable que no figure en
-- la lista de restricciones
newVar1:: [String] -> String
newVar1 restr = newVarAux restr "x" 0

-- Función auxiliar para obtener la nueva variable con un subíndice numérico
-- que busca el primer número que no figure en la lista de restricciones.
newVarAux :: [String] -> String -> Int -> String
newVarAux (x:xs) var index =
    if var == x then newVarAux xs var (index)
                            else var++show (index)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------------ >4< SUSTITUCIONES DE VARIABLES-------------------------
--------------------------------------------------------------------------------

--Sustiución textual de variables por otra variable en términos
sustTextVarT ::  TERM -> (String,String)-> TERM
sustTextVarT (Var s) (sOld, sNew)  = if s==sOld then (Var sNew)
                                                else (Var s)
sustTextVarT (Fun c []) _ = Fun c []
sustTextVarT (Fun f xs) sustPair = Fun f ( map (\x -> sustTextVarT x sustPair) xs)


-- Sustiución textual de variables por otras variables en fórmulas
-- Sustituye ocurrencias en cuentificadores.
sustTextVarF :: PO -> (String, String) ->  PO
sustTextVarF (FA (Cte b)) sustPair = (FA (Cte b))
sustTextVarF (FA (Prd n lst)) sustPair = FA (Prd n (map (\x -> sustTextVarT x sustPair) lst ))
sustTextVarF (FA (Eq t1 t2)) sustPair =FA (Eq ( sustTextVarT t1 sustPair) ( sustTextVarT t2 sustPair ))
sustTextVarF (Neg phi) sustPair =Neg (sustTextVarF phi sustPair)
sustTextVarF (Disy phi psi) sustPair =Disy (sustTextVarF phi sustPair) (sustTextVarF psi sustPair)
sustTextVarF (Conj phi psi) sustPair =Conj (sustTextVarF phi sustPair) (sustTextVarF psi sustPair)
sustTextVarF (Impl phi psi)  sustPair =Impl (sustTextVarF phi sustPair) (sustTextVarF psi sustPair)
sustTextVarF (Syss phi psi) sustPair =Syss (sustTextVarF phi sustPair) (sustTextVarF psi sustPair)
sustTextVarF (Pt s phi) (sOld, sNew) =Pt (cambiarVarCuantificador s (sOld,sNew)) (sustTextVarF phi (sOld, sNew))
sustTextVarF (Ex s phi) (sOld, sNew) =Ex (cambiarVarCuantificador s (sOld,sNew)) (sustTextVarF phi (sOld, sNew))

--Funcion que recibe la variable de ligado de un cuantificador y lo sustituye con una nueva variable si es necesario
cambiarVarCuantificador:: String ->(String,String)->String
cambiarVarCuantificador var (var1,var2)= if var==var1 then var2
                                                      else var


asignarNuevaVar :: PO ->[Char]
asignarNuevaVar formula =[x | x<- newVar1 (varLib formula)]
--asignarNuevaVar formula = [(x,y)|x<-(varLib formula),y<-(newVar1 (varLib formula))]

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
----------------- >6< SUSTITUCIONES MÚLTIPLES DE VARIABLES----------------------
--------------------------------------------------------------------------------
-- Tipo que respresenta una lista de sustituciones de una variable x por una y
-- en una pareja (x,y).
type SustVar = [(String,String)]

-- Dadas dos fórmulas define las sustituciones correspondientes entre sus
-- variables de ligado provisto que tengan la misma cantidad de elementos.
creaSust :: PO -> PO -> SustVar
creaSust phi psi = zip (varLig phi) (varLig psi)


-- Función alternativa para cuando se tiene la lista de variables
creaSust1 :: [String] -> [String] -> SustVar
creaSust1 xs ys = zip xs ys



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
------------------- >7< EQUIVALENCIAS-------------------------------------------
--------------------------------------------------------------------------------
-- Indica si dos términos son equivalentes .
equivalentesT :: TERM -> TERM -> Bool
equivalentesT ( Var v) ( Var w) = v == w
equivalentesT ( Fun c []) ( Fun d []) = c == d
equivalentesT ( Fun f xs) ( Fun g ys) = valida xs ys

-- Verifica par a par si dos listas de términos son equivalentes .
valida :: [ TERM ] -> [ TERM ] -> Bool
valida [] [] = True
valida (x:xs) (y:ys) = ( equivalentesT x y) && ( valida xs ys)





--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-------------------- >8< SUSTITUCIONES DE TÉRMINOS------------------------------
--------------------------------------------------------------------------------

-- Definición de sustituciones de términos
type Sust = [( String , TERM )]

-- Función que dada una única substitución devuelve una lista de variables que
-- figuran en la substitución, tanto la variable como del término
varSustAux :: (String,TERM) -> [String]
varSustAux (v,t) = v: (varLstAux [t])

-- Función que devuelve las una lista de variables que figuran en una
-- substitución sin repetición.
varSust :: Sust -> [String]
varSust sts = foldl unionAux [] (map varSustAux sts)


-- Verifica si una sustitución es legal .
verifSus :: Sust -> Bool
verifSus [] = True
verifSus ls = ver1 ls && ver2 ls

-- Verifica que una lista no tiene elementos repetidos .
ver1 :: [( String , TERM )] -> Bool
ver1 [] = True
ver1 (x:xs) = not ( elems x xs) && ver1 xs

-- Busca en una lista de tuplas .
elems :: ( String , TERM ) -> [( String , TERM )] -> Bool
elems _ [] = False
elems (x,y) ((w,_): xs) = x == w || elems (x,y) xs

-- Verifica que una variable no se sustituya consigo misma .
ver2 :: [( String , TERM )] -> Bool
ver2 [] = True
ver2 ((x ,( Var v )): xs) = x /= v && ver2 xs
ver2 (x:xs) = ver2 xs

-- Sustitución de términos .
apsusT :: TERM -> Sust -> TERM
apsusT t [] = t
apSust (Var v) ls = busca v ls
apSust (Fun c []) _ = Fun c []
spSust (Fun f xs) ys = Fun f ( map (\x -> apSust x ys) xs)

-- Busqueda de variables .
busca :: String -> Sust -> TERM
busca v [] = Var v
busca s ((x,y): xs)
  | s == x = y
  | otherwise = busca s xs

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- >9< EJERCICIOS --------------------------------------------------------------
--------------------------------------------------------------------------------

-- NOMBRE:__________________________

-- EJERCICIO1 ------------------------------------------------------------------
-- Completar el cuerpo de la función apsusF tal que (apsusF p s) representa la
-- aplicación de la sustitución s en la fórmula p. Para este ejercicio no usen
-- aún alfa-equivalencias.

apsusF :: PO -> Sust -> PO
apsusF (FA (Cte b)) _ = FA (Cte b)
apsusF (FA (Prd n ls)) ys = FA (Prd n (map (\x -> apsusT x ys) ls ))
apsusF (FA (Eq p q)) ys = FA (Eq ( apsusT p ys) ( apsusT q ys ))
apsusF (Neg p) ys = Neg ( apsusF p ys)
apsusF ( Conj p q) ys = Conj ( apsusF p ys) ( apsusF q ys)
apsusF ( Disy p q) ys = Disy ( apsusF p ys) ( apsusF q ys)
apsusF ( Impl p q) ys = Impl ( apsusF p ys) ( apsusF q ys)
apsusF ( Syss p q) ys = Syss ( apsusF p ys) ( apsusF q ys)
apsusF ( Ex s p) ys= Ex (termToString(busca s ys)) (apsusF p ys)

termToString :: TERM ->String
termToString (Var a)= a

-- EJERCICIO2 ------------------------------------------------------------------
-- Responder las siguientes preguntas en un archivo README.txt:
-- a) ¿Por qué se dice que la función definida en el ejercicio 2 es parcial?
-- b) ¿Qué modificaciones tendrían que hacerse a esta función para volverla
--     una función total?
-- c) Mencionar otra área de las Ciencias de la Computación dónde se usen
--    relaciones de alfa-equivalencia.



-- EJERCICIO3 ------------------------------------------------------------------
-- Completar el cuerpo de la función equivalentes tal que (equivalentes p q)
-- indica si las fórmulas phi y psi son alfa-equivalentes.
equivalentes :: PO -> PO -> Bool
equivalentes phi psi = varLib phi/=varLib psi && varLig phi /=varLig psi



-- EJERCICIO4 ------------------------------------------------------------------
-- Completar el cuerpo de la función renombra tal que (renombra p ls) es el
-- resultado de renombrar las variables ligadas de una fórmula p, de forma que
-- éstas sean ajenas a los nombres de la lista ls.
renombra :: PO -> [ String ] -> PO
renombra p ls =if varLig p /=ls then p
                                else renombra p ls


