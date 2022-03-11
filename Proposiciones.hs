-- Leonardo Aguirre Muñoz
module Proposiciones where
-- Tipo de dato que modele las fórmulas atómicas
data ATOM = Cte Bool
          |Var String

-- Tipo de dato que representa las fórmulas del lenguaje proposicional
data PROP = FA ATOM
          |Negacion PROP
          |Conjuncion PROP PROP
          |Disyuncion PROP PROP
          |Implicacion PROP PROP
          |SiYSoloSi PROP PROP

-- Implementacion de la función show para ATOM
instance Show ATOM where
    show (Var p) = p
    show (Cte c) = show c

    --Implementacion de la funcion show para PROP
instance Show PROP where
    show (FA f) = show f
    show (Negacion p) = "No " ++ show p
    show (Conjuncion p q) = "("++show p++" & "++show q++")"
    show (Disyuncion p q) = "("++show p++" | "++show q++")"
    show (Implicacion p q) = "("++show p++" -> "++show q++")"
    show (SiYSoloSi p q) = "("++show p++" <-> "++show q++")"

--Funcion
conectivos ::PROP -> Int
conectivos (FA _ ) = 0
conectivos (Negacion p) = 1 + conectivos p
conectivos (Conjuncion p q) = 1 + conectivos p
conectivos (Disyuncion p q) = 1 + conectivos p
conectivos (Implicacion p q) = 1 + conectivos p
conectivos (SiYSoloSi p q) = 1 + conectivos p

--Funcion que calcula el número de presencias atómicas
presenciaAtomica :: PROP -> Int
presenciaAtomica (FA _) =1
presenciaAtomica (Negacion p) = presenciaAtomica p
presenciaAtomica (Conjuncion p q) =presenciaAtomica p + presenciaAtomica q
presenciaAtomica (Disyuncion p q) =presenciaAtomica p + presenciaAtomica q
presenciaAtomica (Implicacion p q) =presenciaAtomica p + presenciaAtomica q
presenciaAtomica (SiYSoloSi p q) =presenciaAtomica p + presenciaAtomica q

--Funcion que calcula el número de parentesis

numeroParentesis :: PROP -> Int
numeroParentesis (FA _) =0
numeroParentesis (Negacion p) = numeroParentesis p
numeroParentesis (Conjuncion p q)=2+numeroParentesis p+numeroParentesis q
numeroParentesis (Disyuncion p q)= 2+numeroParentesis p+numeroParentesis q
numeroParentesis (Implicacion p q)=2+numeroParentesis p+numeroParentesis q
numeroParentesis (SiYSoloSi p q)= 2+numeroParentesis p+numeroParentesis q


--Funcion de substitucion textual
sustitucionTextual :: PROP -> (String,PROP) -> PROP
sustitucionTextual (FA (Cte c)) _ = (FA (Cte c))
sustitucionTextual (FA (Var p)) (q,form) = if p==q then form else (FA (Var p))
sustitucionTextual (Negacion p) par = (Negacion (sustitucionTextual p par))
sustitucionTextual (Conjuncion p q) par = (Conjuncion (sustitucionTextual p par) (sustitucionTextual q par))
sustitucionTextual (Disyuncion p q) par = (Disyuncion (sustitucionTextual p par) (sustitucionTextual q par))
sustitucionTextual (Implicacion p q) par = (Implicacion (sustitucionTextual p par) (sustitucionTextual q par))
sustitucionTextual (SiYSoloSi p q) par = (SiYSoloSi (sustitucionTextual p par) (sustitucionTextual q par))
