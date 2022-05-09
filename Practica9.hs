module Ejercicio09 where
-- Leonardo Aguirre Muñoz
import LPred
import Ejemplo
--------------------------------------------------------------------------------
-- Ejercicios


-- Ejercicio 1
-- Definir la siguiente función recursiva donde numCteVar p 
-- representa el número de presencias de constantes y variables de la fórmula p.

numCteVar :: PO -> Int
numCteVar (FA (Cte a))=1
numCteVar (FA (Prd _ []))=0
numCteVar (FA (Prd _ a))= tamano a
numCteVar (Neg p)= numCteVar p
numCteVar (Conj p q) = numCteVar p+ numCteVar q
numCteVar (Disy p q) = numCteVar p +numCteVar q
numCteVar (Impl p q) = numCteVar p+ numCteVar q
numCteVar (Syss p q) = numCteVar p +numCteVar q
numCteVar (Pt _ q) = numCteVar q
numCteVar (Ex _ q) = numCteVar q

tamano :: [a] ->Int
tamano l = sum [1 | _<-l]
-- Ejercicio 2
-- Definir la siguiente función recursiva simPred tal que simPred p 
-- es el conjunto de símbolos de predicado en una fórmula p.
simPred :: PO -> [ String ]
simPred (FA (Cte a))=[]
simPred (FA (Prd a _))= [[x]|x<-a]
simPred (Neg p) = simPred p
simPred (Conj p q)= unirlistas (simPred p) (simPred q)
simPred (Disy p q) = unirlistas (simPred p) (simPred q)
simPred (Impl p q) = unirlistas (simPred p) (simPred q)
simPred (Syss p q) = unirlistas (simPred p) (simPred q)
simPred (Pt _ q) = simPred q
simPred (Ex _ q) = simPred q

unirlistas:: [a]->[a]->[a]
unirlistas xs [] = xs
unirlistas [] ys = ys
unirlistas (x:xs) (y:ys) = x : y : unirlistas xs ys



-- Ejercicio 3 
-- Definir una función recursiva tal que ligadas p representa el 
-- número de variables ligadas en una fórmula.
ligadas :: PO -> Int
ligadas (FA _)=0
ligadas (Ex a(FA (Cte b)))=0
ligadas (Ex a(FA (Prd _ [])))=0
ligadas (Ex a(FA (Prd _ b)))= esvarlig b (termino a)
ligadas (Ex a(Neg (FA (Prd _ b)))) = esvarlig b (termino a)
ligadas (Neg(Ex a (FA (Prd _ b)))) = esvarlig b (termino a)
ligadas (Ex a(Conj (FA (Prd _ b)) (FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Conj (FA (Prd _ b)) (Ex a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Ex a(Disy (FA (Prd _ b)) (FA (Prd _ c))))=esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Disy (FA (Prd _ b)) (Ex a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Ex a(Impl (FA (Prd _ b)) (FA (Prd _ c))))=esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Impl (FA (Prd _ b)) (Ex a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Ex a(Syss (FA (Prd _ b)) (FA (Prd _ c))))=esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Syss (FA (Prd _ b)) (Ex a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Pt a(FA (Cte b)))=0
ligadas (Pt a(FA (Prd _ [])))=0
ligadas (Pt a(FA (Prd _ b)))= esvarlig b (termino a)
ligadas (Pt a(Neg (FA (Prd _ b)))) = esvarlig b (termino a)
ligadas (Neg(Pt a (FA (Prd _ b)))) = esvarlig b (termino a)
ligadas (Pt a(Conj (FA (Prd _ b)) (FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Conj (FA (Prd _ b)) (Pt a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Pt a(Disy (FA (Prd _ b)) (FA (Prd _ c))))=esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Disy (FA (Prd _ b)) (Pt a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Pt a(Impl (FA (Prd _ b)) (FA (Prd _ c))))=esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Impl (FA (Prd _ b)) (Pt a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Pt a(Syss (FA (Prd _ b)) (FA (Prd _ c))))=esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Syss (FA (Prd _ b)) (Pt a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
ligadas (Conj (FA (Prd _ b)) (Pt a(FA (Prd _ c))))= esvarlig b (termino a)+esvarlig c (termino a)
--ligadas (Ex b(FA (Prd _ a))) = sum[1|_<-a,b==a]

esvarlig ::Eq a=> [a]-> a ->Int
esvarlig [] _=0
--esvarlig (a:as) b=sum [1 + esvarlig as | _<-(a:as)]
esvarlig (a:as) b|a==b=1+ esvarlig as b|otherwise=0+esvarlig as b
-- Ejercicio 4 
{-- 
Se tienen seis cubos de color amarillo, azul o verde. 
Un cubo puede estar uno sobre otro o en el piso.
Considérese la signatura 
Sigma = {S(2),A(1), Az(1), V (1),L(1), p } 
	donde S (x, y) significa x está sobre y, 
	A(x), Az (x) , V (x) representan los colores amarillo, azul y verde, 
	L(x) significa x está libre, es decir ningún cubo está sobre x, 
	y la constante p representa al piso.
Definir cada elemento de la signatura anterior 
y formalizar los siguientes enunciados:

a) Hay un cubo azul sobre el piso con un cubo amarillo sobre él y un cubo verde sobre el amarillo.

(V(x),(S(A(x)(S(Az(x),p)))))

b) Hay un cubo azul libre y un cubo verde libre.
Existe x(L(x)&&Az(x))&&Existe x(V(x)&&L(x))

c) No todos los cubos azules están libres.
Existe x(Az(x)&&L(x))
--}
