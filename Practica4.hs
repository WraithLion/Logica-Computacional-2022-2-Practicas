--Leonardo Aguirre Muñoz
module Practica4 where

import Proposiciones

-- Variables


--Identificación de Proposiciones

--p -> Raúl está comiendo pastel

p = (FA (Var "p"))

--q -> Raúl está jugando con su PS4

q = (FA (Var "q"))

--r -> El Papá de Raúl pagará el seguro de la casa

r = (FA (Var "r"))

--Traducción de cada oracion

--Si p, no q, Si no q entonces no r, por tanto no r

--(p ^ (p -> neg q) ^ (neg q -> neg r)) -> neg r


-- Especificación formal

-- Raúl está comiendo pastel. Si Raúl está comiendo pastel, no está jugando con su PS4. Si no está
-- jugando con su PS4 entonces su padre no pagará el seguro de la casa. Por tanto el padre de Raúl no
-- pagará el seguro de la casa.

expr1 = Implicacion (Conjuncion p (Conjuncion (Implicacion p (Negacion q)) (Implicacion (Negacion q) (Negacion r)))) (Negacion r)

-- Que el auditorio esté lleno es condición necesaria y suficiente para que la banda de rock toque. Si la
-- banda de rock toca entonces todos están cantando. Nadie canta. Por tanto el auditorio no está lleno.
-- Variables

-- u -> Auditorio está lleno

u = (FA (Var "u"))

-- v -> La banda de rock toca

v = (FA (Var "v"))

-- w -> Todos están cantando

w = (FA (Var "w"))

-- Traduccion de cada oracion

-- Si u entonces v, Si v entonces w, no w por lo tanto no u

-- (u -> v ^ v-> w ^¬w) -> ¬ u

--Identificación de Proposiciones

expr2 = Implicacion (Conjuncion (Conjuncion (Implicacion u v)(Implicacion v w)) (Negacion w)) (Negacion u)

exprapoyo = Conjuncion p (Conjuncion (Implicacion p (Negacion q)) (Implicacion p (Negacion r)))
