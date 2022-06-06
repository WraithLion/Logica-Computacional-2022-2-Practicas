/**
* Autor: Leonardo Aguirre Muñoz
* Logica Computacional 7059
* Practica 11
* Facultad de Ciencias
*/

/**Conversion de enunciados a Prolog*/

asesino(DarthVader).
jedi(ObiWan,Luke).
muerto(ObiWan).
ladoOscuro(mata):-enseñar(Luke).
ahorca(DarthVader):-interrumpe(Alguien).
:-pierde(snowspeeder),pierde(mano).


/**Forma Normal Negada de una formula \phi y/o / \psi*/

/**Reglas de precedencia*/

:-op(600 , xfy , sii ).
:-op(500 , xfy , imp ).
:-op(400 , xfy , and ).
:-op(300 , xfy , or ).
:-op(200 , fx , neg ).

/**Representacion de una formula*/

form( X ):-atom( X ).
form( neg X ) : - form( X ) .
form(( X or Y ) ) : - form( X ) , form( Y ).
form(( X and Y ) ) : - form( X ) , form( Y ).
form(( X imp Y ) ) : - form( X ) , form( Y ).
form(( X sii Y ) ) : - form( X ) , form( Y ).

/**Reglas para Forma Normal Negada*/

fnn(X,X):- atom(X).
fnn(neg X, neg X):- atom(X).
fnn(neg (neg X), R):- fnn(X,R).
fnn(X or Y, Rx or Ry ):- fnn(X,Rx), fnn(Y,Ry).
fnn(X and Y, Rx and Ry ):- fnn(X,Rx), fnn(Y,Ry).
fnn(X imp Y, R ):- fnn(neg X or Y,R).
fnn(X sii Y, R):- fnn((neg X or Y) and (neg Y or X),R).
fnn(neg (X or Y), R1 and R2):- fnn(neg X, R1),fnn(neg Y,R2).
fnn(neg (X and Y), R1 or R2):- fnn(neg X, R1),fnn(neg Y,R2).
fnn(neg (X imp Y), R):- fnn(X and neg Y, R).
fnn(neg (X sii Y), R):- fnn(neg((neg X or Y) and (neg Y or X)),R).
