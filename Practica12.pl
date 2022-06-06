/*
* Autor Leonardo Aguirre Muñoz
* Logica Computacional
* Practica 12
* Arboles binarios de busqueda
*/

arbolbb( vacio ) .
arbolbb( nodo(E , Ai , Ad )) :-
integer( E ) ,
mayor(E , Ai ) ,
menor(E , Ad ) ,
arbolbb( Ai ) ,
arbolbb( Ad ).
/**Funcion que determina si un numero dado es mayor a todos los elementos del arbol binario*/
mayor(X,vacio).
mayor(X, arbolbb(nodo(X1, Arbol_Izquierdo, Arbol_Derecho)))  :- X>X1, mayor(X, Arbol_Izquierdo),mayor(X, Arbol_Derecho).

/**Funcion que determina si un numero dado es el mas pequeño de todos los elementos presentes en el arbol binario*/

menor(X,vacio).
menor(X, arbolbb(nodo(X1, Arbol_Izquierdo, Arbol_Derecho)))  :- X<X1, menor(X, Arbol_Izquierdo),menor(X, Arbol_Derecho).

/**Funcion que determina si un elemento se encuentra en el arbol binario de busqueda*/

elemento(X, arbolbb(nodo(X, _, _))).
elemento(X, arbolbb(nodo(_, Arbol_Izquierdo, _)))  :- elemento(X, Arbol_Izquierdo).
elemento(X, arbolbb(nodo(_, _, Arbol_Derecho))) :- elemento(X, Arbol_Derecho).

/**Ejemplos

elemento(6,arbolbb(nodo(1,arbolbb(nodo(3,vacio,vacio)),arbolbb(nodo(6,vacio,vacio))))).

agrega(4,arbolbb(nodo(3,arbolbb(vacio),arbolbb(vacio))),A).

elimina(4,arbolbb(nodo(3,arbolbb(vacio),arbolbb(nodo(4,vacio,vacio)))),A).

sum_list(inorden(arbolbb(1,arbolbb(0,vacio,vacio),arbolbb(3,arbolbb(2,vacio,vacio),vacio)),A)).

Suma is Elemento_Derecho+Elemento_Izquierdo+Raiz.

promedio_elementos(arbolbb(1,arbolbb(0,vacio,vacio),arbolbb(3,arbolbb(2,vacio,vacio),vacio)),A).

suma_elementos(arbolbb(1,arbolbb(0,vacio,vacio),arbolbb(3,arbolbb(2,vacio,vacio),vacio)),A).

*/

/**Funcion que agrega un elemento nuevo al arbol binario*/

agrega(E,arbolbb(vacio),arbolbb(nodo(E,vacio,vacio))):-!.

/**Verifica si el elemento es menor para colocarlo en el lado izquierdo de la raiz principal, en caso contrario, sera ubicado en el lado derecho*/

/**Notese que se tiene un menor que, lo cual evita repeticiones de elementos del arbol*/

agrega(E,arbolbb(nodo(E1,Arbol_Izquierdo,Arbol_Derecho)),nodo(E1,Nuevo_Arbol_Izquierdo,Arbol_Derecho)):-
E<E1,
    !,
        agrega(E,Arbol_Izquierdo,Nuevo_Arbol_Izquierdo).

agrega(E,arbolbb(nodo(E1,Arbol_Izquierdo,Arbol_Derecho)),arbol(E1,Arbol_Izquierdo,Nuevo_Arbol_Derecho)):- agrega(E,Arbol_Derecho,Nuevo_Arbol_Derecho).


/**Funcion que elimina un elemento del arbol, verifica si esta en el lado izquierdo o derecho y principalmente si esta en el arbol*/

elimina(E,arbolbb(nodo(E,vacio,vacio)),arbolbb(vacio)):-!.

elimina(E,arbolbb(nodo(E1,Arbol_Izquierdo,Arbol_Derecho)),nodo(E1,Nuevo_Arbol_Izquierdo,Arbol_Derecho)):-
E<E1,
    !,
        elimina(E,Arbol_Izquierdo,Nuevo_Arbol_Izquierdo).

elimina(E,arbolbb(nodo(E1,Arbol_Izquierdo,Arbol_Derecho)),arbol(E1,Arbol_Izquierdo,Nuevo_Arbol_Derecho)):- elimina(E,Arbol_Derecho,Nuevo_Arbol_Derecho).


/**Funcion que hace el recorrido de arbol tipo Inorden, el algoritmo que se tiene es:
Se tiene un arbol que tiene raiz y nodos hijos izquierdo y derecho respectivamente.

Realiza recursion sobre el nodo izquierdo para verificar y procesar el nodo izquierdo-

Una vez realizado pasa a procesar la raiz del arbol.

Seguido de la recursion para el nodo derecho y finalizar el recorrido del arbol.

*/

inorden(arbolbb(Raiz,Arbol_Izquierdo,Arbol_Derecho),Suma):-
inorden(Arbol_Izquierdo,
    Elemento_Izquierdo),
        inorden(Arbol_Derecho,Elemento_Derecho),
            append(Elemento_Izquierdo,[Raiz|Elemento_Derecho],Suma).
inorden(vacio,[]).

/**Funcion que suma los elementos del arbol usando el recorrido Inorden

Usa la funcion auxiliar suma_list para sumar todos los elementos de la lista, es decir:

Los elementos del arbol sin contar los nodos vacios.

*/

suma_elementos(Arbol,SumaTotal):-inorden(Arbol,Lista),!,sum_list(Lista,SumaTotal).

/**Funcion que obtiene el promedio de la lista antes mencionada

Ahora usa una funcion auxiliar con otra funcion, una para hacer la suma de la lista que se obtiene al realizar el recorrido del arbol y la otra es para obtener el promedio de acuerdo al tamaño de la lista.

*/

promedio_elementos(Arbol,Promedio):-inorden(Arbol,Lista),!,mean(Lista,Promedio).

sum_list([],0).
sum_list([H|T],R):-sum_list(T,Rp), R is Rp+H.

mean([],0).
mean([H|T],R):- sum_list([H|T],S),length([H|T],L), R is S / L.

/*arbolbb(nodo(3,arbolbb(vacio),arbolbb(nodo(4,vacio,vacio))))*/

/*Comportamiento de robot*/

/**Estado actual del robot*/

estado(C,P,O):-

/**Funcion que dado un estado del robot, recibe una serie de instrucciones para moverse y de esta forma se obtiene la posicion final que tiene al realizar dichas acciones*/

status((0,0),north,C,P,O).

status((Xi,Yi),Oi,[],(Xi,Yi),Oi).
status((Xi,Yi),Oi,[C|T],Pf,Of):-
	execute((Xi,Yi),Oi,C,P1,O1),status(P1,O1,T,Pf,Of).

/**Funcion que simula un paso al frente que hace el robot sin alterar su orientacion*/

execute((Xi,Yi),north,move,(Xi,Yf),north):-
Yf is Yi+1.
execute((Xi,Yi),south,move,(Xi,Yf),south):-
Yf is Yi-1.
execute((Xi,Yi),east,move,(Xf,Yi),east):-
Xf is Xi+1.
execute((Xi,Yi),west,move,(Xf,Yi),west):-
Xf is Xi-1.

/**Funcion que hace que el robot de un paso atras sin modificar su orientacion*/

execute((Xi,Yi),north,back,(Xi,Yf),north):-
Yf is Yi-1.
execute((Xi,Yi),south,back,(Xi,Yf),south):-
Yf is Yi+1.
execute((Xi,Yi),east,back,(Xf,Yi),east):-
Xf is Xi-1.
execute((Xi,Yi),west,back,(Xf,Yi),west):-
Xf is Xi+1.

/**Funcion que hace retroceder dos pasos al robot sin modificar su orientacion*/

execute((Xi,Yi),north,back2,(Xi,Yf),north):-
Yf is Yi-2.
execute((Xi,Yi),south,back2,(Xi,Yf),south):-
Yf is Yi+2.
execute((Xi,Yi),east,back2,(Xf,Yi),east):-
Xf is Xi-2.
execute((Xi,Yi),west,back2,(Xf,Yi),west):-
Xf is Xi+2.

/**Funcion que hace caminar dos pasos al robot sin verse afectado su orientacion*/

execute((Xi,Yi),north,move2,(Xi,Yf),north):-
Yf is Yi+2.
execute((Xi,Yi),south,move2,(Xi,Yf),south):-
Yf is Yi-2.
execute((Xi,Yi),east,move2,(Xf,Yi),east):-
Xf is Xi+2.
execute((Xi,Yi),west,move2,(Xf,Yi),west):-
Xf is Xi-2.

/**Funcion que realiza un movimiento en forma de L en direccion hacia la izquierda realizando un giro para volver a la orientacion que estaba antes de moverse*/

execute((Xi,Yi),north,knightL,(Xf,Yf),north):-
Yf is Yi+2,Xf is Xi-1.
execute((Xi,Yi),south,knightL,(Xf,Yf),south):-
Yf is Yi-2,Xf is Xi+1.
execute((Xi,Yi),east,knightL,(Xf,Yf),east):-
Xf is Xi+2,Yf is Yi+1.
execute((Xi,Yi),west,knightL,(Xf,Yf),west):-
Xf is Xi-2,Yf is Yi-1.

/**Funcion que realiza el mismo movimiento anterior en direccion hacia la derecha*/

execute((Xi,Yi),north,knightR,(Xf,Yf),north):-
Yf is Yi+2,Xf is Xi+1.
execute((Xi,Yi),south,knightR,(Xf,Yf),south):-
Yf is Yi-2,Xf is Xi-1.
execute((Xi,Yi),east,knightR,(Xf,Yf),east):-
Xf is Xi+2,Yf is Yi-1.
execute((Xi,Yi),west,knightR,(Xf,Yf),west):-
Xf is Xi-2,Yf is Yi+1.

/**Funcion que hace girar hacia la izquierda al robot, modificando la orientacion*/

execute((Xi,Yi),Oi,left,(Xi,Yi),Of):-
turn(Oi,left,Of).

/**Funcion que hace girar hacia la derecha al robot, así como su orientacion*/

execute((Xi,Yi),Oi,right,(Xi,Yi),Of):-
turn(Oi,right,Of).

/**Funcion que hace al robot dar media vuelta, de acuerdo a su orientacion*/

execute((Xi,Yi),Oi,turn180,(Xi,Yi),Of):-
turn180(Oi,turn180,Of).

/**Funcion auxiliar que realiza las respectivas rotaciones de acuerdo a la direccion que está mirando el robot*/

turn180(north,turn180,south).
turn180(south,turn180,north).
turn180(east,turn180,west).
turn180(west,turn180,east).

/**Funcion auxiliar que realiza las respectivas rotaciones hacia la izquierda de acuerdo a la direccion que está mirando el robot*/

turn(north,left,west).
turn(south,left,east).
turn(east,left,north).
turn(west,left,south).

/**Funcion auxiliar que realiza las respectivas rotaciones hacia la derecha de acuerdo a la direccion que está mirando el robot*/

turn(north,right,east).
turn(south,right,west).
turn(east,right,south).
turn(west,right,north).
