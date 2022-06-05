myconcat(A,B,R):- string_concat(A,B,R),write(R).
%trace., notrace. , nodebug.
1+2=:=3.
1+2=\=3.
= sintactico
=:= aritmetica
is evaluar

1<2.
True
1=<3.
True
1=>3.
False
1>=3.
False

X is 2+(3.1*13-max(17,13)/(2**5)).
X=0.20937
X is 2+3+4
X=9.

X is 10//6 para resolucion entera.
X=1.6666666666

X is 10 mod 6
X=4

repeatString(0,_,'').
repeatString(N,C,Cf):- Np is N-1, repeatString(Np,C,C1),string_concat(C,C1,Cf).

square (N,C):- repeatString(N,C,R),string_concat(R,'\n',Rp), repeatString(N,Rp,Cf),write(Cf).
square(5,'*')

distance((X,Y),(A,B),D):-D is sqrt((X-A)**2+(Y-B)**2).


sum_list([],0).
sum_list([H|T],R):-sum_list(T,Rp), R is Rp+H.

mean([],0).
mean([X|XS],R):- sum_list([H|T],S),length([H|T],L), R is S / L.

Range (L,U,X) :- [L,L+1,...,U]
Range (1,5,X)= 
X=[1,2,3,4,5]

range(L,L,[L]).
range(L,U,[]):-U<L.
range (L,U,R):- L1 is L+1, range(L1,U,Rp),append([L],Rp,R).

estado(C,P,O):-
status((0,0),north,C,P,O).

status((Xi,Yi),Oi,[],(Xi,Yi),Oi).
status((Xi,Yi),Oi,[C|T],Pf,Of):-
	execute((Xi,Yi),Oi,C,P1,O1),status(P1,O1,T,Pf,Of).

execute((Xi,Yi),north,move,(Xi,Yf),north):-
Yf is Yi+1.
execute((Xi,Yi),south,move,(Xi,Yf),south):-
Yf is Yi-1.
execute((Xi,Yi),east,move,(Xf,Yi),east):-
Xf is Xi+1.
execute((Xi,Yi),west,move,(Xf,Yi),west):-
Xf is Xi-1.

execute((Xi,Yi),Oi,left,(Xi,Yi),Of):-
turn(Oi,left,Of).

execute((Xi,Yi),Oi,right,(Xi,Yi),Of):-
turn(Oi,right,Of).

turn(north,left,west).
turn(south,left,east).
turn(east,left,north).
turn(west,left,south).

turn(north,right,east).
turn(south,right,west).
turn(east,right,south).
turn(west,right,north).

random_between(0,10,X).
