
% consult('H:/Mi unidad/universidad/PROGRAMACION 2/base.pl').

masGrande(elefante,caballo).
masGrande(caballo,perro).
masGrande(perro,gato).
masGrande(gato,raton).
masGrande(raton,hormiga).


% ---------------------------------------------------------
% REGLAS RECURSIVAS
%	1) COMENZAR CON EL PREDICADO DE PARADA
%	2) EVITAR LA RECURSIVIDAD Á LA IZQUIERDA


muchoMasGrande(A,B):-masGrande(A,B).
muchoMasGrande(A,B):-masGrande(A,X),muchoMasGrande(X,B).




%						ARBOL GENEALOGICO
% -----------------------------------------------------------

esPadre(martin).
esPadre(luis).
padreDe(martin,luis).
padreDe(luis,jose).
padreDe(luis,pedro).

% -------------REGLAS----------------------

hijoDe(A,B):-padreDe(B,A).
abueloDe(A,B):-padreDe(A,X),padreDe(X,B).
hermanoDe(A,B):-esPadre(X), padreDe(X,A),padreDe(X,B).
familiarDe(A,B):- padreDe(A,B); hijoDe(A,B);hermanoDe(A,B); abueloDe(A,B).



% ---------------------FIBONACCI RECURSIVO---------------------
fibo(0,0).
fibo(1,1).
fibo(N,R):-N>1,N1 is N-1,N2 is N-2,fibo(N1,R1),fibo(N2,R2),R is R1+R2.


% ---------------------MULTIPLICACION RUSA---------------------

mrusa(0,_,0).
mrusa(_,0,0).
mrusa(1,B,B).
mrusa(A,1,A).
mrusa(A,B,R):- A>1,1 is A mod 2,A1 is A//2,B1 is B*2,mrusa(A1,B1,R1), R is B+R1;
               A>1,0 is A mod 2,A1 is A//2,B1 is B*2,mrusa(A1,B1,R).

% -----------------------SUMA RECURSIVA-----------------------

suma(0,0).
suma(N,R):- Nt is N-1, suma(Nt,Rt), R is N + Rt.



% ----------------------DIVISION RECURSIVA----------------------

divisionR(A,B,0):- B>A.
divisionR(A,B,R1):- T is A-B,divisionR(T,B,P),R1 is P+1.

% ----------------------------FACTORIAL----------------------------

factorialR(0,1).
factorialR(N,R):- N>0, N1 is N-1,factorialR(N1,R1),R is N*R1.

%---------------------MULTIPLICACION CON SUMA---------------------
multi(_,0,0).
multi(A,B,R):- B>0,B1 is B-1,multi(A,B1,R1),R is A+R1.




% ------------------------DECIMAL A BINARIO------------------------

binario(0,'0').
binario(1,'1').
binario(X,Y):- X>1 ,X1 is X mod 2, X2 is X//2,
    binario(X2,Y1), atom_concat(Y1,X1,Y).

% ------------------------BINARIO A DECIMAL------------------------

% ----------------------POTENCIA RECURSIVA----------------------

potenciaR(0,1).
potenciaR(P,R):-P>0,P1 is P-1,potenciaR(P1,R1),R is 2 * R1.

prueba(N,R):- atom_length(N,Expo),R is N^Expo.
pruebas(0,_,0).
pruebas(N,C,R):- N>0,N1 is N-1, C1 is C+1,pruebas(N1,C1,R1),R is R1+N+C1.



binarioR(N1,N2,N3,N4,N5,N6,N7,R):-
                            R1 is N1*2^6,
                            R2 is N2*2^5,
                            R3 is N3*2^4,
                            R4 is N4*2^3,
                            R5 is N5*2^2,
                            R6 is N6*2^1,
                            R7 is N7*2^0,
                             R is R1+R2+R3+R4+R5+R6+R7.



binarioRecursivo(0,_,0).
binarioRecursivo(1,0,1).

binarioRecursivo(N,C,R):- N1 is N//10,C1 is C+1,X is N mod 10,binarioRecursivo(N1,C1,R1),
                R is R1+(X*2^C).


%-----------------------------LISTAS-----------------------------


%-DEVOLVER EL PRIMER DATO DE LA LISTA-
listaC([H|_],H).


%-MOSTRAR LA POSICION DE UN DATO BUSCADO-
buscarDato([H|_],H,C,C).
buscarDato([],_,_,'false').
buscarDato([_|T],V,C,R):- C1 is C+1,buscarDato(T,V,C1,R).

%-CONTAR LOS ELEMENTOS DE UNA LISTA-
contar([],0).
contar([_|T],Total):- contar(T,Aux), Total is Aux +1.








%-MOSTRAR EL PRIMER DATO DE UNA LISTA ANIDADA

caLista([],R):-R=false.
caLista([H|_],R):- caLista(H,R).
caLista([H|_],H).















