caLista([],R):-R=false.
caLista([H|_],R):- caLista(H,R).
caLista([H|_],H).

