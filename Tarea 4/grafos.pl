posicion(X, [X|_], 1).
posicion(X, [_|R], P) :- posicion(X,R,M), P is M + 1. 
%Metodo que resuelve las adyacencias
adj(_,[],[]).	
adj(X,[A|B],P) :- adj(X,B,C) , append([[X,A]], C, P),!.
% adj(X,[A|B], R) :-  
test(_,[A|B],P) :- append([x,y,s], [A|B], P).
test2([A|B],A).
listascomponentes([], []).
listascomponentes([H|T], X) :- H, listascomponentes(T,_).
%listascomponentes([H|T], X) :- append(H, listascomponentes(T,_), X).
:- op(500, xfy, -).
:- op(500, xfy, >).

get-nodos([],[]).
get-nodos([H|T],D) :- findall(X, H\=X-Y, B), get-nodos(T,C), append(B,C,D).  

componentes-arcos([], []).
componentes-arcos(A, U) :- findall(X, not(member(X-_, A)),B1), findall(X, member(X-_, A),B2), append(B1,B2,B), findall(Q, member(_-Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V),member(N-V, A),F), append([E], F, U).

dcomponentes-arcos([], []).
dcomponentes-arcos(A, U) :- findall(X, member(X>_, A),B), findall(Q, member(_>Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V),member(N>V, A),F), append([E], F, U).


%findall(X,member(element(_,X), Elements),Numbers).