posicion(X, [X|_], 1).
posicion(X, [_|R], P) :- posicion(X,R,M), P is M + 1. 
%Metodo que resuelve las adyacencias
adj(_,[],[]).	
adj(X,[A|B],P) :- adj(X,B,C) , append([[X,A]], C, P),!.

% Definicion de los operandos
:- op(500, xfy, -).
:- op(500, xfy, >).

get-nodos([],[]).
get-nodos([H|T],D) :- get-nodos(T,B), (atomic(H), M=[H] ; M=[]), append(B,M,D). 

% Ejemplo componentes-arcos([a-a, h-a, t, b-a, u, a-a, t], X).
arcos-componentes(A, U) :- get-nodos(A,B1), findall(X, member(X-_, A),B2), append(B1,B2,B), findall(Q, member(_-Q, A),C), append(B,C,D), sort(D,E), setof(arco(N,V),member(N-V, A),F), append([E], F, U).

dcomponentes-arcos([], []).
dcomponentes-arcos(A, U) :- findall(X, member(X>_, A),B), findall(Q, member(_>Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V),member(N>V, A),F), append([E], F, U).

componentes-arcos(A, U) :- findall(N-V, member(arco(N,V), A),U).