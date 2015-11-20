
% Definicion de los operandos
:- op(500, xfy, -).
:- op(500, xfy, >).


getnodos([],[]).
getnodos([H|T],D) :- getnodos(T,B), (atomic(H), M=[H] ; M=[]), append(B,M,D). 

% Ejemplo arcos-componentes "arcoscomponentes([a-b, x-y, c],D).  ->  D = grafo([a, b, c, x, y], [arco(a, b), arco(x, y)])".
arcoscomponentes(A, U) :- getnodos(A,B1), findall(X, member(X-_, A),B2), append(B1,B2,B), findall(Q, member(_-Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V), member(N-V, A),F1), sort(F1,F), U= grafo(E,F).

% Ejemplo darcos-componentes "darcoscomponentes([a>b, x>y, c],D).  ->  D = digrafo([a, b, c, x, y], [arco(a, b), arco(x, y), arco(y, x)]) ".
darcoscomponentes(A, U) :- getnodos(A,B1), findall(X, member(X>_, A),B2), append(B1,B2,B), findall(Q, member(_>Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V), member(N>V, A),F1), sort(F1,F), U= digrafo(E,F).

% Ejemplo componentesarcos "componentesarcos(grafo([a,b,c,d,e,g,h], [arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),D). ->  D = [c, a-b, a-d, b-d, d-g, e-h]. ".
primero([H|_],B) :-  B=H. 
getnodos2(A,C) :- findall(X1,member(X1-_,A),U1), findall(Y2,member(_-Y2,A),U2), append(U1,U2,C1), sort(C1,C).
componentesarcos(A, U) :- findall(N1, member(grafo(N1,V),[A]),N2), primero(N2,N), findall(V, member(grafo(_,V),[A]),U1), primero(U1,U2), findall(X-Y, member(arco(X,Y),U2),U3), getnodos2(U3,U4), subtract(N,U4,U5), append(U5,U3,U).

% Ejemplo dcomponentesarcos "dcomponentesarcos(digrafo([a,b,c,d,e,g,h], [arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),D).  -> D = [c, a>b, a>d, b>d, d>g, e>h].  "
getnodos3(A,C) :- findall(X1,member(X1>_,A),U1), findall(Y2,member(_>Y2,A),U2), append(U1,U2,C1), sort(C1,C).
dcomponentesarcos(A, U) :- findall(N1, member(digrafo(N1,V),[A]),N2), primero(N2,N), findall(V, member(digrafo(_,V),[A]),U1), primero(U1,U2), findall(X>Y, member(arco(X,Y),U2),U3), getnodos3(U3,U4), subtract(N,U4,U5), append(U5,U3,U).


ruta(G,N1,N2,R) :- member(N1-N2,G), R=[N1-N2]; rutaR().