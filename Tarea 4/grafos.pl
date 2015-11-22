
% CARLOS SANABRIA SANDOVAL CARNET A75952
% Definicion de los operandos
:- op(500, xfy, -).
:- op(500, xfy, >).


getnodos([],[]).
getnodos([H|T],D) :- getnodos(T,B), (atomic(H), M=[H] ; M=[]), append(B,M,D). 

% Ejemplo arcos-componentes "arcoscomponentes([a-b, x-y, c],D).  ->  D = grafo([a, b, c, x, y], [arco(a, b), arco(x, y)])".
% Ejemplo arcos-componentes "arcoscomponentes([c, a-b, a-d, b-d, d-g, e-h],D).  ->  D = grafo([a, b, c, d, e, g, h], [arco(a, b), arco(a, d), arco(b, d), arco(d, g), arco(e, h)]) ".
arcoscomponentes(A, U) :- getnodos(A,B1), findall(X, member(X-_, A),B2), append(B1,B2,B), findall(Q, member(_-Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V), member(N-V, A),F1), sort(F1,F), U= grafo(E,F).

% Ejemplo darcos-componentes "darcoscomponentes([a>b, x>y, c],D).  ->  D = digrafo([a, b, c, x, y], [arco(a, b), arco(x, y), arco(y, x)]) ".
% Ejemplo arcos-componentes "darcoscomponentes([c, a>b, a>d, b>d, d>g, e>h],D).  ->  D = digrafo([a, b, c, d, e, g, h], [arco(a, b), arco(a, d), arco(b, d), arco(d, g), arco(e, h)]) ".
darcoscomponentes(A, U) :- getnodos(A,B1), findall(X, member(X>_, A),B2), append(B1,B2,B), findall(Q, member(_>Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V), member(N>V, A),F1), sort(F1,F), U= digrafo(E,F).

% Ejemplo componentesarcos "componentesarcos(grafo([a,b,c,d,e,g,h], [arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),D). ->  D = [c, a-b, a-d, b-d, d-g, e-h]. ".
% Ejemplo componentesarcos "componentesarcos(grafo([a, b, c, x, y], [arco(a, b), arco(x, y)]),D). -> D = [c, a-b, x-y]".
primero([H|_],B) :-  B=H. 
getnodos2(A,C) :- findall(X1,member(X1-_,A),U1), findall(Y2,member(_-Y2,A),U2), append(U1,U2,C1), sort(C1,C).
componentesarcos(A, U) :- findall(N1, member(grafo(N1,V),[A]),N2), primero(N2,N), findall(V, member(grafo(_,V),[A]),U1), primero(U1,U2), findall(X-Y, member(arco(X,Y),U2),U3), getnodos2(U3,U4), subtract(N,U4,U5), append(U5,U3,U).

% Ejemplo dcomponentesarcos "dcomponentesarcos(digrafo([a,b,c,d,e,g,h], [arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),D).  -> D = [c, a>b, a>d, b>d, d>g, e>h].  "
% Ejemplo componentesarcos "dcomponentesarcos(digrafo([a, b, c, x, y], [arco(a, b), arco(x, y)]),D). -> D = [c, a>b, x>y]."
getnodos3(A,C) :- findall(X1,member(X1>_,A),U1), findall(Y2,member(_>Y2,A),U2), append(U1,U2,C1), sort(C1,C).
dcomponentesarcos(A, U) :- findall(N1, member(digrafo(N1,V),[A]),N2), primero(N2,N), findall(V, member(digrafo(_,V),[A]),U1), primero(U1,U2), findall(X>Y, member(arco(X,Y),U2),U3), getnodos3(U3,U4), subtract(N,U4,U5), append(U5,U3,U).


estaVacia([]).
getAdyacentes(G,N,V,R):-  findall(X, ( ( member(X-N, G); member(N-X, G) ), not(member(X,V))),R1), sort(R1, R). 

% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],i,a,R). -> R = [i, h, e, b, a]."
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],a,j,R). -> R = []."
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],f,d,R). -> R = [f, d]."
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],c,g,R). -> R = [c, b, d, g] "
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],a,i,R). -> R = [a, b, c, d, e, h, i] "
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],a,x,R). -> R = [] 

ruta(G,N1,N2,R) :- member(N2,G), R=[]; rutaR(G,N1,N2,[],R).
rutaR(G,N1,N2,V,R) :- (member(N1-N2,G); member(N2-N1,G)), R=[N1,N2]; append(V,[N1],V1), getAdyacentes(G,N1,V1,A),(estaVacia(A), R= []; recorrerAdj(G,A,N2,V1,R2), (estaVacia(R2), R= []; append([N1],R2,R))).
recorrerAdj(_,[],_,_,R):- R = [].
recorrerAdj(G,[H|T],N2,V,R):- rutaR(G,H,N2,V,R1),(estaVacia(R1), recorrerAdj(G,T,N2,V,R); R = R1). 

