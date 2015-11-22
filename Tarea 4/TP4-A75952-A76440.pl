% ------------------------------------------------------------------------------
%
% UCR - Facultad de Ingeniería - ECCI
% CI-1441 Paradigmas Computacionales
% II-2015, Prof. Dr. Alvaro de la Ossa
% Tarea 4
% Carlos Sanabria Sandoval, A75952 || Alejandro Sudasassi Arroyo, A76440
% Definicion de los operandos
:- op(500, xfy, -).
:- op(500, xfy, >).

getnodos([],[]).
getnodos([H|T],D) :- getnodos(T,B), (atomic(H), M=[H] ; M=[]), append(B,M,D). 

% --- listasComponentes/2(+L,-R) ----------------------------------------------------------
%     R es la representación en componentes del grafo
%
% --- ejemplos: listasComponentes([adj(a,[b]), adj(b,[a,c]), adj(c,[a,b]), adj(d,[b]), adj(e,[])],T).
%T = [[grafo([a, b, c, d, e], [arco(b, a), arco(b, c), arco(c, a), arco(c, b), arco(d, b)])]].
% No imprime la respuesta completa, pero esta sí está guardada en la base de datos de prolog.
%Implementación funciona con grafos dirigidos y no dirigidos indistintamente:


listasComponentes([],[]).
listasComponentes([A|B],[S]) :-  guardarTodasAdj([A|B]) , setof(X,Y^adj(X,Y),R), armarArcos(R), setof(arco(W,Z),arco(W,Z),M), armarRepresentacion(R,M), setof(grafo(U,V),grafo(U,V),S).

guardarTodasAdj([]).
guardarTodasAdj([X|Y]) :- assert(X) , guardarTodasAdj(Y).

armarArcos([]).
armarArcos([E|F]) :- setof(W,E^adj(E,W),G), armarArcosAux(E,G), armarArcos(F),!.

armarArcosAux([],[]).
armarArcosAux(_,[[]]).
armarArcosAux(H,[[J|L]|K]) :- assert(arco(H,J)) , armarArcosAux(H,[L]),!.

armarRepresentacion([],[]).
armarRepresentacion(R,M) :- assert(grafo(R,M)).

% --- componentesListas/2(+L,-R) ----------------------------------------------------------
%     R es la representación en listas de adyacencia del grafo no dirigido
%
% --- ejemplos: componentesListas(grafo([a,b,c,d,e,g,h],[arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),T).
% adj(a, [b, d, [], []]) adj(b, [d, [], a, []]) adj(c, [[], []]) adj(d, [g, [], a, b, []]) adj(e, [h, [], []]) adj(g, [[], d, []]) adj(h, [[], e, []]).
%Nota: para que el programa trabajara de manera correcta los nodos aislados se incluyó
%el assert(arco(_,[])), lo cual causa que quede un item vacío añadido a cada lista de adyacencia, pero aún así,
%la respuesta del programa es correcta. 

componentesListas([],[]).
componentesListas(A,[S]) :- assert(arco(_,[])), assert(arco([],_)), guardarGrafo(A), setof(X,Y^grafo(X,Y),R), setof(N,M^grafo(M,N),O), guardarArcos(O), armarListas(R), setof(adj(U,V),adj(U,V),S).

armarListas([]).
armarListas([[J|L]|K]) :- setof(W,J^arco(J,W),G), setof(W,J^arco(W,J),H), append(G,H,F), armarListasAux(J,F), armarListas(L),!.
armarListas([J|L]) :- setof(W,J^arco(J,W),G), setof(W,J^arco(W,J),H), append(G,H,F), armarListasAux(J,F), armarListas(L),!.

armarListasAux([],[]).
armarListasAux(T,U) :- assert(adj(T,U)),!.

guardarGrafo([]).
guardarGrafo(A) :- assert(A).

guardarArcos([]).
guardarArcos([[X|Y]|K]) :- assert(X), guardarArcos(Y),!.
guardarArcos([X|Y]) :- assert(X), guardarArcos(Y),!.

% --- diComponentesListas/2(+L,-R) ----------------------------------------------------------
%     R es la representación en listas de adyacencia del grafo dirigido
%
% --- ejemplos: diComponentesListas(grafo([a,b,c,d,e,g,h],[arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),T).
%T = [[adj(a, [b, d, []]), adj(b, [d, []]), adj(c, [[]]), adj(d, [g, []]), adj(e, [h, []]), adj(g, [[]]), adj(h, [...])]].
%Nota: para que el programa trabajara de manera correcta los nodos aislados se incluyó
%el assert(arco(_,[])), lo cual causa que quede un item vacío añadido a cada lista de adyacencia, pero aún así,
%la respuesta del programa es correcta. Además no imprime la respuesta completa, pero esta sí está guardada en la base de datos de prolog.

diComponentesListas([],[]).
diComponentesListas(A,[S]) :- diGuardarGrafo(A), setof(X,Y^digrafo(X,Y),R), setof(N,M^digrafo(M,N),O), diGuardarArcos(O), diArmarListas(R), setof(adj(U,V),adj(U,V),S).

diArmarListas([]).
diArmarListas([[J|L]|K]) :- setof(W,J^arco(J,W),G), diArmarListasAux(J,G), diArmarListas(L),!.
diArmarListas([J|L]) :- setof(W,J^arco(J,W),G), diArmarListasAux(J,G), diArmarListas(L),!.

diArmarListasAux([],[]).
diArmarListasAux(T,U) :- assert(adj(T,U)),!.

diGuardarGrafo([]).
diGuardarGrafo(A) :- assert(A).

diGuardarArcos([]) :- assert(arco(_,[]) :- !).
diGuardarArcos([[X|Y]|K]) :- assert(X), diGuardarArcos(Y),!.
diGuardarArcos([X|Y]) :- assert(X), diGuardarArcos(Y),!.


% --- arcos-componentes/2(+L,-U) ----------------------------------------------------------
%     U es la representación en componentes del grafo L
%
% Ejemplo arcos-componentes "arcoscomponentes([a-b, x-y, c],D).  ->  D = grafo([a, b, c, x, y], [arco(a, b), arco(x, y)])".
% Ejemplo arcos-componentes "arcoscomponentes([c, a-b, a-d, b-d, d-g, e-h],D).  ->  D = grafo([a, b, c, d, e, g, h], [arco(a, b), arco(a, d), arco(b, d), arco(d, g), arco(e, h)]) ".
arcoscomponentes(A, U) :- getnodos(A,B1), findall(X, member(X-_, A),B2), append(B1,B2,B), findall(Q, member(_-Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V), member(N-V, A),F1), sort(F1,F), U= grafo(E,F).

% --- darcos-componentes/2(+L,-U) ----------------------------------------------------------
%     U es la representación en componentes del grafo dirigido L
%
% Ejemplo darcos-componentes "darcoscomponentes([a>b, x>y, c],D).  ->  D = digrafo([a, b, c, x, y], [arco(a, b), arco(x, y), arco(y, x)]) ".
% Ejemplo arcos-componentes "darcoscomponentes([c, a>b, a>d, b>d, d>g, e>h],D).  ->  D = digrafo([a, b, c, d, e, g, h], [arco(a, b), arco(a, d), arco(b, d), arco(d, g), arco(e, h)]) ".
darcoscomponentes(A, U) :- getnodos(A,B1), findall(X, member(X>_, A),B2), append(B1,B2,B), findall(Q, member(_>Q, A),C), append(B,C,D), sort(D,E), findall(arco(N,V), member(N>V, A),F1), sort(F1,F), U= digrafo(E,F).


% --- componentesarcos/2(+L,-U) ----------------------------------------------------------
%     U es la representación en arcos del grafo L
%
% Ejemplo componentesarcos "componentesarcos(grafo([a,b,c,d,e,g,h], [arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),D). ->  D = [c, a-b, a-d, b-d, d-g, e-h]. ".
% Ejemplo componentesarcos "componentesarcos(grafo([a, b, c, x, y], [arco(a, b), arco(x, y)]),D). -> D = [c, a-b, x-y]".
primero([H|_],B) :-  B=H. 
getnodos2(A,C) :- findall(X1,member(X1-_,A),U1), findall(Y2,member(_-Y2,A),U2), append(U1,U2,C1), sort(C1,C).
componentesarcos(A, U) :- findall(N1, member(grafo(N1,V),[A]),N2), primero(N2,N), findall(V, member(grafo(_,V),[A]),U1), primero(U1,U2), findall(X-Y, member(arco(X,Y),U2),U3), getnodos2(U3,U4), subtract(N,U4,U5), append(U5,U3,U).

% --- dcomponentesarcos/2(+L,-U) ----------------------------------------------------------
%     U es la representación en arcos del grafo dirigido L
%
% Ejemplo dcomponentesarcos "dcomponentesarcos(digrafo([a,b,c,d,e,g,h], [arco(a,b), arco(a,d), arco(b,d), arco(d,g), arco(e,h)]),D).  -> D = [c, a>b, a>d, b>d, d>g, e>h].  "
% Ejemplo componentesarcos "dcomponentesarcos(digrafo([a, b, c, x, y], [arco(a, b), arco(x, y)]),D). -> D = [c, a>b, x>y]."
getnodos3(A,C) :- findall(X1,member(X1>_,A),U1), findall(Y2,member(_>Y2,A),U2), append(U1,U2,C1), sort(C1,C).
dcomponentesarcos(A, U) :- findall(N1, member(digrafo(N1,V),[A]),N2), primero(N2,N), findall(V, member(digrafo(_,V),[A]),U1), primero(U1,U2), findall(X>Y, member(arco(X,Y),U2),U3), getnodos3(U3,U4), subtract(N,U4,U5), append(U5,U3,U).




% --- ruta/4(G,N1,N2,R) ----------------------------------------------------------
%     R es la representación de la ruta de N1 a N2 en el grafo G
%
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],i,a,R). -> R = [i, h, e, b, a]."
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],a,j,R). -> R = []."
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],f,d,R). -> R = [f, d]."
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],c,g,R). -> R = [c, b, d, g] "
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],a,i,R). -> R = [a, b, c, d, e, h, i] "
% Ejemplo ruta([a-b,j,b-d,b-c,b-e,c-d,d-g,c-f,d-e,c-e,i-h,d-f,h-e,z-x],a,x,R). -> R = [] 

estaVacia([]).
getAdyacentes(G,N,V,R):-  findall(X, ( ( member(X-N, G); member(N-X, G) ), not(member(X,V))),R1), sort(R1, R). 

ruta(G,N1,N2,R) :- member(N2,G), R=[]; rutaR(G,N1,N2,[],R).
rutaR(G,N1,N2,V,R) :- (member(N1-N2,G); member(N2-N1,G)), R=[N1,N2]; append(V,[N1],V1), getAdyacentes(G,N1,V1,A),(estaVacia(A), R= []; recorrerAdj(G,A,N2,V1,R2), (estaVacia(R2), R= []; append([N1],R2,R))).
recorrerAdj(_,[],_,_,R):- R = [].
recorrerAdj(G,[H|T],N2,V,R):- rutaR(G,H,N2,V,R1),(estaVacia(R1), recorrerAdj(G,T,N2,V,R); R = R1). 

