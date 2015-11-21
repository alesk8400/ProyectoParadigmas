% ------------------------------------------------------------------------------
%
% UCR - Facultad de Ingeniería - ECCI
% CI-1441 Paradigmas Computacionales
% II-2015, Prof. Dr. Alvaro de la Ossa
% Alejandro Sudasassi Arroyo, A76440

% --- listasComponentes/2(+L,-R) ----------------------------------------------------------
%     R es la representación en componentes del grafo
%
% --- ejemplos:  listasComponentes([adj(a,[e,b,k]), adj(b,[a,c]),adj(c,[a,b]), adj(d,[b])],T).
%T = [[grafo([a, b, c, d], [arco(a, b), arco(a, e), arco(a, k), arco(b, a), arco(b, c), arco(..., ...)|...])]].
% No imprime la respuesta completa, pero esta sí está guardada en la base de datos de prolog.
%Implementación funciona con grafos dirigidos y no dirigidos indistintamente:
%listasComponentes([adj(a,[b]), adj(b,[a,c]), adj(c,[a,b]), adj(d,[b]), adj(e,[])],T).
%T = [[grafo([a, b, c, d, e], [arco(b, a), arco(b, c), arco(c, a), arco(c, b), arco(d, b)])]].

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
% --- ejemplos: 

componentesListas([],[]).
componentesListas(A,[S]) :- assert(arco(_,[])), assert(arco([],_)), guardarGrafo(A), setof(X,Y^grafo(X,Y),R), setof(N,M^grafo(M,N),O), guardarArcos(O), armarListas(R).

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
%assert(arco(_,[])), 
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