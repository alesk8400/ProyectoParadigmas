% ------------------------------------------------------------------------------
%
% UCR - Facultad de Ingeniería - ECCI
% CI-1441 Paradigmas Computacionales
% II-2015, Prof. Dr. Alvaro de la Ossa
% Alejandro Sudasassi Arroyo, A76440

% --- listas-componentes/2(+L,-R) ----------------------------------------------------------
%     R es la representación en componentes del grafo
%
% --- ejemplos:  listas-componentes([adj(a,[e,m,k]), adj(b,[a,c]),adj(c,[a,b]), adj(d,[b])],T).
%T = [[grafo([a, b, c, d], [arco(a, e), arco(a, k), arco(a, m), arco(b, a), arco(b, c), arco(..., ...)|...])]]. 
% No imprime la respuesta completa, pero esta sí está guardada en la base de datos de prolog.
%listas-componentes(L,R).

listas-componentes([],[]).
listas-componentes([A|B],[S]) :-  guardarTodasAdj([A|B]) , setof(X,Y^adj(X,Y),R), armarArcos(R), setof(arco(W,Z),arco(W,Z),M), armarRepresentacion(R,M), setof(grafo(U,V),grafo(U,V),S).

guardarTodasAdj([]).
guardarTodasAdj([X|Y]) :- assert(X) , guardarTodasAdj(Y).

armarArcos([]).
armarArcos([E|F]) :- setof(W,E^adj(E,W),G), armarArcosAux(E,G), armarArcos(F),!.

armarArcosAux([],[]).
armarArcosAux(_,[[]]).
armarArcosAux(H,[[J|L]|K]) :- assert(arco(H,J)) , armarArcosAux(H,[L]),!.

armarRepresentacion([],[]).
armarRepresentacion(R,M) :- assert(grafo(R,M)).




