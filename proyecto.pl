nodo(X,[]) :- write('nodo -- caso base '), writeln(X),
				!,integer(X)>0.

nodo(X, [Head|Tail]) :- write('nodo -- caso recursivo '),writeln(X),
					!,integer(X)>0,
					Head,
					nl,
					auxiliar(Tail).

nodo(_) :- !, writeln('Error: Nodo no cumple con la sintaxis requerida.'),
			false.

arista(X,Hijo) :- write('arista -- unico caso '),writeln(X),
					!,integer(X)>0,
					Hijo.

arista(_) :- !, writeln('Error: Arista no cumple con la sintaxis requerida.'),
				false.

% Predicado utilizado para separar head y tail de un arreglo.
% Sirve para ejecutar cada arista de un nodo, tiene caso base y recursivo.
auxiliar([]) :- true.
auxiliar([Head|Tail]) :- !,Head,
						auxiliar(Tail).

bienEtiquetado(nodo(X,[])) :- !,writeln('Caso base bienEtiquetado '),
								(integer(X)=<0 -> writeln('Arbol no esta bien etiquetado: Etiqueta de nodo es menor o igual a 0.'), false);
								lnodos = [],
								laristas = [],
								append(X,lnodos,lnodos).
%								nb_getval(lnodos,Lista),
%								(not(nb_getval(lnodos,ListaN)) -> nb_setval(Lnodos,[]));
%								nb_getval(lnodos,ListaN),
%								(member(X,lnodos) -> writln('Error: Etiqueta de Nodo ya existente.'),false);
%								append(X,lnodos,lnodos).

bienEtiquetado(X,arista(Y,nodo(Z,[]))) :- !,writeln('Caso base bienEtiquetado '), not(X = Z),
											(not(Y =:= abs(X-Z)) -> writeln('Arbol no esta bien etiquetado: Etiqueta de nodo es menor o igual a 0.')), false;
											Lnodos = [],
											Laristas=[],
											append(X,Lnodos,Lnodos),
											append(Z,Lnodos,Lnodos),
											append(Y,Laristas,Laristas).
bienEtiquetado(X,arista(Y,nodo(Z,Lista))) :- !, Lnodos = [],
											Laristas = [],
											bienEtiquetado_Aristas(X,Y,Z,Laristas),
											bienEtiquetado_Nodos(X,Z,Lnodos,Laristas,Lista).
% A LOS PREICADOS AUXILIARES QUE ESTAN ABAJO HAY QUE PASARLES NODO Y ARISTA COMO A LOS NORMALES
% JUNTO CON LAS LISTAS PARA QUE SIRVA

bienEtiquetado_Nodos(nodo(X,[Head|Tail]),Lnodos,Laristas) :- not(member(X,Lnodos)),
													not(member(Z,Lnodos)),
													append(X,Lnodos,Lnodos),
													bienEtiquetado_Aristas(Head,Laristas,Lnodos,X),
													bienEtiquetado_Aristas(Tail,Laristas,Lnodos,X).

bienEtiquetado_Aristas([],Laristas,Laristas).

bienEtiquetado_Aristas(arista(X,nodo(Y,Hijos)),Laristas,Lnodos,W) :- !,not(Y = W),
											(not(X =:= abs(W-Y)) -> writeln('Arbol no esta bien etiquetado: Etiqueta de nodo es menor o igual a 0.'), false);
											not(member(X,Laristas)),
											append(Y,Laristas,Laristas),
											bienEtiquetado_Nodos(nodo(Y,Hijos),Lnodos,Laristas).
											%bienEtiquetado_Aristas([],Laristas,Laristas).


% bienEtiquetado(nodo(V,[arista(E,nodo(W,[]))])) :- append([W], ListaNodos, ListaNodos)