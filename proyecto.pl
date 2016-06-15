nodo(X,[]) :- write('nodo -- caso base '), writeln(X),
				!,integer(X)>0.

nodo(X, [Head|Tail]) :- write('nodo -- caso recursivo '),writeln(X),
					!,integer(X)>0,
					Head,
					nl,
					auxiliar(Tail).

nodo(_) :- !, writeln('Error: Nodo no cumple con la sintaxis requerida.'),
			false.

arista(X,Hijo) :- write('arista -- unico caso '), writeln(X),
					!,integer(X)>0,
					Hijo.

arista(_) :- !, writeln('Error: Arista no cumple con la sintaxis requerida.'),
				false.

% Predicado utilizado para separar head y tail de un arreglo.
% Sirve para ejecutar cada arista de un nodo, tiene caso base y recursivo.
auxiliar([]) :- true.
auxiliar([Head|Tail]) :- !,Head,
						auxiliar(Tail).

bienEtiquetado(nodo(X,[])) :- writeln('Caso base Num 1 bienEtiquetado '), 
								(integer(X)=<0 -> writeln('Arbol no esta bien etiquetado: Etiqueta de nodo es menor o igual a 0.'), false);
								!,integer(X)>0,
								Lnodos = [],
								Laristas = [],
								append([X],Lnodos,Lnodos1),
								writeln(Lnodos1),
								writeln(Laristas).

bienEtiquetado(nodo(X,[arista(Y,nodo(Z,[]))])) :- !,writeln('Caso base Num 2 bienEtiquetado '),
											not(X = Z),
											P is abs(X-Z),
											writeln(Y), writeln(P),
											not(Y =:= P) -> (writeln('Arbol no esta bien etiquetado: Etiqueta de arista es distinta a diferencia entre padre e hijo.'), false);
											Lnodos = [], Laristas=[],
											append([X],Lnodos,Lnodos1),
											append([Z],Lnodos1,Lnodos2),
											append([Y],Laristas,Laristas1),
											writeln('Lista de Nodos. '),
											writeln(Lnodos2),
											writeln('Lista de Aristas. '),
											writeln(Laristas1).

bienEtiquetado(nodo(X,Lista)) :- writeln('Caso recursivo bienEtiquetado'),
									!, Lnodos = [], Laristas = [],
									bienEtiquetado_Aristas(Lista,Laristas,Lnodos,X),
									writeln('Lista Final de Nodos'),writeln(Lnodos), 
									writeln('Lista Final de Aristas'),writeln(Laristas).
											%bienEtiquetado_Nodos(X,Z,lnodos,laristas,Lista).
% A LOS PREICADOS AUXILIARES QUE ESTAN ABAJO HAY QUE PASARLES NODO Y ARISTA COMO A LOS NORMALES
% JUNTO CON LAS LISTAS PARA QUE SIRVA
bienEtiquetado_Nodos([],Lnodos1,Lnodos1).

bienEtiquetado_Nodos(nodo(X,[]),Lnodos,Lnodos1) :- writeln('Caso base bienEtiquetado_Nodos'),
													!,not(member(X,Lnodos)),
													append([X],Lnodos,Lnodos2),
													writeln('Esta es Lnodos'),
													writeln(Lnodos2),
													bienEtiquetado_Nodos([],Lnodos2,Lnodos1).

bienEtiquetado_Nodos(nodo(X,[Head|Tail]),Lnodos,Laristas) :- writeln('Caso recursivo bienEtiquetado_Nodos'),
															!,integer(X)>0,not(member(X,Lnodos)),
															append([X],Lnodos,Lnodos1),
															writeln('VOY A LLAMAR A ARISTA HEAD Y ARISTA TAIL'),
															bienEtiquetado_Aristas(Head,Laristas,Lnodos1,X),
															bienEtiquetado_Aristas(Tail,Laristas,Lnodos1,X),
															writeln('Aristas'),
															writeln(Laristas),
															writeln(Lnodos1).

%bienEtiquetado_Nodos([Head|Tail],Laristas,Lnodos,W) :- writeln('Caso recursivo auxiliar bienEtiquetado_Aristas '), 
%														bienEtiquetado_Aristas(Head,Laristas,Lnodos,W),
%														bienEtiquetado_Aristas(Tail,Laristas,Lnodos,W).


bienEtiquetado_Aristas([],Laristas,Laristas,X).

bienEtiquetado_Aristas(arista(X,nodo(Y,Hijos)),Laristas,Lnodos,W) :- !, writeln('Caso base recursivo bienEtiquetado_Aristas '), 
											not(W = Y),
											P is abs(W-Y),
											not(X =:= P) -> (writeln('Arbol no esta bien etiquetado: Etiqueta de arista es distinta a diferencia entre padre e hijo.'), false);
											P is abs(W-Y),
											writeln('Etiqueta padre: W - Etiqueta arista: X - Etiqueta hijo: Y - Diferencia: P - Lnodos - Laristas'),
											write(W),write(' '), write(X),write(' '), write(Y),write(' '), 
											write(P),write(' '), write(Lnodos),write(' '), write(Laristas),
											not(member(X,Laristas)),
											append([X],Laristas,Laristas1),writeln(Hijos), 
											bienEtiquetado_Nodos(nodo(Y,Hijos),Lnodos,Lnodos).
											%bienEtiquetado_Aristas([],Laristas,Laristas).

bienEtiquetado_Aristas([Head|Tail],Laristas,Lnodos,W) :- writeln('Caso recursivo auxiliar bienEtiquetado_Aristas '), 
														bienEtiquetado_Aristas(Head,Laristas,Lnodos,W),
														bienEtiquetado_Aristas(Tail,Laristas,Lnodos,W).


% bienEtiquetado(nodo(V,[arista(E,nodo(W,[]))])) :- append([W], ListaNodos, ListaNodos)