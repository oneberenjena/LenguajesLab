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
									bienEtiquetado_Nodos(nodo(X,Lista),Laristas,Lnodos,LaristasF,LnodosF),
									writeln('Lista Final de Nodos'),writeln(LnodosF), 
									writeln('Lista Final de Aristas'),writeln(LaristasF),
									max_list(LnodosF,N),
									max_list(LaristasF,E),
									Maxarista is N-1,
									(E =:= Maxarista),
									sort(LnodosF,LnodosO),
									sort(LaristasF,LaristasO),
									setof(Z,between(1,N,Z),L),
									setof(W,between(1,E,W),A),
									compare(=,LnodosO,L),
									compare(=,LaristasO,A).
									%(LnodosF =:= L).
									%writeln('LnodosF'), writeln(A).
											%bienEtiquetado_Nodos(X,Z,lnodos,laristas,Lista).
% A LOS PREICADOS AUXILIARES QUE ESTAN ABAJO HAY QUE PASARLES NODO Y ARISTA COMO A LOS NORMALES
% JUNTO CON LAS LISTAS PARA QUE SIRVA
bienEtiquetado_Nodos([],Lnodos,Lnodos).

bienEtiquetado_Nodos(nodo(X,[]),Laristas,Lnodos,LaristasR,LnodosR) :- writeln('Caso base bienEtiquetado_Nodos'),
													writeln(Lnodos),
													!,not(member(X,Lnodos)),
													append([X],Lnodos,Lnodos1),
													writeln('Esta es Lnodos'),
													writeln(Lnodos1),
													writeln(Laristas),
													bienEtiquetado_Aristas([],Laristas,LaristasR),
													bienEtiquetado_Nodos([],Lnodos1,LnodosR),
													writeln('ULTIMA LINEA DE bienEtiquetado_Nodos/5 '),
													writeln(LaristasR),
													writeln(LnodosR).

bienEtiquetado_Nodos(nodo(X,[Head|Tail]),Laristas,Lnodos,LaristasR,LnodosR) :- writeln('Caso recursivo bienEtiquetado_Nodos'),
															!,integer(X)>0,not(member(X,Lnodos)),
															append([X],Lnodos,Lnodos1),
															writeln('VOY A LLAMAR A ARISTA HEAD Y ARISTA TAIL'),
															bienEtiquetado_Aristas(Head,Laristas,Lnodos1,X,Laristas1,Lnodos2),
															bienEtiquetado_Aristas(Tail,Laristas1,Lnodos2,X,LaristasR,LnodosR),
															writeln('VOY A VER MIS RESULTADOS'),
															writeln(LaristasR),
															writeln(LnodosR).
															%bienEtiquetado_Aristas([],LaristasR,LaristasQ),
															%bienEtiquetado_Nodos([],LnodosR,LnodosQ),
															%writeln('Aristas'),
															%writeln(LaristasQ),
															%writeln('Nodos'),
															%writeln(LnodosQ).

%ESTABA MODFICANDO bienEtiquetado PARA QUE LLAME a bienEtiquetado_Nodos Y DE ALLI PASAR A ARISTA, ME FALTA ES VERIFICAR QUE NO TENGA QUE CAMBIAR MAS NADA

bienEtiquetado_Aristas([],Laristas,Laristas).

bienEtiquetado_Aristas([],Laristas,Lnodos,X,LaristasF,LnodosF) :- bienEtiquetado_Aristas([],Laristas,LaristasF),
																	bienEtiquetado_Nodos([],Lnodos,LnodosF).


bienEtiquetado_Aristas(arista(X,nodo(Y,Hijos)),Laristas,Lnodos,W,LaristasR,LnodosR) :- !, writeln('Caso base recursivo bienEtiquetado_Aristas '), 
											not(W = Y),
											P is abs(W-Y),
											not(X =:= P) -> (writeln('Arbol no esta bien etiquetado: Etiqueta de arista es distinta a diferencia entre padre e hijo.'), false);
											P is abs(W-Y),
											writeln('Etiqueta padre: W - Etiqueta arista: X - Etiqueta hijo: Y - Diferencia: P - Lnodos - Laristas'),
											write(W),write(' '), write(X),write(' '), write(Y),write(' '), 
											write(P),write(' '), write(Lnodos),write(' '), write(Laristas),nl,
											not(member(X,Laristas)),
											append([X],Laristas,Laristas1), writeln(Laristas1),writeln(Lnodos), writeln(Hijos),
											bienEtiquetado_Nodos(nodo(Y,Hijos),Laristas1,Lnodos,LaristasR,LnodosR),
											writeln('ULTIMA LINEA DE bienEtiquetado_Aristas/6-1 '),
											writeln(LaristasR),
											writeln(LnodosR).
											%bienEtiquetado_Aristas([],Laristas,Laristas).

bienEtiquetado_Aristas([Head|Tail],Laristas,Lnodos,W,LaristasF,LnodosF) :- writeln('Caso recursivo auxiliar bienEtiquetado_Aristas'),
														bienEtiquetado_Aristas(Head,Laristas,Lnodos,W,Laristas2,Lnodos2),
														writeln('Falle'), writeln(Tail),
														writeln(Laristas2), writeln(Lnodos2), writeln(W),
														bienEtiquetado_Aristas(Tail,Laristas2,Lnodos2,W,LaristasF,LnodosF),
														writeln('ULTIMA LINEA DE bienEtiquetado_Aristas/6-2 '),
														writeln(LaristasF),
														writeln(LnodosF).
														%bienEtiquetado_Aristas([],Laristas,LaristasR),
														%bienEtiquetado_Nodos([],Lnodos1,LnodosR).


% bienEtiquetado(nodo(V,[arista(E,nodo(W,[]))])) :- append([W], ListaNodos, ListaNodos)