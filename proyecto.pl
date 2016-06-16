% Laboratorio de Lenguajes de Programacion I
% Proyecto 2
% Archivo: proyecto.pl 
% Descripcion: Contiene los predicados del proyecto.
% Ultima modificacion: 16/06/16
% Autores: - Benjamin Amos    #12-10240
%		   - Douglas Torres   #11-11027
%		   - Roberto Camara   #11-10235

% NOTA: ESTE PROYECTO FUE ESCRITO USANDO PREDICADOS BUILT-IN DE SWI-PROLOG.

% Estructura de Nodo que es hoja, y sirve para la impresion del arbol.
nodo(X,[]) :- !,integer(X)>0,
				write('Etiqueta nodo: '), writeln(X),
				writeln('Fin de rama').
				
% Estructura de Nodo recursivo, que sirve para la impresion del arbol.
nodo(X, [Head|Tail]) :- !,integer(X)>0,
						write('Etiqueta nodo: '),writeln(X),
						Head,
						auxiliar(Tail),
						nl,
						writeln('Regreso al nivel anterior a imprimir los hermanos.')
						,nl.

% Estructura de Nodo que sirve para capturar errores en la sintaxis de un nodo.
nodo(_) :- !, writeln('Error: Nodo no cumple con la sintaxis requerida.'),
			false.

% Estructura de Arista que actua de forma recursiva, y sirve para la impresion del arbol.
arista(X,Hijo) :- !,integer(X)>0,
					writeln('|'),
					write('Etiqueta arista: '), writeln(X),
					writeln('|'),
					Hijo.

% Estructura de Arista que sirve para capturar errores en la sintaxis de una arista.
arista(_) :- !, writeln('Error: Arista no cumple con la sintaxis requerida.'),
				false.

% Predicado utilizado para separar head y tail de un arreglo.
% Sirve para ejecutar cada arista de un nodo, tiene caso base y recursivo.
auxiliar([]) :- true.
auxiliar([Head|Tail]) :- !,Head,
						auxiliar(Tail).

% Predicado bienEtiquetado para un caso base.
bienEtiquetado(nodo(X,[])) :- (integer(X)=<0 -> writeln('Arbol no esta bien etiquetado: Etiqueta de nodo es menor o igual a 0.'), false);
								!,integer(X)>0,
								Lnodos = [],
								Laristas = [],
								append([X],Lnodos,Lnodos1),
								writeln('Lista de nodos en el arbol'),
								writeln(Lnodos1).

% Predicado bienEtiquetado para un caso base.
bienEtiquetado(nodo(X,[arista(Y,nodo(Z,[]))])) :- !, not(X = Z),
												P is abs(X-Z),
												not(Y =:= P) -> (writeln('Arbol no esta bien etiquetado: Etiqueta de arista es 	distinta a diferencia entre padre e hijo.'), false);	
												Lnodos = [], Laristas = [],	
												append([X],Lnodos,Lnodos1),	
												append([Z],Lnodos1,Lnodos2),
												append([Y],Laristas,Laristas1),
												sort(Lnodos2,LnodosO),
												sort(Laristas1,LaristasO),
												writeln('Lista de nodos en el arbol'),
												writeln(LnodosO),
												writeln('Lista de aristas en el arbol'),
												writeln(LaristasO).

% Predicado bienEtiquetado para el caso recursivo.
bienEtiquetado(nodo(X,Lista)) :- !, Lnodos = [], Laristas = [],
									bienEtiquetado_Nodos(nodo(X,Lista),Laristas,Lnodos,LaristasF,LnodosF),
									max_list(LnodosF,N),
									max_list(LaristasF,E),
									Maxarista is N-1,
									(E =:= Maxarista),
									sort(LnodosF,LnodosO),
									sort(LaristasF,LaristasO),
									setof(Z,between(1,N,Z),L),
									setof(W,between(1,E,W),A),
									compare(=,LnodosO,L),
									compare(=,LaristasO,A),
									writeln('Lista de nodos en el arbol'),
									writeln(LnodosO),
									writeln('Lista de aristas en el arbol'),
									writeln(LaristasO).

% Predicado para unificacion de resultado de lista de nodos a devolver.
bienEtiquetado_Nodos([],Lnodos,Lnodos).

% Predicado auxiliar para la verificacion del buen etiquetamiento de nodos.
bienEtiquetado_Nodos(nodo(X,[]),Laristas,Lnodos,LaristasR,LnodosR) :- writeln('Caso base bienEtiquetado_Nodos'),
													writeln(Lnodos),
													!,not(member(X,Lnodos)),
													append([X],Lnodos,Lnodos1),
													bienEtiquetado_Aristas([],Laristas,LaristasR),
													bienEtiquetado_Nodos([],Lnodos1,LnodosR).

% Predicado auxiliar para la verificacion del buen etiquetamiento de nodos.
bienEtiquetado_Nodos(nodo(X,[Head|Tail]),Laristas,Lnodos,LaristasR,LnodosR) :- !,integer(X)>0,not(member(X,Lnodos)),
															append([X],Lnodos,Lnodos1),
															bienEtiquetado_Aristas(Head,Laristas,Lnodos1,X,Laristas1,Lnodos2),
															bienEtiquetado_Aristas(Tail,Laristas1,Lnodos2,X,LaristasR,LnodosR).

% Predicado para unificacion de resultado de la lista de aristas a devolver.
bienEtiquetado_Aristas([],Laristas,Laristas).

% Predicado auxiliar para la verificacion del buen etiquetamiento de aristas.
bienEtiquetado_Aristas([],Laristas,Lnodos,X,LaristasF,LnodosF) :- bienEtiquetado_Aristas([],Laristas,LaristasF),
																	bienEtiquetado_Nodos([],Lnodos,LnodosF).

% Predicado auxiliar para la verificacion del buen etiquetamiento de aristas.
bienEtiquetado_Aristas(arista(X,nodo(Y,Hijos)),Laristas,Lnodos,W,LaristasR,LnodosR) :- !, writeln('Caso base recursivo bienEtiquetado_Aristas '), 
											not(W = Y),
											P is abs(W-Y),
											not(X =:= P) -> (writeln('Arbol no esta bien etiquetado: Etiqueta de arista es distinta a diferencia entre padre e hijo.'), false);
											not(member(X,Laristas)),
											append([X],Laristas,Laristas1), writeln(Laristas1),writeln(Lnodos), writeln(Hijos),
											bienEtiquetado_Nodos(nodo(Y,Hijos),Laristas1,Lnodos,LaristasR,LnodosR).

% Predicado auxiliar para la verificacion del buen etiquetamiento de aristas.
bienEtiquetado_Aristas([Head|Tail],Laristas,Lnodos,W,LaristasF,LnodosF) :- writeln('Caso recursivo auxiliar bienEtiquetado_Aristas'),
														bienEtiquetado_Aristas(Head,Laristas,Lnodos,W,Laristas2,Lnodos2),
														bienEtiquetado_Aristas(Tail,Laristas2,Lnodos2,W,LaristasF,LnodosF).

% Predicado de esqueleto que devuelve un solo esqueleto valido.
esqueleto(N,R,Esq) :- !, (R>=N -> writeln('R es mayor o igual a N.'),false);
						Esque = [],
						NX is N-1,
						Contador = 0,
						esqueleto_auxiliar(NX,R,Contador,Esque,Esq).

% Predicado auxiliar de esqueleto que sirve para agregar los distintos niveles.
esqueleto_auxiliar(0,R,Contador,EsqI,Esq) :- !, Lista = [],
												Diff is R-Contador,
												integer(Diff)>=0,
												append(Lista,[Contador],Esq1),
												append(EsquI,[Esq1],EsqO),
												RX is R-Contador,
												Hijos = [],
												esqueleto_hijos(0,RX,Contador,EsqO,Hijos,Esq).

% Predicado auxiliar de esqueleto que sirve para decrementar los hijos a colocar.
esqueleto_auxiliar(N,R,Contador,EsqI,Esq) :- !,P is N-1,
												Contador1 is Contador+1,
												esqueleto_auxiliar(P,R,Contador1,EsqI,Esq).

% Predicado donde se colocan los hijos del nodo previo
esqueleto_hijos(0,R,0,EsqI,Hijos,EsqO) :- !,append(EsqI,[Hijos],EsqO).

% Predicado que sirve para agregar los hijos del nodo al esqueleto.
esqueleto_hijos(0,R,Contador,EsqI,Hijos,Esq):- 	!, append(Hijos,[R],Aux),
											Contador1 is Contador-1,
											esqueleto_hijos(0,R,Contador1,EsqI,Aux,Esq).

% Predicado que sirve para imprimir el arbol dado.
describirEtiquetamiento(nodo(X,[Head|Tail])) :- !,integer(X)>0,
												write('Nodo Raiz '),
												write('etiqueta: '),writeln(X),
												Head,
												%nl,
												(Tail \= []) -> (write('Nodo Raiz '),write('etiqueta: '),write(X),nl,auxiliar(Tail));
												%auxiliar(Tail),
												writeln('Fin de arbol.').