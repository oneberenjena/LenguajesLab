nodo(X,[]) :- !,write('nodo -- caso base '), writeln(X),
				integer(X)>0.

nodo(X, [Head|Tail]) :- write('nodo -- caso recursivo '),writeln(X),
					integer(X)>0,
					Head,
					nl,
					auxiliar(Tail).

nodo(_) :- !, writeln('Error: Nodo no cumple con la sintaxis requerida.'),
			false.

arista(X,Hijo) :- write('arista -- unico caso '),writeln(X),
					integer(X)>0,
					Hijo.

arista(_) :- !, writeln('Error: Arista no cumple con la sintaxis requerida.'),
				false.

% Predicado utilizado para separar head y tail de un arreglo.
% Sirve para ejecutar cada arista de un nodo, tiene caso base y recursivo.
auxiliar([]) :- true.
auxiliar([Head|Tail]) :- Head,
						auxiliar(Tail).
