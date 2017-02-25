-module(game).
-compile(export_all).


mod() ->
	io:format("~p~n~n~n~p",[module_info(exports),module_info(functions)]).
%Caracter N del tablero D.
find(N,D) ->
	case dict:find(N,D) of
		{ok,Result} -> Result;
		error       -> '-'
	end.

%% Devuelve el tablero con todos los espacios en blanco.
inicializar_tablero() ->
	T = dict:new(),
	T1 = dict:store(1," ",T),
		T2 = dict:store(2," ",T1),
		T3 = dict:store(3," ",T2),
		T4 = dict:store(4," ",T3),
		T5 = dict:store(5," ",T4),
		T6 = dict:store(6," ",T5),
		T7 = dict:store(7," ",T6),
		T8 = dict:store(8," ",T7),
		T9 = dict:store(9," ",T8),
		T9.

%Cadena para tener el tablero.
tprint(D) -> A = "%%===========%%\n",
						 B = "%| "++find(1,D)++" | "++find(2,D)++" | "++find(3,D)++" |%\n%| "++find(4,D)++" | "++find(5,D)++" | "++find(6,D)++" |%\n%| "++find(7,D)++" | "++find(8,D)++" | "++find(9,D)++" |%\n",
						 A++B++A.

%% Envia el juego actualizado a los jugadores y a los observadores.
upd(Juego,Jugadores,Tablero,Observadores) ->
	presentacion_j(Juego,Jugadores,Tablero),
	presentacion_o(Juego,Jugadores,Tablero,Observadores).

%Para imprimir el tablero con su JuegoID|J1|J2.
presentacion_j(G,J,T) ->
	Aux = "+ "++atom_to_list(G)++" | "++atom_to_list(element(2,lists:nth(1,J)))++" | "++atom_to_list(element(2,lists:nth(2,J)))++" +\n\n",
	lists:foreach(fun(X) -> global:send(element(2,X),{print,Aux}),global:send(element(2,X),{print,tprint(T)}) end,J).

presentacion_o(G,J,T,O) ->
	Aux = "+ "++atom_to_list(G)++" | "++atom_to_list(element(2,lists:nth(1,J)))++" | "++atom_to_list(element(2,lists:nth(2,J)))++" +\n\n",
	lists:foreach(fun(X) -> global:send(X,{print,Aux}),global:send(X,{print,tprint(T)}) end,O).

%Actualizo la casilla C del tablero T, dependiendo de quien es el turno.										
update(C,T,Turno) ->
	S = if Turno == 1 -> "X";
				 Turno == 2 -> "0"
			end,
	dict:store(C,S,T).

%Verifico si  alguien gano.
gano(T,S) ->
	Posibles = [[1,2,3],[4,5,6],[7,8,9],[1,4,7],[2,5,8],[3,6,9],[1,5,9],[7,5,3]],
	P = check(lists:nth(1,Posibles),T,S) or
			check(lists:nth(2,Posibles),T,S) or
			check(lists:nth(3,Posibles),T,S) or
			check(lists:nth(4,Posibles),T,S) or
			check(lists:nth(5,Posibles),T,S) or
			check(lists:nth(6,Posibles),T,S) or
			check(lists:nth(7,Posibles),T,S) or
			check(lists:nth(8,Posibles),T,S),
	P.

%Para cada una de las Posibilidades, chequea si es valida para ganar.
check(L,T,S) ->
	L1 = lists:nth(1,L),
	L2 = lists:nth(2,L),
	L3 = lists:nth(3,L),
	P = ((find(L1,T) == S) and (find(L2,T) == S)) and (find(L3,T) == S),
	%io:format(find(L1,T)++find(L2,T)++find(L3,T)++S++"P: ~p \n---------\n",[P]),
	% io:format("Check: ~p~n",[P]),
	P.

ganoJ1(Jugadores) ->
	case element(1,lists:nth(1,Jugadores)) of
		1 -> J1 = element(2,lists:nth(1,Jugadores)),
				 J2 = element(2,lists:nth(2,Jugadores)),
				 global:send(J1,{print,"Has Ganado!\n"}),
				 global:send(J2,{print,"Perdiste.\n"});
		2 -> J1 = element(2,lists:nth(2,Jugadores)),
				 J2 = element(2,lists:nth(1,Jugadores)),
				 global:send(J1,{print,"Has Ganado!\n"}),
				 global:send(J2,{print,"Perdiste.\n"})
	end.

ganoJ2(Jugadores) ->
	case element(1,lists:nth(1,Jugadores)) of
		1 -> J1 = element(2,lists:nth(1,Jugadores)),
				 J2 = element(2,lists:nth(2,Jugadores)),
				 global:send(J2,{print,"Has Ganado!\n"}),
				 global:send(J1,{print,"Perdiste.\n"});
		2 -> J1 = element(2,lists:nth(2,Jugadores)),
				 J2 = element(2,lists:nth(1,Jugadores)),
				 global:send(J2,{print,"Has Ganado!\n"}),
				 global:send(J1,{print,"Perdiste.\n"})
	end.

%%Actualiza el turno.
turno(T) ->
	if T == 1 -> 2;
		 T == 2 -> 1
	end.

%% Dado un turno y la lista de Jugadores de la forma [{1,J1},{2,J2}], devuelve J1 o J2
%% dependiendo de quien es el turno.
%% La lista generalmente va a estar en orden, es decir que Jugadores[1] es J1, pero por si acaso...
es_turno(Turno,Jugadores) ->
	A = element(1,lists:nth(1,Jugadores)) == Turno,
	B = element(1,lists:nth(2,Jugadores)) == Turno,
	if A -> element(2,lists:nth(1,Jugadores));
		 true ->	if B -> element(2,lists:nth(2,Jugadores));
		 						 true -> ok
		 					end
	end.
	