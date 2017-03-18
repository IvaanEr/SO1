-module(game).
-compile(export_all).


help(N) ->
	% A = "CON 'id', logearse como id\n",
	B = "LSG, muestra todas las partidas~n",
	C = "NEW [juegoid], comienza un nuevo juego como juegoid~n",
	D = "ACC [juegoid], accede a jugar al juego juegoid~n",
	E = "OBS [juegoid], accede a observar el juego juegoid~n",
	F = "PLA [juegoid] [X], realiza la jugada para juegoid a la casilla X~n    X puede ser un numero entre 1 y 9 o BYE para abandonar~n",
	G = "LEA [juegoid], deja de observar juegoid~n",
	H = "BYE, desconectarse.~n",
	I = "%| 1 | 2 | 3 |%~n%| 4 | 5 | 6 |%~n%| 7 | 8 | 9 |%~n",
	J = "%%===========%%~n",
	K = "   ->Utilizar 'X' de acuerdo al siguiente diagrama~n",
	Tablero = J++I++J,
	global:send(N,{print,B++C++D++E++G++H++F++K++Tablero}).	

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
tprint(D) -> A = "%%===========%%~n",
						 B = "%| "++find(1,D)++" | "++find(2,D)++" | "++find(3,D)++" |%~n%| "++find(4,D)++" | "++find(5,D)++" | "++find(6,D)++" |%~n%| "++find(7,D)++" | "++find(8,D)++" | "++find(9,D)++" |%~n",
						 A++B++A++"~n~n".

%% Envia el juego actualizado a los jugadores y a los observadores.
upd(Juego,Jugadores,Tablero,Observadores,Turno) ->
	presentacion_j(Juego,Jugadores,Tablero,Turno),
	presentacion_o(Juego,Jugadores,Tablero,Observadores,Turno).

%Para imprimir el tablero con su JuegoID|J1|J2.
presentacion_j(G,J,T,Turno) ->
	Aux = "+ "++atom_to_list(G)++" | "++atom_to_list(element(2,lists:nth(1,J)))++" (X) | "++atom_to_list(element(2,lists:nth(2,J)))++" (0) +~n~n",
	JAUX = es_turno(Turno,J),
	% io:format("Pruebaaaa: "++atom_to_list(JAUX)++"~n"),
	lists:foreach(fun(X) -> global:send(element(2,X),{print,Aux}), global:send(element(2,X),{print,"Turno: "++atom_to_list(JAUX)++"\n"}),global:send(element(2,X),{print,tprint(T)}) end,J).

presentacion_o(G,J,T,O,Turno) ->
	Aux = "+ "++atom_to_list(G)++" | "++atom_to_list(element(2,lists:nth(1,J)))++" (X) | "++atom_to_list(element(2,lists:nth(2,J)))++" (0) +\n\n",
	JAUX = es_turno(Turno,J),
	% io:format("Pruebaaaa: "++atom_to_list(JAUX)++"~n"),
	lists:foreach(fun(X) -> global:send(X,{print,Aux}),global:send(X,{print,"Turno: "++atom_to_list(JAUX)++"\n"}), global:send(X,{print,tprint(T)}) end,O).


%Actualizo la casilla C del tablero T, dependiendo de quien es el turno.										
update(C,T,Turno) ->
	S = if Turno == 1 -> "X";
				 Turno == 2 -> "0"
			end,
	dict:store(C,S,T).

%Verifico si  alguien gano.
gano(T) ->
	(vertical(T) or horizontal(T)) or diagonal(T).

vertical(T) ->
	(((find(1,T) == find(4,T)) and (find(4,T) == find(7,T))) and (find(1,T) /= " ")) or
	(((find(2,T) == find(5,T)) and (find(5,T) == find(8,T))) and (find(2,T) /= " ")) or
	(((find(3,T) == find(6,T)) and (find(6,T) == find(9,T))) and (find(3,T) /= " ")).

horizontal(T) ->
	(((find(1,T) == find(2,T)) and (find(2,T) == find(3,T))) and (find(1,T) /= " ")) or
	(((find(4,T) == find(5,T)) and (find(5,T) == find(6,T))) and (find(4,T) /= " ")) or
	(((find(7,T) == find(8,T)) and (find(8,T) == find(9,T))) and (find(7,T) /= " ")).

diagonal(T) ->
	(((find(1,T) == find(5,T)) and (find(5,T) == find(9,T))) and (find(1,T) /= " ")) or
	(((find(7,T) == find(5,T)) and (find(5,T) == find(3,T))) and (find(7,T) /= " ")).

send_msj_obs(L,Data) ->
	lists:foreach(fun (X) -> global:send(X,{print,Data}) end ,L).

send_msj_j1(Jugadores,Data) ->
	case element(1,lists:nth(1,Jugadores)) == 1 of
		true -> J1 = element(2,lists:nth(1,Jugadores)),
						global:send(J1,{print,Data});
		false -> J1 = element(2,lists:nth(2,Jugadores)),
						 global:send(J1,{print,Data})
	end.

send_msj_j2(Jugadores,Data) ->
	case element(1,lists:nth(1,Jugadores)) == 2 of
		true -> J2 = element(2,lists:nth(1,Jugadores)),
						global:send(J2,{print,Data});
		false -> J2 = element(2,lists:nth(2,Jugadores)),
						 global:send(J2,{print,Data})
	end.


ganoJ1(Jugadores,Observadores) ->
	J1 = atom_to_list(element(2,hd(Jugadores))),
	send_msj_j1(Jugadores,"Ganaste!~n"),
	send_msj_j2(Jugadores,"Perdiste.~n"),
	send_msj_obs(Observadores,"Gano el jugador "++J1++" (X)~n").
	
ganoJ2(Jugadores,Observadores) ->
	J2 = atom_to_list(element(2,lists:nth(2,Jugadores))),
	send_msj_j2(Jugadores,"¡Ganaste!~n"),
	send_msj_j1(Jugadores,"Perdiste.~n"),
	send_msj_obs(Observadores,"Gano el jugador "++J2++" (0)~n").

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
	
empate(T) ->
	A =  (find(1,T) /= " "),
	B =	 (find(2,T) /= " "),
	C =	 (find(3,T) /= " "),
	D =	 (find(4,T) /= " "),
	F =	 (find(5,T) /= " "),
	G =	 (find(6,T) /= " "),
	H =	 (find(7,T) /= " "),
	I =	 (find(8,T) /= " "),
	J =	 (find(9,T) /= " "),
	if ((((((((A and B) and C) and D) and F) and G) and H) and I) and J) ->
		true;
	true -> false
	end.
