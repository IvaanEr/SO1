-module(server).
-compile(export_all).


start(SName,Port) ->
	net_kernel:start([SName,shortnames]),
	spawn(?MODULE,server,[Port]).

server(Port) ->
	Resolve = fun (Name,Pid1,Pid2) -> Pid1 end,
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
	ClientPid = spawn(?MODULE,list_of_client,[[]]), %%creo el proceso que maneja la lista para los clientes
	yes = global:register_name(clients_pid,ClientPid,Resolve),
	GamesPid = spawn(?MODULE,lists_of_games,[[]]), %%crea el proceso que maneja la lista de juegos en curso.
	yes = global:register_name(games_pid,GamesPid,Resolve),
	PidBalance = spawn(?MODULE,pbalance,[statistics(total_active_tasks),node()]),
	register(pbalance,PidBalance),
	PidStat = spawn(?MODULE,pstat,[]),
	register(pstat,PidStat),
	% io:format("Registrados? ~p and ~p ~n",[lists:member(pbalance,registered()),lists:member(pstat,registered())]),
	dispatcher(LSock). %%llamo a dispatcher con el listen socket

%% Espera nuevas conexiones y crea un proceso para atender cada una.
dispatcher(LSock) ->
	{ok, CSock} = gen_tcp:accept(LSock), %%acepta la conexion del cliente 
		Pid = spawn(?MODULE, psocket, [CSock]),
		%io:format("Todo bien hasta aca ~p ~n",[Pid]),
		ok = gen_tcp:controlling_process(CSock, Pid), %%Ahora a CSock lo controla Pid -- los mensajes a CSock llegan a Pid
		Pid ! ok,
		dispatcher(LSock).
	
%Atendera todos los pedidos del cliente hablando en CSock.
psocket(CSock) ->
	receive ok -> ok end, %%magia para que controlling process suceda antes que setopts.
		%io:format("Seguimos~n"),
		ok = inet:setopts(CSock	,[{active,true}]), %%recivo msjs con receive comun.	
	gen_tcp:send(CSock,"Bienvenido!\n Recuerda primero debes registrarte con CON\n"),
	psocket_loop(CSock).

%%Primer PSocket: Solo para registrarse.
psocket_loop(CSock) ->
	pbalance ! {req,self()},
	receive	
		{send,Nodo} ->	%io:format("Nodo1:~p~n",[Nodo]),
										receive
											{tcp,CSock,Data} ->
												case string:tokens(lists:sublist(Data, length(Data)-2)," ") of
													["CON",Nombre] -> 
																spawn(Nodo,?MODULE,connect,[Nombre,CSock,self()]),
																receive {con,N} -> io:format("registro exitoso~n"),psocket_loop(CSock,N);
																				{error} -> psocket_loop(CSock)
																end;
													["BYE"] -> ok;
													Otherwise -> gen_tcp:send(CSock,"Primero debes registrarte con CON\n"),
																 io:format("Pid: ~p quiere ejecutar comandos sin registrarse~n",[self()]),
																 psocket_loop(CSock)
												end;
											{error,Closed} -> io:format("Closed:~p~n",[Closed]) %%hace falta volver a llamar a psocketloop?
										end
	end.

%%Segundo PSocket: Una vez registrado N tiene acceso a los demas comandos.
psocket_loop(CSock,N) ->
	{pbalance,node()} ! {req,self()},
	receive
		{send,Nodo} ->	%io:format("Nodo2:~p~n",[Nodo]),
										receive 
											{tcp,CSock,Data} ->
												spawn(Nodo,?MODULE, pcommand, [Data,CSock,N]),
												psocket_loop(CSock,N);
											{print,Data} -> gen_tcp:send(CSock,Data),psocket_loop(CSock,N);
											{error,Closed} -> io:format("Closed:~p~n",[Closed])
										end
		
	end.	

pcommand(Data,CSock,N) ->
	case string:tokens(lists:sublist(Data, length(Data)-2)," ") of
		["CON",Nombre] -> gen_tcp:send(CSock,"Ya estas registrado\n");
		["LSG"] -> lsg(CSock);
		["NEW", NJuego] -> newgame(NJuego,CSock,N);
		["ACC", Juego] -> acc(Juego,CSock,N);
		["PLA", NJuego,Casilla] -> jugada(NJuego,Casilla,N);
		["OBS", Cmdid,Juegoid] -> ok;
		["LEA", Cmdid, Juegoid] -> ok;
		["BYE"] -> ok;
		Otherwise -> error
	end.


%Encargado de mandar la info de carga al resto de los nodos
pstat() -> 
	Carga = statistics(total_active_tasks),

	{pbalance,node()} ! {st,Carga,node()},
	lists:foreach(fun (X) -> {pbalance,X}!{st,Carga,X} end,nodes()),

	receive after 20000 -> ok end,
	pstat().

%Recibe lo de pstat y calcula en que nodo se debe ejecutar.
pbalance(Carga,Nodo) -> 
	receive
		{st,C,N} -> case C > Carga of
						true -> pbalance(C,N);
						false -> pbalance(Carga,Nodo)
					end;
		{req,Pid} -> Pid!{send,Nodo}, pbalance(Carga,Nodo) 
	end.

%Comando CON. Registra un usuario con Nombre.	
connect(Nombre, CSock, PSocketPid) ->
		%io:format("Connect, antes del case~p~n",[DiccPid]),
		N = list_to_atom(Nombre),
		io:format("is ~p an atom? ~p ~n",[N,is_atom(N)]),
		io:format("is ~p a pid? ~p ~n",[PSocketPid,is_pid(PSocketPid)]),

		case global:register_name(N,PSocketPid) of
			 yes -> global:send(clients_pid,{new_client,N}),
					    gen_tcp:send(CSock, "OK CON "++Nombre++"\n"),
					 		io:format("OK CON~n"),
					 		PSocketPid ! {con,N};
				no -> io:format("ERROR CON "++Nombre++"\n"),
				gen_tcp:send(CSock, "Ya estas registrado o el nombre está en uso\n"),
				PSocketPid ! {error}
	end,

	%Solo para ver que anduvo bien.
	global:send(clients_pid,{req,self()}),
	receive
		{send,L} -> io:format("Clientes: ~p~n",[L])
	end.

%% Lista de los clientes conectados
list_of_client(L) ->
		receive
				{new_client,Client} -> 
						list_of_client(L++[Client]);
				{req,Pid} -> Pid ! {send,L},
							 list_of_client(L)
		end.

%% Lista de todos los juegos en curso.
lists_of_games(L) ->
	receive
		{new,Juego} -> lists_of_games(L++[Juego]);
		{req,Pid} -> Pid ! {send,L},
								 lists_of_games(L);
		{elim,Juego} -> lists_of_games(lists:delete(Juego,L))
	end.
	%%io:format("lists_of_games = ~p || Juego ~p ~n",[L,Juego]).

%Comando NEW. N crea el juego NJuego.
newgame(NJuego,CSock,N) ->
	J = list_to_atom(NJuego),
	Game = spawn(?MODULE,game_init,[N,J]),
	case global:register_name(J,Game) of
		yes -> gen_tcp:send(CSock,"OK NEW\n"),
				 global:send(games_pid,{new,J});
				 % global:send(games_pid,{req,self()});
				 % %No hace falta. 
				 % receive
				 % 	   {send,L} -> io:format("Games: ~p~n",[L])
				 % end;
		no -> gen_tcp:send(CSock,"ERROR Nombre de juego\n")
	end.
	
%Comando LSG. Para cada juego, su ID y participantes.
lsg(CSock) ->
	gen_tcp:send(CSock,"Lista de juegos:\n\n"),
	global:send(games_pid,{req,self()}),

	Imp_datos = fun (L) -> 	lists:foreach(fun (X) -> gen_tcp:send(CSock,atom_to_list(X)++" | ") end,L),
							 						gen_tcp:send(CSock,"\n") end,
	Obt_datos = fun (Game) -> global:send(Game,{datos,self()}),
														receive
														{send,Jugadores,Observadores} -> gen_tcp:send(CSock,"Jugadores:\n"),
																						 Imp_datos(lists:map(fun(X)-> element(2,X) end,Jugadores)),
																						 gen_tcp:send(CSock,"Observadores: \n"),
																						 Imp_datos(Observadores),
																						 gen_tcp:send(CSock,"_______________\n")
														end
				end,
	receive
		{send,L} -> lists:foreach(fun (X) -> gen_tcp:send(CSock,atom_to_list(X)++"\n"),
											 Obt_datos(X) end, L)
	end.

%Comando ACC. N accede a Juego si es posible.
acc(Juego,CSock,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive
	{send,L} -> case lists:member(J,L) of
				false -> gen_tcp:send(CSock,"ERROR Juego inexistente\n");
				true  -> global:send(J,{datos,self()}),
						 receive {send,Jugadores,Observadores} ->
							if length(Jugadores) == 2 -> gen_tcp:send(CSock,"Juego en curso\n");
								 true -> global:send(J,{update,Jugadores++[{2,N}],Observadores}),
											 	 gen_tcp:send(CSock,"OK ACC\n"),
											 	 global:send(J,{start})
							end
						 end
				end
	end.

%%Inicializa el juego. Crea el tablero y parametros iniciales.
game_init(N,J) ->
	Tablero = inicializar_tablero(),
	Jugadores = [{1,N}],
	Observadores = [],
	Turno = 1,
	game(J,Tablero,Jugadores,Observadores,Turno).

%% Maneja el juego J.
game(J,Tablero,Jugadores,Observadores,Turno) ->
	receive
		{datos,Pid} -> Pid ! {send,Jugadores,Observadores}, game(J,Tablero,Jugadores,Observadores,Turno);
		{datos2,Pid} -> Pid ! {send,Tablero,Jugadores,Observadores,Turno},game(J,Tablero,Jugadores,Observadores,Turno);
		{update,Jugadores_new,Observadores_new} -> game(J,Tablero,Jugadores_new,Observadores_new,Turno);
		{update2,TNew,TurnoNew} -> presentacion(J,Jugadores,TNew),
															 io:format("~p ~p~n",[gano(TNew,"X"),gano(TNew,"0")]),
															 case gano(TNew,"X") of
															 	true -> ganoJ1(Jugadores),global:send(games_pid,{elim,J}),exit(normal);
															 	false -> case gano(TNew,"0") of
															 						true -> ganoJ2(Jugadores),global:send(games_pid,{elim,J}),exit(normal);
															 						false -> game(J,TNew,Jugadores,Observadores,TurnoNew)
															 					 end
															 end; 
		{start} ->io:format("El jugador es: ~p~n",[es_turno(Turno,Jugadores)]),
							global:send(es_turno(Turno,Jugadores),{print,"-- Es tu turno --"++integer_to_list(Turno)++"\n"}),
							presentacion(J,Jugadores,Tablero),
							game(J,Tablero,Jugadores,Observadores,Turno)
	end.

%%Actualiza el turno.
turno(T) ->
	if T == 1 -> 2;
		 T == 2 -> 1
	end.

%% Dado un turno y la lista de Jugadores de la forma [{1,J1},{2,J2}], devuelve J1 o J2
%% dependiendo de quien es el turno.
es_turno(Turno,Jugadores) ->
	A = element(1,lists:nth(1,Jugadores)) == Turno,
	B = element(1,lists:nth(2,Jugadores)) == Turno,
	if A -> element(2,lists:nth(1,Jugadores));
		 true ->	if B -> element(2,lists:nth(2,Jugadores));
		 						 true -> ok
		 					end
	end.

%%Comando PLA. Actualiza, si esta permitido, la Casilla del juego NJuego.	
jugada(NJuego,Casilla,N) ->
	C = list_to_integer(Casilla),
	J = list_to_atom(NJuego),
	global:send(J,{datos2,self()}),
	receive
		{send,T,Jugadores,Obs,Turno} ->
			case length(Jugadores) /= 2 of 
				 true -> global:send(N,{print,"ERROR PLA (Jugadores)\n"});
				 false -> A = element(2,lists:nth(1,Jugadores)), B = element(2,lists:nth(2,Jugadores)),
				 				 case lists:member(N,[A,B]) and (es_turno(Turno,Jugadores) == N) of 
				 	          	true -> %io:format("C:~p T: ~p A: ~p~n",[Casilla,find(Casilla,T)," " == find(Casilla,T)]),
				 	          					case find(C,T) == " " of 
				 	          						true -> TNew = update(C,T,Turno),
																				TurnoNew = turno(Turno),
																				global:send(J,{update2,TNew,TurnoNew});
										 						false -> global:send(N,{print,"ERROR PLA (Casilla)\n"})
															end;
											false -> global:send(N,{print,"ERROR PLA (Jugador/Turno)\n"})
						     end
			end
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
tprint(D) -> %A = "+______________+\n\n",
						 B = "| "++find(1,D)++" | "++find(2,D)++" | "++find(3,D)++" |\n| "++find(4,D)++" | "++find(5,D)++" | "++find(6,D)++" |\n| "++find(7,D)++" | "++find(8,D)++" | "++find(9,D)++" |\n",
						 B.

%Para imprimir el tablero con su JuegoID|J1|J2.
presentacion(G,J,T) ->
	Aux = "+ "++atom_to_list(G)++" | "++atom_to_list(element(2,lists:nth(1,J)))++" | "++atom_to_list(element(2,lists:nth(2,J)))++" +\n\n",
	lists:foreach(fun(X) -> global:send(element(2,X),{print,Aux}),global:send(element(2,X),{print,tprint(T)}) end,J).

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
	P = (find(L1,T) == find(L2,T)) and (find(L3,T) == S),
	io:format(find(L1,T)++find(L2,T)++find(L3,T)++S++"P: ~p \n---------\n",[P]),
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

%Caracter N del tablero D.
find(N,D) ->
	case dict:find(N,D) of
		{ok,Result} -> Result;
		error       -> '-'
	end.

