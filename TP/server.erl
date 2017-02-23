-module(server).
-compile(export_all).

start(SName,Port) ->
	net_kernel:start([SName,shortnames]),
	spawn(?MODULE,server,[Port]).

server(Port) ->
	Resolve = fun (Name,Pid1,Pid2) -> Pid1 end,
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
	ClientPid = spawn(?MODULE,list_of_client,[[]]), %%creo el proceso que maneja el diccionario para los clientes
	yes = global:register_name(clients_pid,ClientPid,Resolve),
	GamesPid = spawn(?MODULE,lists_of_games,[[]]), %%crea el proceso que maneja la lista de juegos en curso.
	yes = global:register_name(games_pid,GamesPid,Resolve),
	PidBalance = spawn(?MODULE,pbalance,[statistics(total_active_tasks),node()]),
	register(pbalance,PidBalance),
	PidStat = spawn(?MODULE,pstat,[]),
	register(pstat,PidStat),
	io:format("Registrados? ~p and ~p ~n",[lists:member(pbalance,registered()),lists:member(pstat,registered())]),
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

psocket_loop(CSock) ->
	pbalance ! {req,self()},
	receive	
		{send,Nodo} ->	io:format("Nodo1:~p~n",[Nodo]),
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
	
psocket_loop(CSock,N) ->
	{pbalance,node()} ! {req,self()},
	receive
		{send,Nodo} ->	io:format("Nodo2:~p~n",[Nodo]),
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
		["PLA", Cmdid,Juegoid,Jugada] -> ok;
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

list_of_client(L) ->
		receive
				{new_client,Client} -> 
						list_of_client(lists:append(L,[Client]));
				{req,Pid} -> Pid ! {send,L},
							 list_of_client(L)
		end.

lists_of_games(L) ->
	receive
		{new,Juego} -> lists_of_games(lists:append(L,[Juego]));
		{req,Pid} -> Pid ! {send,L},
								 lists_of_games(L)
	end.
	%%io:format("lists_of_games = ~p || Juego ~p ~n",[L,Juego]).

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
	


lsg(CSock) ->
	gen_tcp:send(CSock,"Lista de juegos:\n\n"),
	global:send(games_pid,{req,self()}),

	Imp_datos = fun (L) -> 	lists:foreach(fun (X) -> gen_tcp:send(CSock,atom_to_list(X)++" | ") end,L),
							 						gen_tcp:send(CSock,"\n") end,
	Obt_datos = fun (Game) -> global:send(Game,{datos,self()}),
														receive
														{send,Jugadores,Observadores} -> gen_tcp:send(CSock,"Jugadores:\n"),
																						 Imp_datos(Jugadores),
																						 gen_tcp:send(CSock,"Observadores: \n"),
																						 Imp_datos(Observadores),
																						 gen_tcp:send(CSock,"_______________\n")
														end
				end,
	receive
		{send,L} -> lists:foreach(fun (X) -> gen_tcp:send(CSock,atom_to_list(X)++"\n"),
											 Obt_datos(X) end, L)
	end.

acc(Juego,CSock,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive
	{send,L} -> case lists:member(J,L) of
				false -> gen_tcp:send(CSock,"ERROR Juego inexistente\n");
				true  -> global:send(J,{datos,self()}),
						 receive {send,Jugadores,Observadores} ->
							if length(Jugadores) == 2 -> gen_tcp:send(CSock,"Juego en curso\n");
								 true -> global:send(J,{update,lists:append(Jugadores,[N]),Observadores}),
											 	 gen_tcp:send(CSock,"OK ACC\n"),
											 	 global:send(J,{start})
							end
						 end
				end
	end.

game_init(N,J) ->
	Tablero = inicializar_tablero(),
	Jugadores = [N],
	Observadores = [],
	Turno = 1,
	game(J,Tablero,Jugadores,Observadores,Turno).
	
game(J,Tablero,Jugadores,Observadores,Turno) ->
	receive
		{datos,Pid} -> Pid ! {send,Jugadores,Observadores}, game(J,Tablero,Jugadores,Observadores,Turno);
		{update,Jugadores_new,Observadores_new} -> game(J,Tablero,Jugadores_new,Observadores_new,Turno);
		{start} ->io:format("El jugador es: ~p~n",[lists:nth(Turno,Jugadores)]),
							global:send(lists:nth(Turno,Jugadores),{print,"-- Es tu turno --\n"}),
							Aux = "+ "++atom_to_list(J)++" | "++atom_to_list(lists:nth(1,Jugadores))++" | "++atom_to_list(lists:nth(2,Jugadores))++" +\n",
							global:send(lists:nth(Turno,Jugadores),{print,Aux}),
							global:send(lists:nth(Turno,Jugadores),{print,tprint(Tablero)}),
							game(J,Tablero,Jugadores,Observadores,Turno+1)

	end.

%% Toda esta mierda para tener un tablero bonito :D:D
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

tprint(D) -> A = "+______________+\n\n",
						 B = "| "++find(1,D)++" | "++find(2,D)++" | "++find(3,D)++" |\n| "++find(4,D)++" | "++find(5,D)++" | "++find(6,D)++" |\n| "++find(7,D)++" | "++find(8,D)++" | "++find(9,D)++" |\n",
						 A++B++A.
													

find(N,D) ->
	case dict:find(N,D) of
		{ok,Result} -> Result
	;	error       -> '-'
		end.

