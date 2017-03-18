-module(server).
-compile(export_all).

%%=================================================%%
% Trabajo Practico Final - Sistemas Operativos I    %
% --------------------------------------------------%
% Integrantes: Ivan Ernandorena - Julio Guella -    %
%              Luis Dzikiewicz                      %
% --------------------------------------------------%
%         SERVIDOR DE JUEGOS DISTRIBUIDO            %
%%=================================================%%

%%Observación:
%%Para acceder al segundo jugador de la lista de jugadores de un juego especifico
%%se usa lists:nth ya que sino deberia llamar a dos funciones: "tl" que me devuelve
%%UNA LISTA con el ultimo elemento (ya que solo son 2) y a "hd" para obetener ese elemento

%Comienza el sistema distribuido, con hacer ping a un solo nodo basta.
%Ya que se sincroniza con todo el sistema.

start(SName,Port,LServer) ->
	net_kernel:start([SName,shortnames]),
	case length(LServer) > 0 of
			true -> S = hd(LServer),
							net_adm:ping(S);
			false -> ok
	end,
	spawn(?MODULE,server,[Port]).

server(Port) ->
	Resolve = fun (_,Pid1,_) -> Pid1 end,
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
	ClientPid = spawn(?MODULE,list_of_client,[[]]), %%creo el proceso que maneja la lista para los clientes
	yes = global:register_name(clients_pid,ClientPid,Resolve),
	GamesPid = spawn(?MODULE,lists_of_games,[[]]), %%crea el proceso que maneja la lista de juegos en curso.
	yes = global:register_name(games_pid,GamesPid,Resolve),
	PidBalance = spawn(?MODULE,pbalance,[]), 
	register(pbalance,PidBalance),
	PidStat = spawn(?MODULE,pstat,[]),
	register(pstat,PidStat),
	PidCargas = spawn(?MODULE,cargas,[dict:new()]), %%Diccionario para llevar la carga de los nodos
	yes = global:register_name(cargas,PidCargas,Resolve),
	global:send(cargas,{newNode,node()}),
	dispatcher(LSock). %%llamo a dispatcher con el listen socket

%% Espera nuevas conexiones y crea un proceso para atender cada una.
dispatcher(LSock) ->
		{ok, CSock} = gen_tcp:accept(LSock),
		{pbalance,node()} ! {req,self()},							%%psocket lo ejecuto en el nodo menos cargadp
		receive {send,Nodo} ->												%%
		Pid = spawn(Nodo,?MODULE, psocket, [CSock])   %%
		end,
		ok = gen_tcp:controlling_process(CSock, Pid), %%Ahora a CSock lo controla Pid -- los mensajes a CSock llegan a Pid
		Pid ! ok,
		dispatcher(LSock).
	
%Atendera todos los pedidos del cliente hablando en CSock.
psocket(CSock) ->
	receive ok -> ok end, %%para que controlling process suceda antes que setopts.
	ok = inet:setopts(CSock	,[{active,true}]), %%recivo msjs con receive, no con gen_tcp:recv.	
	psocket_loop(CSock).

%%Primer PSocket: Solo para registrarse.
psocket_loop(CSock) ->
	receive
		{tcp,CSock,Data} ->
			{pbalance,node()} ! {req,self()},
			receive {send,Nodo} ->
				case string:tokens(lists:sublist(Data, length(Data)-1)," ") of
					["CON",Nombre] 	-> spawn(Nodo,?MODULE,connect,[Nombre,CSock,self()]),
														receive {con,N} -> io:format("registro exitoso~n"),psocket_loop(CSock,N);
																		{error} -> psocket_loop(CSock)
														end;
					["HELP"] 				-> gen_tcp:send(CSock,"HelpSinCon"),psocket_loop(CSock);
					["BYE"] 			 	-> gen_tcp:close(CSock),exit(normal);
					_Else 					-> gen_tcp:send(CSock,"ErReg"),
														 psocket_loop(CSock)
				end
			end;
		{error,Closed} -> io:format("Closed:~p~n",[Closed]),exit(normal)
	end.


%%Segundo PSocket: Una vez registrado N tiene acceso a los demas comandos.
psocket_loop(CSock,N) ->
receive 
	{tcp,CSock,Data} ->
			{pbalance,node()} ! {req,self()},
			receive
				{send,Nodo} 	 ->	spawn(Nodo,?MODULE, pcommand, [Data,CSock,N]),
													psocket_loop(CSock,N)
			end;
	empate         -> gen_tcp:send(CSock,"Empate"),psocket_loop(CSock,N);
	er_juego_inex  -> gen_tcp:send(CSock,"ErPlaInex"),psocket_loop(CSock,N);
	er_pla_cas1		 -> gen_tcp:send(CSock,"ErPlaCas1"),psocket_loop(CSock,N);
	er_pla_jug     -> gen_tcp:send(CSock,"ErPlaJug"),psocket_loop(CSock,N);
	er_pla_cas2    -> gen_tcp:send(CSock,"ErPlaCas2"),psocket_loop(CSock,N);
	er_pla_turno   -> gen_tcp:send(CSock,"ErPlaTur"),psocket_loop(CSock,N);
	er_obs_part    -> gen_tcp:send(CSock,"ErObsPart"),psocket_loop(CSock,N);
	er_obs_ya      -> gen_tcp:send(CSock,"ErObsYa"),psocket_loop(CSock,N);
	{ok_obs,G}     -> gen_tcp:send(CSock,"OkObs "++G),psocket_loop(CSock,N);
	er_lea         -> gen_tcp:send(CSock,"ErLea"),psocket_loop(CSock,N);
	ok_lea         -> gen_tcp:send(CSock,"OkLea"),psocket_loop(CSock,N);
	{abandona,G}   -> gen_tcp:send(CSock,"Abandona "++G),psocket_loop(CSock,N);
	{abandona2,J,G}-> gen_tcp:send(CSock,"Abandona2 "++J++" "++G),psocket_loop(CSock,N);

	{print,Data}   -> gen_tcp:send(CSock,Data),psocket_loop(CSock,N);
	{error,Closed} -> io:format("Closed:~p~n",[Closed]),gen_tcp:close(CSock),exit(normal)
end.	

pcommand(Data,CSock,N) ->
	
	case string:tokens(lists:sublist(Data, length(Data)-1)," ") of
		["CON",_]               -> gen_tcp:send(CSock,"ErCon2");
		["LSG"]                 -> lsg(CSock);
		["NEW", NJuego]         -> newgame(NJuego,CSock,N);
		["ACC", Juego]  				-> acc(Juego,CSock,N);
		["PLA", NJuego,Casilla] -> jugada(NJuego,Casilla,N);
		["OBS", NJuego] 				-> observa(NJuego,N);
		["LEA", NJuego] 				-> lea(N,NJuego);
		["BYE"] 								-> bye(N);
		["HELP"] 								-> game:help(N);
		_Else     							-> gen_tcp:send(CSock,"Er")
	end.

%%Encargado de mandar la info de carga al resto de los nodos
%%Cada cierto tiempo, le envio al proceso global cargas "quien soy" y el estado de mi carga.
pstat() -> 
	Carga = statistics(total_active_tasks),
	global:send(cargas,{update,node(),Carga}),
 
	receive after 20000 -> ok end,
	pstat().

%% Cuando alguien (Pid) le pide el nodo con menor carga. pbalance le pregunta a cargas cual es este nodo
pbalance() -> 
	receive
		{req,Pid} -> global:send(cargas,{req,self()}),
								 receive 
									{send,Node} -> Pid!{send,Node}
								 end
	end,
	pbalance().

%Proceso que lleva el diccionario {Nodo,Carga}.
%% Si se agrega un nuevo nodo, lo agrega al dicc
%% pstat de cada nodo regularmente le informa la carga
%% cuando pbalance le pide un nodo, le envia el de menor carga
cargas(Dict) ->
	receive
	{newNode,Node}      -> cargas(dict:store(Node,statistics(total_active_tasks),Dict));
	{update,Node,Carga} -> cargas(dict:store(Node,Carga,Dict)); 
	{req,Pid}           ->  %Magia negra para calcular el nombre del nodo de menor carga.
													Min = dict:fold(fun(K,V,{S,C})-> 	if V < C -> {K,V};
																															true -> {S,C} 
																														end end,{node(),dict:fetch(node(),Dict)},Dict),
													Pid ! {send,element(1,Min)},
													cargas(Dict)
	end.

%Comando CON. Registra un usuario con Nombre.	
connect(Nombre, CSock, PSocketPid) ->
		N = list_to_atom(Nombre),
		case global:register_name(N,PSocketPid) of
				yes -> 	global:send(clients_pid,{new_client,N}),
								gen_tcp:send(CSock, "OkCon "++Nombre),
								io:format("OK CON~n"),
								PSocketPid ! {con,N};
				no 	-> 	io:format("ErCon "++Nombre),
				gen_tcp:send(CSock, "ErCon "++Nombre),
				PSocketPid ! {error}
		end.

%% Lista de los clientes conectados
list_of_client(L) ->
		receive
				{new_client,Client} ->	list_of_client(L++[Client]);
				{req,Pid} 					->	Pid ! {send,L},
																list_of_client(L);
				{elim,N} 						-> 	list_of_client(lists:delete(N,L))
		end.

%% Lista de todos los juegos en curso.
lists_of_games(L) ->
	receive
		{new,Juego} 	-> lists_of_games(L++[Juego]);
		{req,Pid} 		-> Pid ! {send,L},
										 lists_of_games(L);
		{elim,Juego} 	-> lists_of_games(lists:delete(Juego,L))
	end.

%Comando NEW. N crea el juego NJuego.
newgame(NJuego,CSock,N) ->
	J = list_to_atom(NJuego),
	Game = spawn(?MODULE,game_init,[N,J]),
	case global:register_name(J,Game) of
		yes -> gen_tcp:send(CSock,"OkNewGame "++NJuego),
					 global:send(games_pid,{new,J});
		no 	-> gen_tcp:send(CSock,"ErNewGame")
	end.
	
%Comando LSG. Para cada juego, su ID y participantes.
%%Esta funcion directamente imprime en el cliente.
lsg(CSock) ->
	%%Imprimo nombres, de jugadores o observadores, separados por "|".
	Imp_datos = fun (L) -> 	lists:foreach(fun (X) -> gen_tcp:send(CSock,atom_to_list(X)++" | ") end,L),
													gen_tcp:send(CSock,"~n") end,
	%%Uso Imp_datos para imprimir los jugadores y observadores de cierto juego.						 						
	Obt_datos = fun (Game) -> global:send(Game,{datos,self()}),
														receive
														{send,Jugadores,Observadores} -> gen_tcp:send(CSock,"Jugadores:~n"),
																						 Imp_datos(lists:map(fun(X)-> element(2,X) end,Jugadores)),
																						 gen_tcp:send(CSock,"Observadores:~n"),
																						 Imp_datos(Observadores),
																						 gen_tcp:send(CSock,"············~n~n")
														end
							end,
	gen_tcp:send(CSock,"Lista de juegos:~n"),
	%%Uso Obt_datos para imprimir jugadores y observadores de todos los juegos.
	global:send(games_pid,{req,self()}),
	receive
		{send,L} -> lists:foreach(fun (X) -> gen_tcp:send(CSock,"Juego: "++atom_to_list(X)++"~2n"),
											 Obt_datos(X) end, L)
	end.

%Comando ACC. N accede a Juego si es posible.
acc(Juego,CSock,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive
	{send,L} -> case lists:member(J,L) of
				false -> gen_tcp:send(CSock,"ErAccInex");
				true  -> global:send(J,{datos,self()}),
						 receive {send,Jugadores,Observadores} ->
							if length(Jugadores) == 2 -> gen_tcp:send(CSock,"ErAccEnCurso");
								 true -> case element(2,hd(Jugadores)) == N of
												 true 	-> gen_tcp:send(CSock,"ErAccContraTi");
												 false 	-> global:send(J,{update,Jugadores++[{2,N}],Observadores}),
																		gen_tcp:send(CSock,"OkAcc "++Juego),
																		global:send(J,{start})
												end
							end
						 end
				end
	end.

%%Inicializa el juego. Crea el tablero y parametros iniciales.
game_init(N,J) ->
	Tablero = game:inicializar_tablero(),
	Jugadores = [{1,N}],
	Observadores = [],
	Turno = (random:uniform(1000) rem 2)+1, %un numero aleatorio entre 1 y 2, para que no empiece siempre el mismo
	io:format("Turno: ~p~n",[Turno]),
	game(J,Tablero,Jugadores,Observadores,Turno).

%% Maneja el juego J.
%% Jugadores = [{1,J1},{2,J2}]
%% Se usa datos,datos2 para no enviar cosas innecesarias cuando se puede evitar.
%% Lo mismo para update y update2.
game(J,Tablero,Jugadores,Observadores,Turno) ->
	receive
		{datos,Pid} -> Pid ! {send,Jugadores,Observadores}, game(J,Tablero,Jugadores,Observadores,Turno);
		{datos2,Pid} -> Pid ! {send,Tablero,Jugadores,Observadores,Turno},game(J,Tablero,Jugadores,Observadores,Turno);
		{update,Jugadores_new,Observadores_new} -> game(J,Tablero,Jugadores_new,Observadores_new,Turno);
		{update2,TNew} -> game:upd(J,Jugadores,TNew,Observadores,Turno),
											 case game:gano(TNew) of
												true -> case Turno == 1 of
																	true  -> game:ganoJ1(Jugadores,Observadores),global:send(games_pid,{elim,J}),exit(normal);
																	false -> game:ganoJ2(Jugadores,Observadores),global:send(games_pid,{elim,J}),exit(normal)
																end;
												false -> case game:empate(TNew) of
																	true -> lists:foreach(fun(X)->global:send(element(2,X),empate) end,Jugadores),
																					lists:foreach(fun(Y)->global:send(Y,empate) end,Observadores),
																					global:send(games_pid,{elim,J}),exit(normal);
																	false -> TurnoNew = game:turno(Turno),
																					 X = game:es_turno(TurnoNew,Jugadores),
																					 global:send(X,{print,"-- Es tu turno "++atom_to_list(X)++" --~n"}), 
																					 game(J,TNew,Jugadores,Observadores,TurnoNew)
																 end
												end;
		{start} ->X = game:es_turno(Turno,Jugadores),
							global:send(X,{print,"-- Es tu turno "++atom_to_list(X)++" --~n"}),
							Aux = "+ "++atom_to_list(J)++" | "++atom_to_list(element(2,hd(Jugadores)))++" (X) | "++atom_to_list(element(2,lists:nth(2,Jugadores)))++" (0) +~n~n",
							lists:foreach(fun(Y) -> global:send(element(2,Y),{print,Aux}),global:send(element(2,Y),{print,game:tprint(Tablero)}) end,Jugadores),
							lists:foreach(fun(Z) -> global:send(Z,{print,Aux}),global:send(Z,{print,game:tprint(Tablero)}) end,Observadores),
							game(J,Tablero,Jugadores,Observadores,Turno);
		bye -> global:send(games_pid,{elim,J}),exit(normal)
	end.


%%Comando PLA. Actualiza, si esta permitido, la Casilla del juego NJuego.	
jugada(NJuego,Casilla,N) ->
	J = list_to_atom(NJuego),
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		case lists:member(J,L) of
				false -> global:send(N,er_juego_inex); %el juego no existe...
				true -> 
					if 	Casilla == "BYE" -> %%se puede abandonar la partida con PLA.
								abandona(N,NJuego),
								exit(normal);
							true -> ok
					end,
					case (Casilla < "1") or (Casilla > "9") of
						true -> global:send(N,er_pla_cas1),exit(normal);
						false -> ok
					end,
					C = list_to_integer(Casilla),
					global:send(J,{datos2,self()}),
					receive
						{send,T,Jugadores,_,Turno} ->
							case length(Jugadores) /= 2 of 
								 true -> global:send(N,er_pla_jug);
								 false -> A = element(2,hd(Jugadores)), B = element(2,lists:nth(2,Jugadores)),
												 case lists:member(N,[A,B]) and (game:es_turno(Turno,Jugadores) == N) of 
															true -> case game:find(C,T) == " " of 
																				true -> TNew = game:update(C,T,Turno),
																								global:send(J,{update2,TNew});
																				false -> global:send(N,er_pla_cas2)
																			end;
															false -> global:send(N,er_pla_turno)
												 end
							end
					end
		end
	end.
	
abandona(N,NJuego) ->	
	J = list_to_atom(NJuego),
	% solo llamo a esta funcion en jugada, donde ya chequie si el juego existe.
	global:send(J,{datos,self()}),
	receive
		{send,Jugadores,_} ->
			J1 = element(2,hd(Jugadores)),
			
			if 	length(Jugadores) /= 2 -> 
						%%Solo está el jugador 1
						global:send(J1,{abandona,NJuego}),global:send(J,bye);
					true ->
					J2 = element(2,lists:nth(2,Jugadores)),
					case N == J1 of
						true -> global:send(J1,{abandona,NJuego}),
										global:send(J2,{abandona2,atom_to_list(J1),NJuego}),
										global:send(J,bye);
						false -> global:send(J2,{abandona,NJuego}),
										global:send(J1,{abandona2,atom_to_list(J2),NJuego}),
										global:send(J,bye)
					end
			end
	end.

observa(Juego,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		case lists:member(J,L) of
			false -> global:send(N,er_juego_inex); %%Utilizo el mismo mensaje que antes. El error es el mismo.
			true -> global:send(J,{datos2,self()}),
							receive
								{send,Tablero,Jugadores,Observadores,Turno} -> 	
									case (element(2,hd(Jugadores)) == N) or (element(2,lists:nth(2,Jugadores)) == N) of
										true -> global:send(N,er_obs_part);
										false ->	case lists:member(N,Observadores) of
																true -> global:send(N,er_obs_ya);
																false ->  global:send(games_pid,{req,self()}),
																					receive {send,L} ->
																						case lists:member(J,L) of
																							false -> global:send(N,er_juego_inex);
																							true -> global:send(N,{ok_obs,Juego}),
																											game:presentacion_o(J,Jugadores,Tablero,[N],Turno),
																											global:send(J,{update,Jugadores,Observadores++[N]})
																						end
																					end
															end
									end
							end
		end
	end.							

lea(N,NJuego) ->
	J = list_to_atom(NJuego),
	global:send(games_pid,{req,self()}),
	receive
		{send,L} -> 
			case lists:member(J,L) of
				false -> global:send(N,er_juego_inex);
				true -> global:send(J,{datos,self()}),
								receive
									{send,Jugadores,Observadores} -> 
											case lists:member(N,Observadores) of
												false -> global:send(N,er_lea);
												true -> Obs_new = lists:delete(N,Observadores),
																global:send(N,ok_lea),
																global:send(J,{update,Jugadores,Obs_new})
											end
								end
			end
	end.

%%Cuando alguien cierra su conexion, comunicamos a todos
%%sus contrincantes y observadores de dichos juegos
%%que ha abandonado.
bye(N) ->
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		lists:foreach(fun (X) -> global:send(X,{datos,self()}),
								 receive
									{send,Jugadores,Observadores} ->
										J1 = element(2,hd(Jugadores)),
										if (length(Jugadores) == 1) and (J1 == N)-> 
											global:send(X,bye);
											true -> 
												J2 = element(2,lists:nth(2,Jugadores)),
												case J1 == N of
													 true -> 
														 global:send(J2,{abandona2,atom_to_list(J1),X}),
														 lists:foreach( fun (Z) -> global:send(Z,{abandona2,atom_to_list(J1),X}) end,Observadores),
														 global:send(X,bye); 
													 false -> case J2 == N of
																			true ->
																				global:send(J1,{abandona2,atom_to_list(J2),X}),
																				lists:foreach( fun (Y) -> global:send(Y,{abandona2,atom_to_list(J2),X}) end,Observadores),
																				global:send(X,bye); 
																			false -> 
																				ok 	
																		end
												end
										end
									end end, L),
		global:send(N,{error,normal})
	end.
 