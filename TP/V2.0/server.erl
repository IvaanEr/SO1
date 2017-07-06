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

%Comienza el sistema distribuido
%Argumentos: SName: Un nombre para el nodo
%            Port : Puerto libre para que el nodo reciba conexiones
%            LServer: Lista con los nombres de los demas nodos del sistema. (Puede ser vacia)
start(SName,Port,LServer) ->
	net_kernel:start([SName,shortnames]),
	case length(LServer) > 0 of
			true -> S = hd(LServer),
							pong = net_adm:ping(S),
							spawn(?MODULE,server,[Port,true]);
			false -> spawn(?MODULE,server,[Port,false])
	end.

%Argumentos: Port: Puerto con el que se inicio el nodo
%			 Flag: Indica si es el primer nodo del sistema, para saber si es necesario
%                   crear los procesos que son registrados globalmente
server(Port,Flag) ->
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
	case Flag of
	false -> %significa que es el primer nodo. Se crean los procesos globales.
		ClientPid = spawn(?MODULE,list_of_client,[[]]), %%creo el proceso que maneja la lista para los clientes
		yes = global:register_name(clients_pid,ClientPid,fun global:random_notify_name/3),

		GamesPid = spawn(?MODULE,lists_of_games,[[]]), %%crea el proceso que maneja la lista de juegos en curso.
		yes = global:register_name(games_pid,GamesPid,fun global:random_notify_name/3),
		
		PidCargas = spawn(?MODULE,cargas,[orddict:new()]), %%Diccionario para llevar la carga de los nodos
		yes = global:register_name(cargas,PidCargas,fun global:random_notify_name/3),
		global:send(cargas,{newNode,node(),statistics(total_active_tasks)});
	true -> ok %ya estan creados, los nuevos nodos seran notificados automaticamente
  end,
	PidBalance = spawn(?MODULE,pbalance,[]), 
	true = register(pbalance,PidBalance),
	
	PidStat = spawn(?MODULE,pstat,[]),
	true = register(pstat,PidStat),
	
	dispatcher(LSock). %%llamo a dispatcher con el listen socket

%% Espera nuevas conexiones y crea un proceso para atender cada una.
% Argumentos: LSock: Listen Socket del nodo
dispatcher(LSock) ->
		{ok, CSock} = gen_tcp:accept(LSock),
		Pid = spawn(?MODULE, psocket, [CSock]),  
		ok = gen_tcp:controlling_process(CSock, Pid), %%Ahora a CSock lo controla Pid -- los mensajes a CSock llegan a Pid
		Pid ! ok,
		dispatcher(LSock).
	
%Atendera todos los pedidos del cliente hablando en CSock.
% Argumentos: CSock: Client Socket de cada cliente que se conecto al nodo
psocket(CSock) ->
	receive ok -> ok end, %%para que controlling process suceda antes que setopts.
	ok = inet:setopts(CSock	,[{active,true}]), %%recivo msjs con receive, no con gen_tcp:recv.	
	psocket_loop(CSock).

%%Primer PSocket: Solo para registrarse.
psocket_loop(CSock) ->
	receive
		{tcp,CSock,Data} ->
			{pbalance,node()} ! {req,self()},
			receive {send,Nodo} -> %io:format("Nodo: ~p~nNodo Ejec: ~p~n",[node(),Nodo]),
				case string:tokens(lists:sublist(Data, length(Data)-1)," ") of
					["CON",Nombre] 	-> spawn(Nodo,?MODULE,connect,[Nombre,self()]),

														receive {con,N} 			 -> %io:format("registro exitoso~n"),
																											 gen_tcp:send(CSock,"OkCon "++atom_to_list(N)),
																							 					psocket_loop(CSock,N);
																		{error,Nombre} -> gen_tcp:send(CSock, "ErCon "++Nombre),psocket_loop(CSock)
														end;
					["HELP"] 				-> gen_tcp:send(CSock,"HelpSinCon"),psocket_loop(CSock);
					["BYE"] 			 	-> gen_tcp:close(CSock),exit(normal);
					_Else 					-> gen_tcp:send(CSock,"ErReg"),
														 psocket_loop(CSock)
				end
			end;
		{error,Closed} -> exit(Closed)
	end.


%%Segundo PSocket: Una vez registrado N tiene acceso a los demas comandos.
% Argumentos: CSock: Client Socket de cada cliente que se conecto al nodo
%             N: [atomo] Nombre que eligio el cliente para conectarse al server
psocket_loop(CSock,N) ->
receive 
	{tcp,CSock,Data} ->
			{pbalance,node()} ! {req,self()},
			receive
				{send,Nodo} 	 ->	spawn(Nodo,?MODULE, pcommand, [Data,N])
													
			end;
	empate         -> gen_tcp:send(CSock,"Empate");
	er_con2        -> gen_tcp:send(CSock,"ErCon2");
	er_juego_inex  -> gen_tcp:send(CSock,"ErPlaInex");
	{ok_new_game,Nombre}-> gen_tcp:send(CSock,"OkNewGame "++Nombre);
	er_new_game    -> gen_tcp:send(CSock,"ErNewGame");
	er_acc_inex    -> gen_tcp:send(CSock,"ErAccInex");
	er_acc_en_curso-> gen_tcp:send(CSock,"ErAccEnCurso");
	er_acc_vs_ti   -> gen_tcp:send(CSock,"ErAccContraTi");
	{ok_acc,J}     -> gen_tcp:send(CSock, "OkAcc "++J);
	er_pla_cas1		 -> gen_tcp:send(CSock,"ErPlaCas1");
	er_pla_jug     -> gen_tcp:send(CSock,"ErPlaJug");
	er_pla_cas2    -> gen_tcp:send(CSock,"ErPlaCas2");
	er_pla_turno   -> gen_tcp:send(CSock,"ErPlaTur");

	er_obs_part    -> gen_tcp:send(CSock,"ErObsPart");
	er_obs_ya      -> gen_tcp:send(CSock,"ErObsYa");
	er_obs_nolisto -> gen_tcp:send(CSock,"ErObsNolisto");
	{ok_obs,G}     -> gen_tcp:send(CSock,"OkObs "++G);
	
	er_lea         -> gen_tcp:send(CSock,"ErLea");
	ok_lea         -> gen_tcp:send(CSock,"OkLea");
	{abandona,G}   -> gen_tcp:send(CSock,"Abandona "++G);
	{abandona2,J,G}-> gen_tcp:send(CSock,"Abandona2 "++J++" "++G);
    er             -> gen_tcp:send(CSock,"Er");

	{print,Data}   -> gen_tcp:send(CSock,Data);
	
	{bye,Closed}   -> global:send(clients_pid,{elim,N}),
										gen_tcp:close(CSock),exit(Closed)
end,
receive after 5 -> ok end, %%este "wait" es para que no acumule mensajes en un solo paquete tcp/ip, sino el PM
psocket_loop(CSock,N).     %%no funciona como es esperado.


%Argumentos: Data: Cadena de caracteres que llega desde un usuario
%            N: [atomo] Usuario que envio Data.
pcommand(Data,N) ->
	
	case string:tokens(lists:sublist(Data, length(Data)-1)," ") of
		["CON",_]               -> global:send(N,er_con2);
		["LSG"]                 -> lsg(N);
		["NEW", NJuego]         -> newgame(NJuego,N);
		["ACC", Juego]  				-> acc(Juego,N);
		["PLA", NJuego,Casilla] -> jugada(NJuego,Casilla,N);
		["OBS", NJuego] 				-> observa(NJuego,N);
		["LEA", NJuego] 				-> lea(N,NJuego);
		["BYE"] 								-> bye(N);
		["HELP"] 								-> game:help(N);
		_Else     							-> global:send(N, er) %gen_tcp:send(CSock,"Er")
	end.

%%Encargado de mandar la info de carga al resto de los nodos
%%Cada cierto tiempo, le envio al proceso global cargas "quien soy" y el estado de mi carga.
%% Se actualiza cada 5 segundos pero solo para que la prueba del balanceo sea mas dinamica y no tener
%% que esperar X segundos para ver si cambiaron los estados. ----- Cambiar a 30000 para algo mas realista.
pstat() -> 
	Carga = statistics(total_active_tasks),
	global:send(cargas,{update,node(),Carga}),
	receive after 5000 -> ok end,
	 
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
%% Si se agrega un nuevo nodo, lo agrega al dicc.
%% El proceso pstat (de cada nodo) regularmente le informa la carga del nodo.
%% Cuando pbalance le pide un nodo, le envia el de menor carga.
%% Para chequear que hace bien el balanceo descomentar los io:format
%%   y compilando "inf.erl", hacer spawn de infinite(0) para cargar un nodo.
% Argumentos: D: Diccionario {clave,valor} con clave: Nodo y valor: Carga
cargas(D) ->
	receive
	{newNode,Node,C}      -> cargas(orddict:store(Node,C,D));%io:format("Before newnode:~p~n",[D]);
	{update,Node,Carga} -> cargas(orddict:store(Node,Carga,D));%io:format("Before update: ~p~n",[D]); 
	{req,Pid}           ->  %Magia negra para calcular el nombre del nodo de menor carga.
													Min = orddict:fold(fun(K,V,{S,C})-> if V < C -> {K,V};
																															true -> {S,C} 
																														end end,hd(D),D),
													Pid ! {send,element(1,Min)},
													%io:format("Before req:~p~nMin:~p~n",[D,Min]),
													cargas(D)
	end.

%Comando CON. Registra un usuario con Nombre.
% Argumentos: Nombre [string]: Nombre que eligio el usuario
%             PSocketPid: Pid (Process ID) del proceso PSocket que esta atendiendo a este usuario	
connect(Nombre, PSocketPid) ->
		N = list_to_atom(Nombre),
		case global:register_name(N,PSocketPid) of
				yes -> 	global:send(clients_pid,{new_client,N}),
								%io:format("OK CON~n"),
								PSocketPid ! {con,N};
				no 	-> 	%
				%io:format("ErCon "++Nombre),
				PSocketPid ! {error,Nombre}
		end.

%% Lista de los clientes conectados
% Argumentos: L: Lista de los nombres [atomos] de todos los usuarios conectados.
list_of_client(L) ->
		receive
				{new_client,Client} ->	list_of_client(L++[Client]);
				{req,Pid} 					->	Pid ! {send,L},
																list_of_client(L);
				{elim,N} 						-> 	list_of_client(lists:delete(N,L))
		end.

%% Lista de todos los juegos en curso.
% Argumentos: L: Lista de los nombres [atomos] de todos los juegos creados.
lists_of_games(L) ->
	receive
		{new,Juego} 	-> lists_of_games(L++[Juego]);
		{req,Pid} 		-> Pid ! {send,L},
										 lists_of_games(L);
		{elim,Juego} 	-> lists_of_games(lists:delete(Juego,L))
	end.


	
%Comando LSG. Para cada juego, su ID y participantes.
%%Esta funcion directamente imprime en el cliente.
%Argumentos: N: Usuario que solicito la lista de los juegos.
lsg(N) ->
	%%Imprimo nombres, de jugadores o observadores, separados por "|".
	Imp_datos = fun (L) -> 	lists:foreach(fun (X) -> global:send(N,{print, atom_to_list(X)++" | "}) end,L),
													global:send(N,{print, "~n"}) end,
	%%Uso Imp_datos para imprimir los jugadores y observadores de cierto juego.						 						
	Obt_datos = fun (Game) -> global:send(Game,{datos,self()}),
														receive
														{send,Jugadores,Observadores} -> global:send(N,{print,"Jugadores:~n"}),
																						 Imp_datos(lists:map(fun(X)-> element(2,X) end,Jugadores)),
																						 global:send(N,{print, "Observadores:~n"}),
																						 Imp_datos(Observadores),
																						 global:send(N,{print, "············~n~n"})
														end
							end,
	global:send(N,{print,"Lista de juegos:~n"}),
	%%Uso Obt_datos para imprimir jugadores y observadores de todos los juegos.
	global:send(games_pid,{req,self()}),
	receive
		{send,L} -> lists:foreach(fun (X) -> global:send(N,{print,"Juego: "++atom_to_list(X)++"~2n"}),
											 Obt_datos(X) end, L)
	end.

%Comando ACC. N accede a Juego si es posible.
%Argumentos: Juego: [String] Nombre de juego al que se quiere acceder a jugar
%            N: Usuario que quiere acceder a dicho juego
acc(Juego,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive
	{send,L} -> case lists:member(J,L) of
				false -> global:send(N, er_acc_inex);
				true  -> global:send(J,{datos,self()}),
						 receive {send,Jugadores,Observadores} ->
							if length(Jugadores) == 2 -> global:send(N, er_acc_en_curso);
								 true -> case element(2,hd(Jugadores)) == N of
												 true 	-> global:send(N, er_acc_vs_ti);
												 false 	-> global:send(J,{update,Jugadores++[{2,N}],Observadores}),
																		global:send(N,{ok_acc,Juego}),
																		global:send(J,{start})
												end
							end
						 end
				end
	end.

%Comando NEW. N crea el juego NJuego.
%Argumentos: NJuego: [string] Nombre del juego, elegido por el usuario
%            N: Usuario que creo el juego.
newgame(NJuego,N) ->
	J = list_to_atom(NJuego),
	Game = spawn(?MODULE,game_init,[N,J]),
	case global:register_name(J,Game) of
		yes -> global:send(N,{ok_new_game,NJuego}),
					 global:send(games_pid,{new,J});
		no 	-> global:send(N,er_new_game) 
	end.

%%Inicializa el juego. Crea el tablero y parametros iniciales.
%Argumentos: N: Usuario que creo el juego
%            J: Nombre del juego
game_init(N,J) ->
	Tablero = game:inicializar_tablero(),
	Jugadores = [{1,N}],
	Observadores = [],
	Turno = 1,
	game(J,Tablero,Jugadores,Observadores,Turno).

%% Maneja el juego J.
%% Jugadores = [{1,J1},{2,J2}]
%% Se usa datos,datos2 para no enviar cosas innecesarias cuando se puede evitar.
%% Lo mismo para update y update2.
% Argumentos: J: Nombre del juego [atomo]
%             Tablero: Diccionario que lleva el tablero de juego
%             Jugadores: Lista de la forma [{1,J1},{2,J2}] donde Jx es el nombre del jugador [atom]
%             Observadores: Lista de observadores del juego J [atomos]
%             Turno: De quien es el turno actual. Coincide con el primer elemento de las tuplas en Jugadores.
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
		{start} ->%receive after 50 -> ok end,
						  X = game:es_turno(Turno,Jugadores),
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

	%Gracias a Erlang, se hace esto para chequear que "Casilla" es lo que se espera.
	Ascii = list_to_integer(lists:flatmap(fun erlang:integer_to_list/1, Casilla)),
	
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
					case ((Ascii < 49) or (Ascii > 57)) of %%En ascii: 49 -> 1 y 57 -> 9
						true -> global:send(N,er_pla_cas1),exit(normal);
						false -> ok
					end,
					C = Ascii - 48,
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

%% El jugador N abandona el juego NJuego.	
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

%% El jugador N quiere comenzar a observar Juego.
observa(Juego,N) ->
	J = list_to_atom(Juego),
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		case lists:member(J,L) of
			false -> global:send(N,er_juego_inex); %%Utilizo el mismo mensaje que antes. El error es el mismo.
			true -> global:send(J,{datos2,self()}),
							receive
								{send,Tablero,Jugadores,Observadores,Turno} -> 
									if length(Jugadores) == 1 ->
											global:send(N,er_obs_nolisto), exit(normal);
										true -> ok
									end,
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

%%El jugador N quiere dejar de observar NJuego
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
%%Tambien, si esta observando algun juego. Lo sacamos de la lista Observadores de dicho juego.
bye(N) ->
	global:send(games_pid,{req,self()}),
	receive {send,L} ->
		lists:foreach(fun (X) -> global:send(X,{datos,self()}),
								 receive
									{send,Jugadores,Observadores} ->
										Aux = lists:member(N,Observadores),
										if Aux -> global:send(X,{update,Jugadores,lists:delete(N,Observadores)}),global:send(N,{bye,normal}),exit(normal) end,
										J1 = element(2,hd(Jugadores)),
										if (length(Jugadores) == 1) and (J1 == N)-> %%hay uno solo y ese es el que abandono...
											global:send(X,bye); %%cierro el juego.
											true -> 
												J2 = element(2,lists:nth(2,Jugadores)), %%hay dos jugadores
												case J1 == N of %%J1 abandono
													 true -> 
														 global:send(J2,{abandona2,atom_to_list(J1),atom_to_list(X)}),
														 lists:foreach( fun (Z) -> global:send(Z,{abandona2,atom_to_list(J1),atom_to_list(X)}) end,Observadores),
														 global:send(X,bye); 
													 false -> case J2 == N of %J2 abandono
																			true ->
																				global:send(J1,{abandona2,atom_to_list(J2),atom_to_list(X)	}),
																				lists:foreach( fun (Y) -> global:send(Y,{abandona2,atom_to_list(J2),atom_to_list(X)}) end,Observadores),
																				global:send(X,bye); 
																			false -> 
																				ok 	
																		end
												end
										end
									end end, L),
		global:send(N,{bye,normal})
	end.
 