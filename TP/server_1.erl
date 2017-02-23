-module(server_1).
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
	receive
		{tcp,CSock,Data} ->
			case string:tokens(lists:sublist(Data, length(Data)-2)," ") of
				["CON",Nombre] -> spawn(?MODULE,connect,[Nombre,CSock,self()]),
								  receive {con,N} -> io:format("registro exitoso~n"),psocket_loop(CSock,N);
										  {error} -> psocket_loop(CSock)
								  end;
				["BYE"] -> ok;
				Otherwise -> gen_tcp:send(CSock,"Primero debes registrarte con CON\n"),
							 io:format("Pid: ~p quiere ejecutar comandos sin registrarse~n",[self()]),
							 psocket_loop(CSock)
			end;
		{error,Closed} -> io:format("Closed:~p~n",[Closed]) %%hace falta volver a llamar a psocketloop?
	end.
	
psocket_loop(CSock,N) ->
	io:format("psocket_loop\2"),
	receive
		{tcp,CSock,Data} ->
			spawn(?MODULE, pcommand, [Data,CSock,N]),
			psocket_loop(CSock,N);
		{error,Closed} -> io:format("Closed:~p~n",[Closed])
	end.	

pcommand(Data,CSock,N) ->
	case string:tokens(lists:sublist(Data, length(Data)-2)," ") of
		["CON",Nombre] -> gen_tcp:send(CSock,"Ya estas registrado\n");
		["LSG"] -> ok;%lsg(CSock);
		["NEW", NJuego] -> ok;%newgame(NJuego,CSock,PSocketPid);
		["ACC", Cmdid,Juegoid] -> ok;
		["PLA", Cmdid,Juegoid,Jugada] -> ok;
		["OBS", Cmdid,Juegoid] -> ok;
		["LEA", Cmdid, Juegoid] -> ok;
		["BYE"] -> ok;
		Otherwise -> error
	end.

pbalance() -> ok.

pstat() -> ok.
	
connect(Nombre, CSock,PSocketPid) ->
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

% newgame(NJuego,CSock,PSocketPid) ->
% 	RegisterNames = global:registered_names(),
% 	RegisterPids  = lists:map(fun (X) -> global:whereis_name(X) end,RegisterNames),

% 	case lists:member(PSocketPid,RegisterPids) of
% 		false -> gen_tcp:send(CSock, "Antes debes registrarte con CON\n");
% 		true -> 		








	% diccpid ! {req1,self()},
	% receive {send,Dicc,Dicc_inv} -> 
	% 	case dict:is_key(CSock, Dicc_inv) of %%si es true, es que CSock ya esta registrado.
	% 		false -> gen_tcp:send(CSock,"Primero registrate,perro\n"),
	% 				 io:format("ERROR NEW ~p~n",[NJuego]);
	% 	    true -> 
	% 	    		PGame = spawn(?MODULE,game,[CSock,NJuego]),
	% 				global:register_name(NJuego,PGame),
	% 				gen_tcp:send(CSock,"ok\n"),
	% 				io:format("OK NEW ~p~n",[NJuego]),
	% 				ListPid ! {new,NJuego}
	% 	end
	% end.

game(PidJ1,NJuego) ->
	Tablero = inicializar_tablero(),
	Jugadores = [PidJ1],
	Observadores = [],
	Turno = 1.


lsg(CSock,ListPid,DiccPid) ->

	DiccPid ! {req1,self()},
	receive 
		{send,Dicc,Dicc_inv} ->
			case dict:is_key(CSock,Dicc_inv) of
				false -> gen_tcp:send(CSock,"Primero registrate,perro\n"),
					     io:format("ERROR LSG~n");
			    true -> ListPid ! {req,self()},
						receive
							{send, L} -> gen_tcp:send(CSock ,"Juegos Disponibles:\n"),
										 print_l(L,CSock),
										 io:format("OK LSG~n")
						end
			end
	end.

print_l(L,CSock) -> 	
 	case L of
 		[]      -> gen_tcp:send(CSock,"________________________\n");
 		[H| LL] -> gen_tcp:send(CSock,H++"\n"),
 				   print_l(LL,CSock)
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

tprint(D) ->
	io:format("~p | ~p | ~p ~n~p | ~p | ~p ~n~p | ~p | ~p ~n",[find(1,D),find(2,D),find(3,D),
			                     find(4,D),find(5,D),find(6,D),
			                     find(7,D),find(8,D),find(9,D)]).

find(N,D) ->
	case dict:find(N,D) of
		{ok,Result} -> Result
	;	error       -> '-'
    end.

