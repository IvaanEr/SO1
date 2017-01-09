-module(server_1).
-compile(export_all).

server(Port,LServer) ->
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
    DiccPid = spawn(?MODULE,dicc,[]), %%creo el proceso que maneja el diccionario para los clientes
    ListPid = spawn(?MODULE,lists_of_games,[[]]), %%crea el proceso que maneja la lista de juegos en curso.
	dispatcher(LSock,DiccPid,ListPid). %%llamo a dispatcher con el listen socket y el pid de diccionario

dispatcher(LSock, DiccPid,ListPid) ->
	{ok, CSock} = gen_tcp:accept(LSock), %%acepta la conexion del cliente 
    Pid = spawn(?MODULE, psocket, [CSock,DiccPid,ListPid]),
    io:format("Todo bien hasta aca ~p ~n",[Pid]),
    ok = gen_tcp:controlling_process(CSock, Pid), %%Ahora a CSock lo controla Pid -- los mensajes de CSock llegan a Pid
    Pid ! ok,
    dispatcher(LSock, DiccPid,ListPid).
	

psocket(CSock,DiccPid,ListPid) ->
	receive ok -> ok end, %%magia para que controlling process suceda antes que setopts.
    io:format("Seguimoooooo"),	
	psocket_loop(CSock,DiccPid,ListPid).

psocket_loop(CSock,DiccPid,ListPid) ->
	ok = inet:setopts(CSock,[{active,true}]), %%recivo msjs con receive comun.
	receive
		{tcp,CSock,Data} ->
			io:format("psocket->Binary:~p CSock~p~n",[Data,CSock]),
			spawn(?MODULE,pcommand,[Data,CSock,DiccPid,ListPid,self()]),
			psocket_loop(CSock, DiccPid,ListPid);
		{error,Closed} -> io:format("Closed:~p~n",[Closed]) %%hace falta volver a llamar a psocketloop? chanchanchan
	end.
	

	%%receive??

pcommand(Data,CSock,DiccPid,ListPid,PSocketPid) ->
	io:format("pcommand->Data:~p~n",[Data]),
	ResList = lists:sublist(Data, length(Data)-2), %%le quitamos el \r\n
	io:format("pcommand->Data2:~p~n~nTransform:~p~n",[ResList,string:tokens(ResList," ")]),
	case string:tokens(ResList," ") of
	
		["CON",Nombre] -> connect(Nombre,CSock,DiccPid,PSocketPid);
		["LSG", Cmdid] -> ok;
		["NEW", NJuego] -> newgame(NJuego,CSock,ListPid);
		["ACC", Cmdid,Juegoid] -> ok;
		["PLA", Cmdid,Juegoid,Jugada] -> ok;
		["OBS", Cmdid,Juegoid] -> ok;
		["LEA", Cmdid, Juegoid] -> ok;
		["BYE"] -> ok;
		Otherwise -> error
	end.

pbalance() -> ok.

pstat() -> ok.
	
connect(Nombre, CSock, DiccPid,PSocketPid) ->
    io:format("Connect, antes del case~p~n",[DiccPid]),
    DiccPid ! {req2,self()},
    receive
    	{send,Dicc} ->
    		case dict:is_key(Nombre, Dicc) of
        		false -> Dicc2 = dict:store(Nombre, CSock, Dicc), io:format("Todo bien~p~n",[Dicc2]),
        				 DiccPid ! {recv,Dicc2},
        				 gen_tcp:send(CSock, "OK CON "++Nombre++"\n");
        				 %%psocket_loop(CSock,DiccPid,ListPid);
        		%% si el nombre no existe, lo agrego al diccionario con el Pid del cliente
        		%% y le envio el nuevo dicc a diccPid
        		true  -> io:format("ERROR: Nombre usado~n"), %% está de más.
        				 gen_tcp:send(CSock,"ERROR CON "++Nombre++" No esta disponible\n"),
        				 DiccPid ! {recv,Dicc}
                 		 %%psocket_loop(CSock,DiccPid,ListPid)
            end
    end.          
		

dicc() ->
	%% io:format("dicc Pid ~p~n",[self()]),
	Dicc = dict:new(),
	dicc_loop(Dicc). 

dicc_loop(Dicc) ->
	%% io:format("dicc_loop Pid ~p~n",[self()]),
    receive
        {req1,Pid} -> 
            Pid ! {send,Dicc},
            dicc_loop(Dicc);    %% con req1 sabemos que el proceso que pide Dicc no lo va a modificar
        {req2,Pid} ->           %% con req2 se va a modificar Dicc y esperamos a que llegue el nuevo. 
            Pid ! {send,Dicc},
            receive  
                {recv,Dicc1} -> dicc_loop(Dicc1) %% llega el dicc modificado y sigo con ese
            end
    end.


newgame(NJuego,CSock,ListPid) ->
	PGame = spawn(?MODULE,game,[CSock]),
	global:register_name(NJuego,PGame),
	%%io:format("PGame = ~s~n",[PGame]),
	ListPid ! {new,NJuego}.

game(PidJ1) ->
	Tablero = inicializar_tablero(),
	Jugadores = [PidJ1],
	Observadores = [],
	Turno = 1.

lists_of_games(L) ->
	receive
		{new,Juego} -> lists_of_games(lists:append(L,[Juego]))
		%%{}
	end.
	%%io:format("lists_of_games = ~p || Juego ~p ~n",[L,Juego]).














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
		{ok,Result} -> Result;
		error       -> '-'
    end.

