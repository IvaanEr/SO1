-module(server_1).
-compile(export_all).

server(Port,LServer) ->
	{ok,LSock} = gen_tcp:listen(Port, [{packet, 0},{active,false}]), %%server escucha en port Port
    DiccPid = spawn(?MODULE,dicc,[]), %%creo el proceso que maneja el diccionario
	dispatcher(LSock,DiccPid). %%llamo a dispatcher con el listen socket y el pid de diccionario

dispatcher(LSock, DiccPid) ->
	{ok, CSock} = gen_tcp:accept(LSock), %%acepta la conexion del cliente 
    Pid = spawn(?MODULE, psocket, [CSock,DiccPid]),
    ok = gen_tcp:controlling_process(CSock, Pid), %%Ahora a CSock lo controla Pid, los mensajes a Pid llegan a CSock
    Pid ! ok,
    dispatcher(LSock, DiccPid).
	

psocket(CSock,DiccPid) ->
	receive ok -> ok end,
	psocket_loop(CSock,DiccPid).

psocket_loop(CSock,DiccPid) ->
	ok = inet:setopts(CSock,[{active,true}]),
	receive
		{tcp,CSock,Data} ->
			%%io:format("psocket->Binary:~p Sock ~p CSock~p~n",[Data,Sock,CSock]),
			spawn(?MODULE,pcommand,[Data,CSock,DiccPid]),
			psocket_loop(CSock, DiccPid);
		{error,Closed} -> io:format("Closed:~p~n",[Closed])
	end.

pcommand(Data,CSock,DiccPid) ->
%%	io:format("pcommand->Data:~p~n",[Data]),
	ResList = lists:sublist(Data, length(Data)-2), %%le quitamos el \r\n
%%	io:format("pcommand->Data2:~p~n~nTransform:~p~n",[ResList,string:tokens(ResList," ")]),
	case string:tokens(ResList," ") of
	
		["CON",Nombre] -> connect(Nombre,CSock,DiccPid);
		["LSG", Cmdid] -> ok;
		["NEW", Cmdid] -> ok;
		["ACC", Cmdid,Juegoid] -> ok;
		["PLA", Cmdid,Juegoid,Jugada] -> ok;
		["OBS", Cmdid,Juegoid] -> ok;
		["LEA", Cmdid, Juegoid] -> ok;
		["BYE"] -> ok;
		Otherwise -> error
	end.
	
connect(Nombre, CSock, DiccPid) ->
    io:format("Connect, antes del case~p~n",[DiccPid]),
    DiccPid ! {req2,self()},
    receive
    	{send,Dicc} ->
    		case dict:is_key(Nombre, Dicc) of
        		false -> Dicc2 = dict:store(Nombre, CSock, Dicc), io:format("Todo bien~p~n",[Dicc2]),
        				 DiccPid ! {recv,Dicc2},
        				 gen_tcp:send(CSock,"OK"),
        				 psocket_loop(CSock,DiccPid);
        		%% si el nombre no existe, lo agrego al diccionario con el Pid del cliente
        		%% y le envio el nuevo dicc a diccPid
        		true  -> io:format("ERROR: Nombre usado~n"),
        				 gen_tcp:send(CSock,"ERROR"),
        				 DiccPid ! {recv,Dicc},
                 		 psocket_loop(CSock,DiccPid)
            end
    end.          
	

dicc() ->
	io:format("dicc Pid ~p~n",[self()]),
	Dicc = dict:new(),
	dicc_loop(Dicc). 

dicc_loop(Dicc) ->
	io:format("dicc_loop Pid ~p~n",[self()]),
    receive
        {req1,Pid} -> 
            Pid ! {send,Dicc},
            dicc_loop(Dicc);         %% con req1 sabemos que el proceso que pide Dicc no lo va a modificar
        {req2,Pid} ->           %% con req2 se va a modificar Dicc y esperamos a que llegue el nuevo. 
            Pid ! {send,Dicc},
            receive  
                {recv,Dicc1} -> dicc_loop(Dicc1) %% llega el dicc modificado y sigo con ese
            end
    end.
                       
	%%gen_tcp:send(CSock,"ERROR no implementado~n").

	%io:format("ERROR no implementado~n").
