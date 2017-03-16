-module(client).
-compile(export_all).

%binary,{packet, 0},{active,false}

client(Ip,Port) ->
	{ok,CSock} = gen_tcp:connect(Ip,Port,[{packet,0},{active,false}]),
	io:format("Bienvenido!~n"),
	spawn(?MODULE,salida,[CSock]),
	entrada(CSock).

cmd(CMD,CSock) ->
	case gen_tcp:send(CSock,CMD) of
		ok -> {ok,Packet} = gen_tcp:recv(CSock,0),
					io:format("~p~n",[Packet]);
		{error,Reason} -> io:format("error: ~p~n",[Reason])
	end.


salida(CSock) ->
	Cmd = io:get_line("-> "),
	gen_tcp:send(CSock,Cmd).


entrada(CSock) ->
	case gen_tcp:recv(CSock,0) of
		{ok,Packet} -> io:format("Paquete: ~p~n",[Packet])
	end,
	entrada(CSock).