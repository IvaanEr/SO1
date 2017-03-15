-module(client).
-compile(export_all).

%binary,{packet, 0},{active,false}

client(Ip,Port) ->
	{ok,CSock} = gen_tcp:connect(Ip,Port,[{packet,0},{active,false}]),
	CSock.

cmd(CMD,CSock) ->
	case gen_tcp:send(CSock,CMD) of
		ok -> {ok,Packet} = gen_tcp:recv(CSock,0),
					io:format("~p~n",[Packet]);
		{error,Reason} -> io:format("error: ~p~n",[Reason])
	end.
