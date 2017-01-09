-module(client).
-compile(export_all).

client() ->
	{ok,CSock} = gen_tcp:connect("127.0.0.1",8000,[binary,{packet, 0},{active,false}]),
	ok =  gen_tcp:send(CSock,"CON juego1").
	%% ok = gen_tcp:close(CSock).
