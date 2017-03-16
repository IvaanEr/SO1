-module(client).
-compile(export_all).

%binary,{packet, 0},{active,false}

client(Ip,Port) ->
	{ok,CSock} = gen_tcp:connect(Ip,Port,[{packet,0},{active,false}]),
	io:format("Bienvenido!~n"),
	P = spawn(?MODULE,salida,[CSock,[]]),
	entrada(CSock,P).

cmd(CMD,CSock) ->
	case gen_tcp:send(CSock,CMD) of
		ok -> {ok,Packet} = gen_tcp:recv(CSock,0),
					io:format("~p~n",[Packet]);
		{error,Reason} -> io:format("error: ~p~n",[Reason])
	end.

salida(CSock,Name) ->
	Cmd = io:get_line(Name++"> "),
	gen_tcp:send(CSock,Cmd),
	receive 
		{new,N} -> salida(CSock,N);
		sigue -> salida(CSock,Name)
	end.


entrada(CSock,P) ->
	case gen_tcp:recv(CSock,0) of
		{ok,Packet} -> 
			case string:tokens(Packet," ") of
				["ErReg"]        -> io:format("Primero debes registrarte. Utilice CON [Id]~n");
				
				["OkCon",Nombre] -> io:format("Registro exitoso "++Nombre++"~n"), P!{new,Nombre};
				["ErCon",Nombre] -> io:format("El nombre "++Nombre++" esta en uso~n");
				["ErCon2"]       -> io:format("Ya estas registrado.~n");

				["OkAcc",J]      -> io:format("Accediste al juego: "++J++"~n");
				["ErAccInex"]    -> io:format("Juego inexistente.~n");
				["ErAccEnCurso"] -> io:format("El juego ya esta en curso.~n");
				["ErAccContraTi"]-> io:format("No puedes jugar contra ti mismo.~n");

				["OkNewGame",N]  -> io:format("El juego "++N++" fue creado exitosamente.~n");
				["ErNewGame"]    -> io:format("Nombre de juego en uso.~n");

				["ErPlaInex"]    -> io:format("Juego inexistente.~n");
				["ErPlaCas"]     -> io:format("Casilla incorrecta.~n");
				["ErPlaJug"]     -> io:foramt("Aun falta un jugador.~n")

				_Else -> io:format(Packet)
			end


	end,
	P!sigue,
	entrada(CSock,P).