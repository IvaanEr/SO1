-module(client).
-compile(export_all).

%binary,{packet, 0},{active,false}

con(Ip,Port) ->
	{ok,CSock} = gen_tcp:connect(Ip,Port,[{packet,0},{active,false}]),
	io:format("Bienvenido!~nRecuerde, primero debes conectarte con el comando CON [id].~n"),
	P = spawn(?MODULE,salida,[CSock,[]]),
	entrada(CSock,P).

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
				["Er"]           -> io:format("Comando Incorrecto  ¯\\_(ツ)_/¯ HELP para la ayuda~n");

				["HelpSinCon"]   -> io:format("Primero debes conectarte con el comando CON [id].~n");

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
				["ErPlaCas1"]    -> io:format("Casilla incorrecta [Rango].~n");
				["ErPlaCas2"]		 -> io:format("Casilla incorrecta [Ocupada].~n");
				["ErPlaJug"]     -> io:format("Aun falta un jugador.~n");
				["ErPlaTur"]     -> io:format("No es tu turno o no perticipas en ese juego.~n");

				["Abandona",G]   -> io:format("Has abandonado "++G++".~n");
				["Abandona2",J,G]-> io:format(J++" ha abandonado "++G++".~n");

				["ErObsPart"]    -> io:format("No puedes observar un juego en el que participas.~n");
				["ErObsYa"]      -> io:format("Ya estas observando ese juego.~n");
				["OkObs",G]      -> io:format("Comenzaste a observar "++G++".~n");

				["ErLea"]        -> io:format("No estas observando ese juego.~n");
				["OkLea"]        -> io:format("Dejaste de observar ese juego.~n");

				["Empate"]       -> io:format("Han empatado. Al menos no perdieron ¯\\_(ツ)_/¯~n");

				_Else						 -> io:format(Packet)	
			end;

		{error,closed} -> io:format("El servidor se ha cerrado inesperadamente. Intente nuevamente en unos minutos (っ◕‿◕)っ~n")	

	end,
	P!sigue,
	entrada(CSock,P).