-module(game).
-compile(export_all).

%% recive un diccionario con claves {1,2,...,9}
%% donde si la clave tendra asociado un " ", X o O
tablero(D) ->
	io:format("~p | ~p | ~p ~n~p | ~p | ~p ~n~p | ~p | ~p ~n",[find(1,D),find(2,D),find(3,D),
			                     find(4,D),find(5,D),find(6,D),
			                     find(7,D),find(8,D),find(9,D)]).

find(N,D) ->
	case dict:find(N,D) of
		{ok,Result} -> Result;
		error       -> '-'
    end.

		
	