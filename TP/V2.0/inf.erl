-module(inf).
-compile(export_all).

infinite(N) ->
	case N > 99999999999999999999.9999999999999999999999 of
		true -> io:format("Ready inf~n");
		false -> infinite(N+0.1)
		end.
		
% sleep() ->
% 	io:format("Antes...~n"),
% 	receive after 2000 -> ok end,
% 	io:format("Dos segundos despues...~n"),
% 	sleep().


% lala(List) ->
% 	lists:flatmap(fun erlang:integer_to_list/1, List).