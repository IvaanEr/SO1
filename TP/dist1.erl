-module(dist1).
-compile(export_all).

s() -> receive {Pid,M} -> Pid ! {M,node()} end,
	s().

f(N)-> {N*2,node()}.

print(M) -> io:format(user, "mensaje: ~p", [M]).

%%Proceso que migra.!

s_loop(N) ->
	receive
		{move,Nodo}-> io:format("Moviendome.."), 
		NuevoProc = spawn(Nodo,?MODULE,s_loop,[N]),
		global:re_register_name(server,NuevoProc),
		sendInbox(NuevoProc);
		
		{ Pid,X} -> io:format("~p clientes atendidos por ~p~n",[N,self()]),
		Pid ! {resultado,X+1},
		s_loop(N+1)
	end.

sendInbox(NuevoProc) ->
	receive M -> NuevoProc ! M, sendInbox(NuevoProc)
		after 0 -> ok
	end.	
%%Primero:
%% darle un nombre
	
s_server()-> 
	global:register_name('ezio',self()),
	s_loop(1).