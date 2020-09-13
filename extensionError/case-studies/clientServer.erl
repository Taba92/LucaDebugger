-module(clientServer).
-export([main/0,server/1,client/2,worker/3]).

main()->
	Pid=spawn(?MODULE,server,[#{}]),
	spawn(?MODULE,client,[Pid,[]]),
	spawn(?MODULE,client,[Pid,[]]),
	spawn(?MODULE,client,[Pid,[]]).

client(Pid,Returns)->
	L=[0,1],
	Ind=rand:uniform(2),
	First=rand:uniform(45000),
	Pid !{calculate,self(),{First,lists:nth(Ind,L)}},
	timer:sleep(3000),
	receive
		{ret,badarg}->
			%io:fwrite("ERRORE RICHIESTA: ~p~n",[self()]),
			client(Pid,Returns);
		{ret,Val}->
			%io:fwrite("RICHIESTA SUCCESSO: ~p~n",[self()]),
			client(Pid,Returns++[Val])
	end.

server(Map)->
	process_flag(trap_exit,true),
	receive 
		{calculate,From,{First,Second}}->
			Pid=spawn_link(?MODULE,worker,[From,First,Second]),
			server(maps:put(Pid,From,Map));
		{'EXIT',Pid,normal}->
		 	server(maps:remove(Pid,Map));
		{'EXIT',Pid,Reason}->
			Client=maps:get(Pid,Map),
			Client ! {ret,badarg},
			server(maps:remove(Pid,Map))
	end.

worker(From,First,Second)->
	Val=First/Second,
	From ! {ret,Val}.