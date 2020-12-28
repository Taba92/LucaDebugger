-module(clientServer).
-export([main/0,server/1,client/1,worker/3]).

main()->
	Pid=spawn(?MODULE,server,[[]]),
	spawn(?MODULE,client,[Pid]),
	spawn(?MODULE,client,[Pid]),
	spawn(?MODULE,client,[Pid]).

client(Server)->
	%%possibili input per la richiesta al server
	Terms=[49,ciao,78,98,0,"term",1000,4.5,74569,{4,5},1],
	{Index1,Index2}={rand:uniform(length(Terms)),rand:uniform(length(Terms))},
	Inputs={lists:nth(Index1,Terms),lists:nth(Index2,Terms)},
	Server !{calculate,self(),Inputs},
	timer:sleep(3000),
	receive
		{ret,badarg}->client(Server);
		{ret,Val}->
			Val
	end.

server(Map)->
	process_flag(trap_exit,true),
	receive 
		{calculate,Client,{First,Second}}->
			Worker=spawn_link(?MODULE,worker,[Client,First,Second]),
			server([{Worker,Client}|Map]);
		{'EXIT',Worker,normal}->
		 	server(remove(Worker,Map));
		{'EXIT',Worker,_}->
			Client=search(Worker,Map),
			Client ! {ret,badarg},
			server(remove(Worker,Map))
	end.

search(_,[])->not_exist;
search(Worker,[{Key,Client}|T])->
	case Worker of
		Key->Client;
		_->search(Worker,T)
	end.

remove(Worker,Map)->
	remove(Worker,Map,[]).
remove(_,[],Acc)->Acc;
remove(Worker,[{Key,Client}|T],Acc)->
	case Worker of
		Key->remove(Worker,T,Acc);
		_->remove(Worker,T,[{Key,Client}|Acc])
	end.

worker(Client,First,Second)->
	Val=First/Second,
	Client ! {ret,Val}.