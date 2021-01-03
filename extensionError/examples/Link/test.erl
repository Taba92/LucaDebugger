-module(test).
-export([main/0,client/1,server/0]).

main()->
	Pid=spawn(?MODULE,server,[]),
	spawn(?MODULE,client,[Pid]).

client(Pid)->
	unlink(Pid),
	Pid ! ciao.

server()->
	4+3.