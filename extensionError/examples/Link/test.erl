-module(test).
-export([main/0,client/0,server/0]).

main()->
	spawn(?MODULE,client,[]).

client()->
	Pid=spawn_link(?MODULE,server,[]),
	unlink(Pid),
	Pid ! ciao.

server()->
	self(),
	4+3.