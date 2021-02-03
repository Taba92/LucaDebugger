-module(test).
-export([main/0,client/0,server/0]).

main()->
	spawn(?MODULE,client,[]).

client()->
	Pid=spawn(?MODULE,server,[]),
	link(Pid),
	Pid ! ciao.

server()->
	self(),
	4+3.