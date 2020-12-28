-module(watcher).
-export([main/0,child/1]).

main()->
	process_flag(trap_exit,true),
	spawn_link(?MODULE,child,[true]),
	spawn_link(?MODULE,child,[false]),
	io:fwrite("~p~n",["ciao"]),
	%spawn_link(?MODULE,child,[false]),
	%spawn_link(?MODULE,child,[true]),
	startSupervision().

child(Flag)->
	process_flag(trap_exit,Flag),
	3/0,
	self().

startSupervision()->
	receive
		Error->
			Error
	end,
	startSupervision().