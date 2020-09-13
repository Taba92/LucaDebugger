-module(watcher).
-export([main/0,child/2]).

main()->
	spawn_link(?MODULE,child,[true,self()]),
	spawn_link(?MODULE,child,[false,self()]),
	process_flag(trap_exit,true),
	%spawn_link(?MODULE,child,[false]),
	%spawn_link(?MODULE,child,[true]),
        receive X -> X end,
	startSupervision().

child(Flag,Pid)->
	process_flag(trap_exit,Flag),
        Pid!Pid,
	3/1,
	self().

startSupervision()->
	receive
		Error->
			Error
	end,
	startSupervision().
