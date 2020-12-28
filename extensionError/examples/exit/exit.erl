-module(exit).
-export([main/0,launcher/0,exiter/1]).

main()->
	spawn(?MODULE,launcher,[]).

launcher()->
	process_flag(trap_exit,true),
	spawn_link(?MODULE,exiter,[self()]),
	receive M->io:fwrite("~p~n",[M]) end.

exiter(Pid)->
	exit(Pid,kill).