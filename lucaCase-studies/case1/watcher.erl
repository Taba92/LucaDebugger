-module(watcher).
-export([main/0,startSupervisionT/0,startSupervisionF/0]).

main()->
	spawn_link(?MODULE,startSupervisionF,[]),
	spawn_link(?MODULE,startSupervisionF,[]),
	spawn_link(?MODULE,startSupervisionF,[]),
	spawn_link(?MODULE,startSupervisionF,[]),
	%self().
	%Pid=spawn_link(?MODULE,test,[]),
	%Pid ! {3,0}.
	3/0.


startSupervisionT()->
	process_flag(trap_exit, true),
	receive
		{'EXIT',_,Reason}->
			case Reason==normal of
				true->
					normal;
				false->
					Reason
			end
	end,
	startSupervisionT().

startSupervisionF()->
	receive
		{'EXIT',_,Reason}->
			case Reason==normal of
				true->
					normal;
				false->
					Reason
			end
	end,
	startSupervisionF().

test()->
	receive
		{X,Y}->
			Z=X/Y
	end.