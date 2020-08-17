-module(watcher).
-export([main/0,errorRaise/0,anotherWorker/0,loop/0]).

main()->
	process_flag(trap_exit,true),
	spawn_link(?MODULE,errorRaise,[]),
	3/0,
	startSupervision().

errorRaise()->
	spawn_link(?MODULE,anotherWorker,[]),
	3/0,
	self(),
	self().

anotherWorker()->
	spawn_link(?MODULE,loop,[]),
	3/0.

loop()->
	process_flag(trap_exit,true),
	5/3.

startSupervision()->
	receive
		Error->
			Error
	end,
	startSupervision().