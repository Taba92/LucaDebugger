-module(watcher).
-export([main/0,errorRaise/0,anotherWorker/0,loop/0]).

main()->
	process_flag(trap_exit,true),
	spawn_link(?MODULE,errorRaise,[]),
	startSupervision().

errorRaise()->
	spawn_link(?MODULE,anotherWorker,[]),
	%3/0,
	exit(normal),
	self(),
	self().

anotherWorker()->
	spawn_link(?MODULE,loop,[]),
	4/2.

loop()->
	process_flag(trap_exit,true),
	5/3.

startSupervision()->
	receive
		Error->
			Error
	end,
	startSupervision().