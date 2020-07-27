-module(watcher).
-export([main/0,errorRaise/0,anotherWorker/0]).

main()->
	process_flag(trap_exit,true),
	spawn_link(?MODULE,anotherWorker,[]),
	startSupervision().

errorRaise()->
	3/1,
	exit(normal),
	self(),
	self().

anotherWorker()->
	%spawn_link(?MODULE,errorRaise,[]),
	loop().

loop()->
	4/2.

startSupervision()->
	receive
		Error->
			Error
	end,
	startSupervision().