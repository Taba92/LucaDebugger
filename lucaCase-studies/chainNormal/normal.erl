-module(normal).
-export([main/0,spawnNorm/1]).

main()->
	process_flag(trap_exit,true),
	spawn_link(?MODULE,spawnNorm,[0]),
	self().

spawnNorm(2)->self(),self(),3/1;
spawnNorm(N)->
	spawn_link(?MODULE,spawnErr,[N+1]),
	self(),
	self(),
	3/1