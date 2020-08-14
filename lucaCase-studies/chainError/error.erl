-module(error).
-export([main/0,spawnErr/1]).

main()->
	%process_flag(trap_exit,true),
	spawn_link(?MODULE,spawnErr,[0]),
	self().

spawnErr(2)->3/0;
spawnErr(N)->
	spawn_link(?MODULE,spawnErr,[N+1]),
	3/0