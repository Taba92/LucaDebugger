-module(tracer).
-export([init/0]).
-include("cauder.hrl").

pp(CoreForm) -> lists:flatten(core_pp:format(CoreForm)).

%%convert from trace record to report for collector
showEvent(CollectorPid,#trace{type=Type,from=From,to=To,val=Val,time=Time})->
	case Type of
		?RULE_SPAWN->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val);
		?RULE_SPAWN_LINK->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val);
		?RULE_PROCESS_FLAG->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val);
		propag_normal->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val);
		propag_error->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val);
		exit->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val);
		_->
			et_collector:report_event(CollectorPid,85,From,To,Type,Val++" ("++integer_to_list(Time)++")")
	end;
showEvent(CollectorPid,ListTrace)->
	[showEvent(CollectorPid,Trace)||Trace<-ListTrace].

%%cit converts the FIELDS of the trace into human-readable form and in the receive case, it associates its sender
parseTrace(ListTrace,#trace{type=Type,from=From,to=To,val=Val,time=Time})->
	case Type of
		?RULE_RECEIVE->
			[ReceiveTo]=[Send#trace.from||Send<-ListTrace,Send#trace.type==?RULE_SEND,Send#trace.val==Val,Send#trace.time==Time],
			#trace{type=Type,from=pp(From),to=pp(ReceiveTo),val=pp(Val),time=Time};
		?RULE_SEND->
			#trace{type=Type,from=pp(From),to=pp(To),val=pp(Val),time=Time};
		?RULE_SPAWN->
			#trace{type=Type,from=pp(From),to=pp(To),val=Val,time=Time};
		?RULE_SPAWN_LINK->
			#trace{type=Type,from=pp(From),to=pp(To),val=Val,time=Time};
		?RULE_PROCESS_FLAG->
			#trace{type=Type,from=pp(From),to=pp(From),val=pp(Val),time=Time};
		?RULE_PROPAG->
			[T|Signals]=Val,
			Rule="propag_"++atom_to_list(T),
			SignalsTrace=[tracify(list_to_atom(Rule),From,Signal)||Signal<-Signals],
			SignalsTrace++[#trace{type=exit,from=pp(From),to=pp(From)}]
	end.
tracify(Rule,From,{LinkPid,MsgValue,Time})->
	#trace{type=Rule,from=pp(From),to=pp(LinkPid),val=pp(MsgValue),time=Time};
tracify(Rule,From,{LinkPid,error})->
	#trace{type=Rule,from=pp(From),to=pp(LinkPid)};
tracify(Rule,From,{LinkPid,normal})->
	#trace{type=Rule,from=pp(From),to=pp(LinkPid)}.

init()->%%initialize the tracer
	{ok,CollectorPid}=et_collector:start_link([]),
	Patterns={?RULE_SPAWN,?RULE_SPAWN_LINK,?RULE_PROCESS_FLAG,propag_normal,propag_error,?RULE_SEND,?RULE_RECEIVE,exit},
	et_viewer:start([{max_actors,20},{collector_pid,CollectorPid},{async,true},{async_patterns,Patterns}]),
	showingTrace(CollectorPid,[]).


showingTrace(CollectorPid,LastTrace)->%%start receive message for handle the viewer
	receive
		{show,ListTrace} when is_list(ListTrace)->
			io:fwrite("~p~n",[ListTrace]),
			io:fwrite("-----------------------------~n"),
			case ListTrace==LastTrace of %I compare the incoming list with the last list
				true->% if they are the same except the last one and I don't change the graphics
					showingTrace(CollectorPid,ListTrace);
				false-> %otherwise I save it, as the events appear, I empty the collector's events and re-fill it
					ParsedTrace=[parseTrace(ListTrace,Trace)||Trace<-ListTrace],
					et_collector:multicast(CollectorPid,clean_all),%%%%RESET THE VIEWER GRAPHIC CHART
					[showEvent(CollectorPid,Item)||Item<-ParsedTrace],%%FILL VIEWER EVENTS
					showingTrace(CollectorPid,ListTrace)
			end;
		close->
			et_collector:stop(CollectorPid);
		_->
			io:fwrite("I DIDN'T UNDERSTAND THE MESSAGE YOU SENT ME!~n"),
			showingTrace(CollectorPid,LastTrace)
	end.
