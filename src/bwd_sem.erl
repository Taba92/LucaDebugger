%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the backward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(bwd_sem).
-export([eval_step/2, eval_sched/2, eval_opts/1,
         eval_procs_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
eval_step(System, Pid) ->
  Procs = System#sys.procs,
  Signals=System#sys.signals,
  Msgs = System#sys.msgs,
  Trace = System#sys.trace,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid,flag=Flag,links=Links,hist = [CurHist|RestHist],mail=Mail} = Proc,
  case CurHist of
    {tau, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      System#sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {unlink,exist,OldEnv,OldExp,LinkPid,Time}->
      Signal = #signal{dest = LinkPid,from=Pid,type=unlink,time =Time},
      OldSignals=lists:delete(Signal,Signals),
      TraceItem = #trace{type = ?RULE_UNLINK, from = Pid,to=LinkPid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp,links=[LinkPid|Links]},
      System#sys{msgs = Msgs,signals=OldSignals, procs = [OldProc|RestProcs],trace=OldTrace};
    {unlink,no_exist,OldEnv,OldExp,LinkPid,Time}->
      Signal = #signal{dest = LinkPid,from=Pid,type=unlink,time =Time},
      OldSignals=lists:delete(Signal,Signals),
      TraceItem = #trace{type = ?RULE_UNLINK, from = Pid,to=LinkPid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp},
      System#sys{msgs = Msgs,signals=OldSignals, procs = [OldProc|RestProcs],trace=OldTrace};
    {process_flag,OldEnv,OldExp,OldFlag}->
      OldProc=Proc#proc{flag=OldFlag,hist=RestHist,env=OldEnv,exp=OldExp},
      TraceItem = #trace{type = ?RULE_PROCESS_FLAG,from = Pid,val=Flag},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = Msgs,procs=[OldProc|RestProcs],trace=OldTrace};
    {propag,OldEnv,OldExp,OldMail,_,HistSignals}->
      OldLinks=[element(1,HistSignal)||HistSignal<-HistSignals],
      Acc={Signals,Pid},
      {OldSignals,_}=lists:foldl(fun utils:bwd_propag/2,Acc,HistSignals),
      OldProc=Proc#proc{links=OldLinks,hist=RestHist,env=OldEnv,exp=OldExp,mail=OldMail},
      OldTrace = [Track||Track<-Trace,Track#trace.type/=?RULE_PROPAG orelse Track#trace.from/=Pid],
      System#sys{msgs=Msgs,signals=OldSignals,procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,unlink,exist,OldEnv,OldExp,From,Time}->
      OldSignal=#signal{dest=Pid,from=From,type=unlink,time=Time},
      OldProc=Proc#proc{links=[From|Links],hist=RestHist,env=OldEnv,exp=OldExp},
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {unlink,undefined},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
       System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,unlink,no_exist,OldEnv,OldExp,From,Time}->
      OldSignal=#signal{dest=Pid,from=From,type=unlink,time=Time},
      OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp},
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {unlink,undefined},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,From,error,OldEnv,OldExp,OldMail,Time}->
      Reason=element(2,cerl:concrete(Proc#proc.exp)),
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {error,Reason},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldProc=Proc#proc{links=[From|Links],hist=RestHist,env=OldEnv,exp=OldExp,mail=OldMail},
      OldSignal=#signal{dest=Pid,from=From,type=error,reason=Reason,time=Time},
      System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,From,error,Reason,Time}->
      OldMail=[M||M<-Mail,element(2,M)/=Time],
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {error,Reason},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldProc=Proc#proc{links=[From|Links],hist=RestHist,mail=OldMail},
      OldSignal=#signal{dest=Pid,from=From,type=error,reason=Reason,time=Time},
      System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,From,true,normal,Time}->
      OldMail=[M||M<-Mail,element(2,M)/=Time],
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {normal,undefined},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldProc=Proc#proc{links=[From|Links],hist=RestHist,mail=OldMail},
      OldSignal=#signal{dest=Pid,from=From,type=normal,time=Time},
      System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,From,false,normal,Time}->
      OldProc=Proc#proc{links=[From|Links],hist=RestHist},
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {normal,undefined},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldSignal=#signal{dest=Pid,from=From,type=normal,time=Time},
      System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {signal,From,killed,OldEnv,OldExp,OldMail,Time}->
     OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp,mail=OldMail},
      TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {killer,kill},to=Pid,time=Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldSignal=#signal{dest=Pid,from=From,type=killer,reason=cerl:abstract(kill),time=Time},
      System#sys{signals=[OldSignal|Signals],procs=[OldProc|RestProcs],trace=OldTrace};
    {exit,OldEnv,OldExp,_}->
      OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp},
      System#sys{msgs = Msgs,procs=[OldProc|RestProcs]};
    {exit,OldEnv,OldExp,Type,DestPid,Reason,Time}->
      Signal=#signal{dest = DestPid,from=Pid,type=Type,reason=Reason,time = Time},
      TraceItem = #trace{type = ?RULE_EXIT, from = Pid, to = DestPid, val = Reason, time = Time},
      OldTrace = lists:delete(TraceItem, Trace),
      OldSignals=lists:delete(Signal,Signals),
      OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp},
      System#sys{msgs = Msgs,signals=OldSignals,procs=[OldProc|RestProcs],trace=OldTrace};
    {error,OldEnv,OldExp,_}->
      OldProc=Proc#proc{hist=RestHist,env=OldEnv,exp=OldExp},
      System#sys{msgs = Msgs,procs=[OldProc|RestProcs]};
    {self, OldEnv, OldExp} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      System#sys{msgs = Msgs, procs = [OldProc|RestProcs]};
    {send, OldEnv, OldExp, DestPid, {MsgValue, Time}} ->
      {_Msg, RestMsgs} = utils:select_msg(Msgs, Time),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = RestMsgs, procs = [OldProc|RestProcs], trace = OldTrace};
    {spawn, OldEnv, OldExp, SpawnPid} ->
      {_SpawnProc, OldRestProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp},
      TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = Msgs, procs = [OldProc|OldRestProcs], trace = OldTrace};
    {spawn_link, OldEnv, OldExp, SpawnPid} ->
      {_SpawnProc, OldRestProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldLinks=lists:delete(SpawnPid,Links),
      OldProc = Proc#proc{links=OldLinks,hist = RestHist, env = OldEnv, exp = OldExp},
      TraceItem = #trace{type = ?RULE_SPAWN_LINK, from = Pid, to = SpawnPid},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = Msgs, procs = [OldProc|OldRestProcs], trace = OldTrace};
    {rec, OldEnv, OldExp, OldMsg, OldMail} ->
      OldProc = Proc#proc{hist = RestHist, env = OldEnv, exp = OldExp, mail = OldMail},
      {MsgValue, Time} = OldMsg,
      TraceItem = #trace{type = ?RULE_RECEIVE, from = Pid, val = MsgValue, time = Time},
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{msgs = Msgs, procs = [OldProc|RestProcs], trace = OldTrace}
  end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in message Id, given System
%% @end
%%--------------------------------------------------------------------
eval_sched(System, Id) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  Proc = utils:select_proc_with_time(Procs, Id),
  Pid = Proc#proc.pid,
  {_, RestProcs} = utils:select_proc(Procs, Pid),
  Mail = Proc#proc.mail,
  {LastMsg,RestMail} = utils:last_msg_rest(Mail),
  {Value, Id} = LastMsg,
  OldMsg = #msg{dest = Pid, val = Value, time = Id},
  OldProc = Proc#proc{mail = RestMail},
  OldMsgs = [OldMsg|Msgs],
  OldProcs = [OldProc|RestProcs],
  System#sys{msgs = OldMsgs, procs = OldProcs}.

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.

eval_sched_opts(#sys{procs = Procs}) ->
  Opts = [eval_sched_opt(Proc) || Proc <- Procs],
  lists:filter(fun (X) ->
                  case X of
                    ?NULL_OPT -> false;
                    _ -> true
                  end
                end, Opts).

eval_procs_opts(System) ->
  Procs = System#sys.procs,
  Signals=System#sys.signals,
  Msgs = System#sys.msgs,
  ProcPairs = [utils:select_proc(Procs, Proc#proc.pid) || Proc <- Procs ],
  Opts = [eval_proc_opt(#sys{msgs = Msgs,signals=Signals,procs = RestProcs}, Proc) ||  {Proc, RestProcs} <- ProcPairs],
  lists:filter( fun (X) ->
                  case X of
                    ?NULL_OPT -> false;
                    _Other -> true
                  end
                end, Opts).

eval_proc_opt(RestSystem, CurProc) ->
  RestProcs = RestSystem#sys.procs,
  Signals=RestSystem#sys.signals,
  Msgs = RestSystem#sys.msgs,
  Hist = CurProc#proc.hist,
  Rule =
    case Hist of
      [] ->
        ?NULL_RULE;
      [CurHist|_RestHist] ->
        case CurHist of
          {tau,_,_} ->  ?RULE_SEQ;
          {process_flag,_,_,_} ->  ?RULE_PROCESS_FLAG;
          {link,no_link,_,_}->?RULE_SEQ;
          {link,signal,_,_,LinkPid,Time}->
            Signal = #signal{dest = LinkPid,from=CurProc#proc.pid,type=link,time =Time},
            case lists:member(Signal,Signals) of
              true->?RULE_SEQ;
              false->?NULL_RULE
            end;
          {unlink,_,_,_,LinkPid,Time}->
            Signal = #signal{dest = LinkPid,from=CurProc#proc.pid,type=unlink,time =Time},
            case lists:member(Signal,Signals) of
              true->?RULE_UNLINK;
              false->?NULL_RULE
            end;
          {exit,_,_,_}->?RULE_SEQ;
          {exit,_,_,Type,DestPid,Reason,Time}->
            Signal = #signal{dest = DestPid,from=CurProc#proc.pid,type=Type,reason=Reason,time =Time},
            case lists:member(Signal,Signals) of
              true->?RULE_EXIT;
              false->?NULL_RULE
            end;
          {error,_,_,_}->?RULE_SEQ;
          {signal,_,_,_,_,_,_}->?RULE_SIGNAL;
          {signal,From,error,Reason,Time}->%%nel caso di errore e flag=true
            case utils:is_signal_msg_top({signal,From,error,Reason,Time},CurProc) of
              true->?RULE_SIGNAL;
              _->?NULL_RULE
            end;
          {signal,From,true,normal,Time}->%nel caso di uscita normale e flag=true
            case utils:is_signal_msg_top({signal,From,true,normal,Time},CurProc) of
              true->?RULE_SIGNAL;
              _->?NULL_RULE
            end;
          {signal,_,_,_,_}->?RULE_SIGNAL;
          {propag,_,_,_,_,HistSig}->
            Acc={Signals,CurProc#proc.pid,true},
            {_,_,Bool}=lists:foldl(fun utils:check_bwd_propag/2,Acc,HistSig),
            case Bool of
              true->?RULE_PROPAG;
              false->?NULL_RULE
            end;
          {self,_,_} -> ?RULE_SELF;
          {send,_,_, DestPid, {MsgValue, Time}} ->
            MsgList = [ M || M <- Msgs, M#msg.time == Time,
                                        M#msg.dest == DestPid,
                                        M#msg.val == MsgValue ],
            case MsgList of
              [] -> ?NULL_RULE;
              _ -> ?RULE_SEND
            end;
          {spawn,_,_,SpawnPid} ->
            {SpawnProc, _RestProcs} = utils:select_proc(RestProcs, SpawnPid),
            #proc{hist = SpawnHist, mail = SpawnMail} = SpawnProc,
            case {SpawnHist, SpawnMail} of
              {[], []} -> ?RULE_SPAWN;
              _ -> ?NULL_RULE
            end;
          {spawn_link,_,_,SpawnPid} ->
            {SpawnProc, _RestProcs} = utils:select_proc(RestProcs, SpawnPid),
            #proc{hist = SpawnHist, mail = SpawnMail} = SpawnProc,
            case {SpawnHist, SpawnMail} of
              {[], []} -> ?RULE_SPAWN_LINK;
              _ -> ?NULL_RULE
            end;
          {rec,_,_, ConsMsg, OldMail} ->
            Mail = CurProc#proc.mail,
            case utils:is_queue_minus_msg(OldMail, ConsMsg, Mail) of
              true -> ?RULE_RECEIVE;
              false -> ?NULL_RULE
            end
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule ->
      Pid = CurProc#proc.pid,
      #opt{sem = ?MODULE, type = ?TYPE_PROC, id = cerl:concrete(Pid), rule = OtherRule}
  end.

eval_sched_opt(Proc) ->
  #proc{hist = Hist, mail = Mail} = Proc,
  Rule =
    case Mail of
      [] -> ?NULL_RULE;
      _ ->
        {LastMsg,_} = utils:last_msg_rest(Mail),
        case utils:is_not_a_signal_message(LastMsg,Hist,true) of
            true->
              {_,Time} = LastMsg,
              TopRec = utils:topmost_rec(Hist),
              case TopRec of
              no_rec -> {?RULE_SCHED, Time};
                {rec,_,_,OldMsg,OldMail} ->
                case utils:is_queue_minus_msg(OldMail, OldMsg,Mail) of
                  false -> {?RULE_SCHED, Time};
                  true -> ?NULL_RULE
                end
              end;
            _->?NULL_RULE
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    {OtherRule, Id} ->
      #opt{sem = ?MODULE, type = ?TYPE_MSG, id = Id, rule = OtherRule}
  end.
