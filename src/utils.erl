%%%-------------------------------------------------------------------
%%% @doc Utils functions for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(utils).
-export([exist_link/3,fwd_propag/2,deliver_signal/3,bwd_propag/2,check_bwd_propag/2,
          is_not_a_signal_message/3,is_signal_msg_top/2,fundef_lookup/2, fundef_rename/1, substitute/2,
         build_var/1, build_var/2, pid_exists/2,
         select_proc/2, select_msg/2,select_signal/2,
         select_proc_with_time/2, select_proc_with_send/2,
         select_proc_with_spawn/2, select_proc_with_rec/2,
         select_proc_with_var/2, list_from_core/1,
         update_env/2, merge_env/2,
         replace/3, replace_all/2, pp_system/2, pp_trace/1, pp_roll_log/1,
         moduleNames/1,
         stringToFunName/1,stringToCoreArgs/1, toCore/1, toErlang/1,
         filter_options/2, filter_procs_opts/1,
         has_fwd/1, has_bwd/1, has_norm/1, has_var/2,
         is_queue_minus_msg/3, topmost_rec/1, last_msg_rest/1,
         gen_log_send/4, gen_log_spawn/2,gen_log_exit/5,gen_log_unlink/3,gen_log_spawn_link/2,gen_log_propag/2,empty_log/1, must_focus_log/1,
         extract_replay_data/1, extract_pid_log_data/2, get_mod_name/1]).

-include("cauder.hrl").
-include_lib("wx/include/wx.hrl").

%%--------------------------------------------------------------------
%% @doc Searches a function definition in FunDefs with name FunName
%% @end
%%--------------------------------------------------------------------
fundef_lookup(FunName, FunDefs) ->
    %io:fwrite("---------------~n"),
    %io:write(FunName),
    %io:fwrite("~n---------------~n"),
    %io:write(FunDefs), 
    %io:fwrite("~n---------------~n"), 
  case lists:keyfind(FunName, 1, FunDefs) of
      {_, FunDef} -> FunDef;
      false -> io:fwrite("Funzione non trovata", [])
  end.

%%--------------------------------------------------------------------
%% @doc Renames all the variables in function definition FunDef
%% @end
%%--------------------------------------------------------------------
fundef_rename(FunDef) ->
  FunVars = cerl:fun_vars(FunDef),
  FunBody = cerl:fun_body(FunDef),
  RenamedVars = pars_rename(FunVars),
  {RenamedExp, _} =
    cerl_trees:mapfold(fun (Exp, Acc) ->
                          case cerl:type(Exp) of
                            var ->
                              case cerl:var_name(Exp) of
                                {_FunName, _FunArity} ->
                                  NewExp = Exp,
                                  NewAcc = Acc;
                              OtherName ->
                                case lists:keyfind(Exp, 1, Acc) of
                                  false ->
                                    NewExp = fresh_var(OtherName),
                                    NewAcc = [{Exp,NewExp}] ++ Acc;
                                  {Exp, NewVar} ->
                                    NewExp = NewVar,
                                    NewAcc = Acc
                                end
                              end;
                            _Other ->
                              NewExp = Exp,
                              NewAcc = Acc
                          end,
                          {NewExp, NewAcc}
                        end,
                        RenamedVars,
                        FunBody),
  NewFunDef = cerl:c_fun([NewVar || {_, NewVar} <- RenamedVars], RenamedExp),
  NewFunDef.

pars_rename(Vars) ->
  [{Var, fresh_var(cerl:var_name(Var))} || Var <- Vars].

substitute(SuperExp, Env) ->
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case proplists:get_value(Exp, Env) of
            undefined -> Exp;
            Value -> Value
          end;
        _   -> Exp
      end
    end, SuperExp).
%%--------------------------------------------------------------------
%% @doc Builds a variable from a given number Num
%% @end
%%--------------------------------------------------------------------
build_var(Num) ->
  NumAtom = list_to_atom("k_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

%%------------ It is not working in Erlang 22-----------------------------
%%build_var(Name,Num) ->
%%  NumAtom = list_to_atom(atom_to_list(Name) ++ "_" ++ integer_to_list(Num)),
%%  cerl:c_var(NumAtom).
%%------------------------------------------------------------------------
build_var(Name,Num) ->
  NewName =
    case Name of
      UserVarName when is_atom(UserVarName) ->
        atom_to_list(UserVarName);
      % Core variable names are just numbers in the last update
       CoreVarName ->
         "c" ++ integer_to_list(CoreVarName)
    end,
  NumAtom = list_to_atom(NewName ++ "_" ++ integer_to_list(Num)),
  cerl:c_var(NumAtom).

pid_exists(Procs, Pid) ->
  case [ P || P <- Procs, P#proc.pid == Pid] of
    [] -> false;
    _ -> true
  end.

%%aggiunte luca

exist_link(FromProc,DestProc,half)->
  {FromPid,DestPid}={FromProc#proc.pid,DestProc#proc.pid},
  lists:member(FromPid,DestProc#proc.links) orelse lists:member(DestPid,FromProc#proc.links);
exist_link(FromProc,DestProc,full)->
  {FromPid,DestPid}={FromProc#proc.pid,DestProc#proc.pid},
  lists:member(FromPid,DestProc#proc.links) andalso lists:member(DestPid,FromProc#proc.links).


%%roll back a propagation
bwd_propag({LinkPid,error,Reason,Time},{Signals,From})->
   Signal=#signal{dest=LinkPid,from=From,type=error,reason=Reason,time=Time},
   {lists:delete(Signal,Signals),From};
bwd_propag({LinkPid,normal,Time},{Signals,From})->
   Signal=#signal{dest=LinkPid,from=From,type=normal,time=Time},
   {lists:delete(Signal,Signals),From}.

%%In case of  propagation,check if a all signals are in the GS
check_bwd_propag({LinkPid,error,Reason,Time},{Signals,From,Bool})->
  Signal=#signal{dest=LinkPid,from=From,type=error,reason=Reason,time=Time},
  NewBool=Bool and lists:member(Signal,Signals),
  {Signals,From,NewBool};
check_bwd_propag({LinkPid,normal,Time},{Signals,From,Bool})->
  Signal=#signal{dest=LinkPid,from=From,type=normal,time=Time},
  NewBool=Bool and lists:member(Signal,Signals),
  {Signals,From,NewBool}.

fwd_propag(Link,{Signals,From,HistList,error,Reason})->
    Time = ref_lookup(?FRESH_TIME),
    ref_add(?FRESH_TIME, Time + 1),
    HistSignal={Link,error,Reason,Time},
    Signal=#signal{dest=Link,from=From,type=error,reason=Reason,time=Time},
    {[Signal|Signals],From,[HistSignal|HistList],error,Reason};
fwd_propag(Link,{Signals,From,HistList,normal})->
    Time = ref_lookup(?FRESH_TIME),
    ref_add(?FRESH_TIME, Time + 1),
    HistSignal={Link,normal,Time},
    Signal=#signal{dest=Link,from=From,type=normal,time=Time},
    {[Signal|Signals],From,[HistSignal|HistList],normal}.

select_signal(Signals,Time)->
  [Signal] = [ M || M <- Signals, M#signal.time == Time],
  RestSignals = [ M || M <- Signals, M#signal.time /= Time],
  {Signal, RestSignals}.

deliver_signal(_,Proc,#signal{from=From,type=error,reason=Reason,time=Time})->
  #proc{pid=Pid,flag=Flag,links=Links,hist=Hist,env=Env,exp=Exp,mail=Mail}=Proc,
  NewProc=case cerl:concrete(Flag) of
      true->
        Msg=cerl:abstract({'EXIT',cerl:concrete(From),Reason}),
        NewMail = Mail ++ [{Msg,Time}],
        NewLinks=lists:delete(From,Links),
        NewHist=[{signal,From,error,Reason,Time}|Hist],
        Proc#proc{links=NewLinks,hist=NewHist,mail = NewMail};
      false->
        NewExp=cerl:abstract({error,Reason,stack}),
        NewLinks=lists:delete(From,Links),
        NewHist=[{signal,From,error,Env,Exp,Mail,Time}|Hist],
        #proc{pid=Pid,links=NewLinks,hist=NewHist,exp=NewExp}
  end,
  NewProc;
deliver_signal(_,Proc,#signal{from=From,type=normal,time=Time})->
  #proc{flag=Flag,links=Links,hist=Hist,mail=Mail}=Proc,
  NewProc=case cerl:concrete(Flag) of
      true->
        Msg=cerl:abstract({'EXIT',cerl:concrete(From),normal}),
        NewMail = Mail ++ [{Msg,Time}],
        NewLinks=lists:delete(From,Links),
        NewHist=[{signal,From,true,normal,Time}|Hist],
        Proc#proc{links=NewLinks,hist=NewHist,mail = NewMail};
      false->
        NewLinks=lists:delete(From,Links),
        NewHist=[{signal,From,false,normal,Time}|Hist],
        Proc#proc{links=NewLinks,hist=NewHist}
  end,
  NewProc;
deliver_signal(_,Proc,#signal{from=From,type=killer,time=Time})->
  #proc{hist=Hist,env=Env,exp=Exp,mail=Mail}=Proc,
  NewExp=cerl:abstract({exit,killed}),
  NewHist=[{signal,From,killed,Env,Exp,Mail,Time}|Hist],
  Proc#proc{hist=NewHist,exp=NewExp};
deliver_signal(_,Proc,#signal{from=From,type=unlink,time=Time})->
  #proc{pid=Pid,links=Links,hist=Hist,env=Env,exp=Exp}=Proc,
  case lists:member(From,Links) of
    true->
      NewHist=[{signal,unlink,exist,Env,Exp,From,Time}|Hist],
      Proc#proc{hist=NewHist,links=Links--[From]};
    false->
      NewHist=[{signal,unlink,no_exist,Env,Exp,From,Time}|Hist],
      Proc#proc{hist=NewHist}
  end.


is_not_a_signal_message(Msg,[SignalHist|RestHist],Bool)when (element(1,SignalHist))==signal->
  {_,TimeMsg}=Msg,
  Time=element(tuple_size(SignalHist),SignalHist),%prendo l'ultimo elemento della tupla
  case TimeMsg == Time of
      true->
        is_not_a_signal_message(Msg,RestHist,Bool and false);
      false->
        is_not_a_signal_message(Msg,RestHist,Bool and true)
  end;
is_not_a_signal_message(Msg,[_|RestHist],Bool)->
  is_not_a_signal_message(Msg,RestHist,Bool and true);
is_not_a_signal_message(_,[],Bool)->Bool.
    
is_signal_msg_top({signal,From,error,Reason,Time},#proc{mail=Mail})->
  MsgValue=cerl:abstract({'EXIT',cerl:concrete(From),Reason}),
  Msg={MsgValue,Time},
  lists:last(Mail)==Msg;
is_signal_msg_top({signal,From,true,normal,Time},#proc{mail=Mail})->
  MsgValue=cerl:abstract({'EXIT',cerl:concrete(From),normal}),
  Msg={MsgValue,Time},
  lists:last(Mail)==Msg.
%%fine aggiunte luca

%%--------------------------------------------------------------------
%% @doc Returns a tuple with a process with pid Pid from Procs and
%% the rest of processes from Procs
%% @end
%%--------------------------------------------------------------------
select_proc(Procs, Pid) ->
  [Proc] = [ P || P <- Procs, P#proc.pid == Pid],
  RestProcs = [ P || P <- Procs, P#proc.pid /= Pid],
  {Proc, RestProcs}.

%%--------------------------------------------------------------------
%% @doc Returns a tuple with a message with id Time from Msgs and
%% the rest of messages from Msgs
%% @end
%%--------------------------------------------------------------------
select_msg(Msgs, Time) ->
  [Msg] = [ M || M <- Msgs, M#msg.time == Time],
  RestMsgs = [ M || M <- Msgs, M#msg.time /= Time],
  {Msg, RestMsgs}.

%%--------------------------------------------------------------------
%% @doc Returns the process that contains a message with id Time
%% from Procs
%% @end
%%--------------------------------------------------------------------
select_proc_with_time(Procs, Time) ->
  ProcWithTime =
    lists:filter( fun (Proc) ->
                    Mail = Proc#proc.mail,
                    length([ ok || {_,MsgTime} <- Mail, MsgTime == Time]) > 0
                  end, Procs),
  hd(ProcWithTime).

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a send item in history
%% with time Time
%% @end
%%--------------------------------------------------------------------
select_proc_with_send(Procs, Time) ->
  ProcWithSend =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_send(Hist, Time)
                  end, Procs),
  ProcWithSend.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------
select_proc_with_spawn(Procs, Pid) ->
  ProcWithSpawn =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_spawn(Hist, Pid)
                  end, Procs),
  ProcWithSpawn.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a spawn item in history
%% with pid Pid
%% @end
%%--------------------------------------------------------------------
select_proc_with_rec(Procs, Time) ->
  ProcWithRec =
    lists:filter( fun (Proc) ->
                    Hist = Proc#proc.hist,
                    has_rec(Hist, Time)
                  end, Procs),
  ProcWithRec.

%%--------------------------------------------------------------------
%% @doc Returns the processes that contain a binding for Var in
%% its environment Env
%% @end
%%--------------------------------------------------------------------
select_proc_with_var(Procs, Var) ->
  ProcWithRec =
    lists:filter( fun (Proc) ->
                    Env = Proc#proc.env,
                    has_var(Env, Var)
                  end, Procs),
  ProcWithRec.

%%--------------------------------------------------------------------
%% @doc Transforms a Core Erlang list to a regular list
%% @end
%%--------------------------------------------------------------------
list_from_core(Exp) ->
  case  cerl:is_c_nil(Exp) of
    true -> [];
    false -> [cerl:cons_hd(Exp)|list_from_core(cerl:cons_tl(Exp))]
  end.

%%--------------------------------------------------------------------
%% @doc Update the environment Env with a single binding
%% @end
%%--------------------------------------------------------------------
update_env({Key, Value}, Env) ->
  DelEnv = proplists:delete(Key, Env),
  DelEnv ++ [{Key, Value}].

%%--------------------------------------------------------------------
%% @doc Update the environment Env with multiple bindings
%% @end
%%--------------------------------------------------------------------
merge_env(Env, []) -> Env;
merge_env(Env, [CurBind|RestBind]) ->
  NewEnv = update_env(CurBind, Env),
  merge_env(NewEnv, RestBind).

%%--------------------------------------------------------------------
%% @doc A typical substitution application
%% @end
%%--------------------------------------------------------------------

replace_all([],Exp) -> Exp;
replace_all([{Var,Val}|R],Exp) ->
  %io:format("replace: ~p~n~p~n~p~n",[Var,Val,Exp]),
  NewExp = utils:replace(Var,Val,Exp),
  %io:format("--result: p~n",[NewExp]),
  replace_all(R,NewExp).


%%--------------------------------------------------------------------
%% @doc Replaces a variable Var by SubExp (subexpression) in SuperExp
%% (expression)
%% @end
%%--------------------------------------------------------------------
%replace(Var, SubExp, SuperExp) ->
%  VarName = cerl:var_name(Var),
%  case cerl:type(SuperExp) of
%    var -> case cerl:var_name(SuperExp) of
%             VarName -> SubExp;
%             _Other -> SuperExp
%           end;
%    call -> NewArgs = lists:map(fun (E) -> replace(Var,SubExp,E) end, cerl:call_args(SuperExp)),
%            CallModule = cerl:call_module(SuperExp),
%            CallName = cerl:call_name(SuperExp),
%            cerl:c_call(CallModule,CallName,NewArgs);
%    %_Other -> SuperExp
%    _Other -> cerl_trees:map(
%                fun (Exp) ->
%                  case cerl:type(Exp) of
%                    var ->
%                      case cerl:var_name(Exp) of
%                          VarName -> SubExp;
%                          _Other -> Exp
%                      end;
%                    _Other -> Exp
%                  end
%                end, SuperExp)
%  end.

%%--------------------------------------------------------------------
%% @doc Replaces a variable Var by SubExp (subexpression) in SuperExp
%% (expression)
%% @end
%%--------------------------------------------------------------------
replace(Var, SubExp, SuperExp) ->
  VarName = cerl:var_name(Var),
  cerl_trees:map(
    fun (Exp) ->
      case cerl:type(Exp) of
        var ->
          case cerl:var_name(Exp) of
            VarName -> SubExp;
            _Other -> Exp
          end;
        _Other -> Exp
      end
    end, SuperExp).

%%--------------------------------------------------------------------
%% @doc Pretty-prints a given System
%% @end
%%--------------------------------------------------------------------
pp_system(#sys{msgs = Msgs,signals=Signals, procs = Procs}, Opts) ->
  pp_msgs(Msgs) ++ "\n" ++ pp_signals(Signals) ++ "\n" ++ pp_procs(Procs, Opts).

pp_signals(Signals)->
  SignalsList=[pp_signal(Signal)||Signal<-Signals],
  "GS: [" ++ string:join(SignalsList,",") ++ "]\n".

pp_signal(#signal{dest= DestPid,from=From,type=Type,time=Time})->
  "(" ++ pp(DestPid) ++ ",{" ++pp(From)++ ","++atom_to_list(Type) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ "})".

pp_msgs(Msgs) ->
  MsgsList = [pp_msg(Msg) || Msg <- Msgs],
  "GM: [" ++ string:join(MsgsList,",") ++ "]\n".

pp_msg(#msg{dest = DestPid, val = MsgValue, time = Time}) ->
  "(" ++ pp(DestPid) ++ ",{" ++ pp(MsgValue) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ "})".

pp_procs(Procs, Opts) ->
  SortProcs = lists:sort(fun(P1, P2) -> P1#proc.pid < P2#proc.pid end, Procs),
  ProcsList = [pp_proc(Proc, Opts) || Proc <- SortProcs],
  string:join(ProcsList,"\n").

pp_proc(#proc{pid = Pid,flag=Flag,links=Links,hist = Hist, env = Env, exp = Exp, mail = Mail, spf = Fun}, Opts) ->
  pp_pre(Pid, Fun) ++
  pp_flag(Flag,Opts)++
  pp_links(Links,Opts)++
  pp_mail(Mail, Opts) ++
  pp_hist(Hist, Opts) ++
  pp_env(Env, Exp, Opts)++
  pp(Exp, Opts).

pp_pre(Pid, Fun) ->
  "=============== " ++ pp_pid(Pid) ++ ": " ++ pp_fun(Fun)++ " ===============\n".

pp_pid(Pid) ->
  "Proc. " ++ pp(Pid).

pp_fun(undef) ->
  "";
pp_fun({Name, Arity}) ->
  atom_to_list(Name) ++ "/" ++ integer_to_list(Arity).

pp_flag(Flag,Opts)->
  case proplists:get_value(?PRINT_ENV, Opts) of%%da aggiungere macro PRINT_FLAG???
    false -> "";
    true  -> "PROC_FLAG: { " ++ [{?CAUDER_GREEN, pp(Flag)}] ++ " } \n"
  end.

pp_links(Links, Opts) ->%%da aggiungere macro PRINT_LINKS???
  case proplists:get_value(?PRINT_ENV, Opts) of
  false -> "";
  true  -> 
      StrItems = ["link(" ++ [{?CAUDER_GREEN, pp(Item)}] ++ ")" || Item <-Links],
      "LINKS: [" ++ string:join(StrItems, ",") ++ "]\n"
  end.

pp_env(Env, Exp, Opts) ->
  case proplists:get_value(?PRINT_ENV, Opts) of
    false -> "";
    true  -> "ENV: " ++ pp_env_1(Env, Exp, Opts) ++ "\n"
  end.

pp_env_1(Env, Exp, Opts) ->
  PrintEnv =
    case proplists:get_value(?PRINT_FULL_ENV, Opts) of
      true  -> Env;
      false -> rel_binds(Env, Exp)
    end,
  PairsList = [pp_pair(Var,Val) || {Var,Val} <- PrintEnv],
  "{" ++ string:join(PairsList,", ") ++ "}".

pp_pair(Var,Val) ->
  pp(Var) ++ " -> " ++ pp(Val).

is_conc_item({spawn,_,_,_}) -> true;
is_conc_item({send,_,_,_,_}) -> true;
is_conc_item({rec,_,_,_,_}) -> true;
is_conc_item({spawn_link,_,_,_}) -> true;
is_conc_item({propag,_,_,_,_,_}) -> true;
is_conc_item({signal,_,_,_,_,_,_}) -> true;
is_conc_item({signal,_,_,_,_}) -> true;
is_conc_item({unlink,_,_,_,_,_})->true;
is_conc_item({link,_,_,_,_,_})->true;
is_conc_item({exit,_,_,_,_,_,_})->true;
is_conc_item(_) -> false.

pp_hist(Hist, Opts) ->
  case proplists:get_value(?PRINT_HIST, Opts) of
    false -> "";
    true  -> pp_hist_1(Hist, Opts) ++ "\n"
  end.

pp_hist_1(Hist, Opts) ->
  FiltHist =
    case proplists:get_value(?PRINT_FULL, Opts) of
      false -> lists:filter(fun is_conc_item/1, Hist);
      true  -> Hist
    end,
  StrItems = [pp_hist_2(Item) || Item <- FiltHist],
  "H : [" ++ string:join(StrItems, ",") ++ "]".

pp_hist_2({tau,_,_}) ->
  "seq";
pp_hist_2({self,_,_}) ->
  "self";
pp_hist_2({unlink,_,_,_,DestPid,_})->
  "unlink("++[{?CAUDER_GREEN, pp(DestPid)}]++")";
pp_hist_2({link,_,_,_,DestPid,_})->
  "link("++[{?CAUDER_GREEN, pp(DestPid)}]++")";
pp_hist_2({exit,_,_,_,_,Reason,Time})->
  "send_exit_signal("++[{?CAUDER_GREEN, pp(Reason)}]++[{?wxRED, integer_to_list(Time)}]++")";
pp_hist_2({error,_,_,Reason}) ->
  "error("++atom_to_list(Reason)++")";
pp_hist_2({exit,_,_,Reason}) ->
  "exit("++atom_to_list(Reason)++")";
pp_hist_2({spawn,_,_,Pid}) ->
  "spawn(" ++ [{?CAUDER_GREEN, pp(Pid)}] ++ ")";
pp_hist_2({send,_,_,_,{Value,Time}}) ->
  "send(" ++ pp(Value) ++ "," ++ [{?wxRED, integer_to_list(Time)}] ++ ")";
pp_hist_2({rec,_,_,{Value,Time},_}) ->
  "rec(" ++ pp(Value) ++ "," ++ [{?wxBLUE, integer_to_list(Time)}] ++ ")";
pp_hist_2({spawn_link,_,_,Pid}) ->
  "spawn_link(" ++ [{?CAUDER_GREEN, pp(Pid)}] ++ ")";
pp_hist_2({process_flag,_,_,Flag}) ->
  "p_flag(" ++ [{?CAUDER_GREEN, pp(Flag)}] ++ ")";
pp_hist_2({propag,_,_,_,_,HistVal}) ->
  String=lists:foldl(fun stringify/2,"",HistVal),
  [[],LinksString]=string:split(String,","),
  Type=element(2,hd(HistVal)),
  case Type of
    normal->"propag_normal(" ++ [{?CAUDER_GREEN, LinksString}] ++ ")";
    error->
      Reason=element(3,hd(HistVal)),
      "propag_error{"++atom_to_list(Reason)++"(" ++ [{?CAUDER_GREEN, LinksString}] ++ ")}"
    end;

pp_hist_2({signal,unlink,_,_,_,FromPid,Time}) ->
  "got_signal(" ++ [{?CAUDER_GREEN, pp(FromPid)}] ++","++[{?wxRED, integer_to_list(Time)}]++ ")";
pp_hist_2({signal,FromPid,_,_,Time}) ->
  "got_signal(" ++ [{?CAUDER_GREEN, pp(FromPid)}] ++","++[{?wxRED, integer_to_list(Time)}]++")";
pp_hist_2({signal,FromPid,_,_,_,_,Time}) ->
  "got_signal(" ++ [{?CAUDER_GREEN, pp(FromPid)}] ++","++[{?wxRED, integer_to_list(Time)}]++ ")".

stringify(HistLink,Acc)->
  PidString=pp(element(1,HistLink)),
  Acc++","++PidString.

pp_mail(Mail, Opts) ->
  case proplists:get_value(?PRINT_MAIL, Opts) of
    false -> "";
    true  -> "LM: " ++ pp_mail_1(Mail) ++ "\n"
  end.

pp_mail_1([]) -> "[]";
pp_mail_1(Mail) ->
  MailList = [pp_msg_mail(Val, Time) || {Val, Time} <- Mail],
  "[" ++ string:join(MailList,",") ++ "]".

pp_msg_mail(Val, Time) ->
  "{" ++ pp(Val) ++ "," ++  [{?CAUDER_GREEN, integer_to_list(Time)}] ++ "}".


pp(CoreForm, Opts) ->
  case proplists:get_value(?PRINT_EXP, Opts) of
    false -> "";
    true  -> "EXP: " ++ pp(CoreForm) ++ "\n"
  end.

pp(CoreForm) ->lists:flatten(core_pp:format(CoreForm)).

%%--------------------------------------------------------------------
%% @doc Pretty-prints a given system trace
%% @end
%%--------------------------------------------------------------------
pp_trace(#sys{trace = Trace}) ->
  % Trace is built as a stack (newest item is first)
  % and we must reverse it to print it
  RevTrace = lists:reverse(Trace),
  %%%TEST POINT OF LUCA'S TOOL INTEGRATION
  case whereis(tracer) of
    undefined->ok;
    TracerPid->
        TracerPid !{show,RevTrace}
  end,
  %%%
  TraceStr = [pp_trace_item(Item) || Item <- RevTrace],
  string:join(TraceStr,"\n").

pp_trace_item(#trace{type = Type,
                     from = From,
                     to   = To,
                     val  = Val,
                     time = Time}) ->
  case Type of
    ?RULE_SEND    -> pp_trace_send(From, To, Val, Time);
    ?RULE_SPAWN   -> pp_trace_spawn(From, To);
    ?RULE_SPAWN_LINK   -> pp_trace_spawn_link(From, To);
    ?RULE_RECEIVE -> pp_trace_receive(From, Val, Time);
    ?RULE_PROCESS_FLAG -> pp_trace_flag(From,Val);
    ?RULE_SIGNAL -> pp_trace_signal(From,Val,To,Time);
    ?RULE_PROPAG -> pp_trace_propag(From, To,Val);
    ?RULE_UNLINK -> pp_trace_unlink(From,To,Time);
    ?RULE_LINK -> pp_trace_link(From,To,Time);
    ?RULE_EXIT -> pp_trace_exit(From,To,Val,Time)
  end.

pp_trace_send(From, To, Val, Time) ->
  [pp_pid(From)," sends ",pp(Val)," to ",pp_pid(To)," (",integer_to_list(Time),")"].

pp_trace_spawn(From, To) ->
  [pp_pid(From)," spawns ",pp_pid(To)].

pp_trace_spawn_link(From, To) ->
  [pp_pid(From)," spawns and links with ",pp_pid(To)].

pp_trace_flag(From,Val) ->
  [pp_pid(From)," set process flag to ",pp(Val)].

pp_trace_signal(From,{Type,Reason},To,Time) when Type==error ->
  [pp_pid(To)," get signal {"++atom_to_list(Type)++","++atom_to_list(Reason)++"}"++" from " ++pp_pid(From)," (",integer_to_list(Time),")"];
pp_trace_signal(From,{Type,_},To,Time)->
  [pp_pid(To)," get signal {"++atom_to_list(Type)++"}"++" from " ++pp_pid(From)," (",integer_to_list(Time),")"].

pp_trace_propag(From, To,HistVal) ->
  Type=element(2,hd(HistVal)),
  case Type of
    normal->[pp_pid(From)," propagated {"++atom_to_list(Type)++"} to ",pp_links(To)];
    error->
      Reason=element(3,hd(HistVal)),
      [pp_pid(From)," propagated {"++atom_to_list(Type)++","++atom_to_list(Reason)++"} to ",pp_links(To)]
    end.

pp_trace_unlink(From,To,Time)->
  [pp_pid(From)," unlink from ",pp_pid(To)," (",integer_to_list(Time),")"].

pp_trace_link(From,To,Time)->
  [pp_pid(From)," link to ",pp_pid(To)," (",integer_to_list(Time),")"].

pp_trace_exit(From,To,Val,Time)->
  [pp_pid(From)," send exit signal ",pp(Val)," to",pp_pid(To)," (",integer_to_list(Time),")"].

pp_trace_receive(From, Val, Time) ->
  [pp_pid(From)," receives ",pp(Val)," (",integer_to_list(Time),")"].


pp_links(Links)->
  A=fun(Pid,Acc)->Acc++","++pp(Pid) end,
  String=lists:foldl(A,"",Links),
  [[],StringLinks]=string:split(String,","),
  "Procs {"++StringLinks++"}".

%%--------------------------------------------------------------------
%% @doc Prints a given system roll log
%% @end
%%--------------------------------------------------------------------
pp_roll_log(#sys{roll = RollLog}) ->
  string:join(RollLog,"\n").

%%--------------------------------------------------------------------
%% @doc Returns the module names from Forms
%% @end
%%--------------------------------------------------------------------
moduleNames(Forms) ->
  FunDefs = cerl:module_defs(Forms),
  FunNames = [cerl:var_name(Var) || {Var,_Fun} <- FunDefs],
  FunNameStrings = [funNameToString({Name,Arity}) || {Name,Arity} <- FunNames, Name =/= 'module_info'],
  FunNameStrings.

funNameToString({Name,Arity}) ->
  atom_to_list(Name) ++ "/" ++ integer_to_list(Arity).

%%--------------------------------------------------------------------
%% @doc Converts a string String into a Core Erlang function name
%% @end
%%--------------------------------------------------------------------
stringToFunName(String) ->
  FunParts = string:tokens(String, "/"),
  Name = list_to_atom(lists:nth(1,FunParts)),
  Arity = list_to_integer(lists:nth(2,FunParts)),
  cerl:c_var({Name,Arity}).

%%--------------------------------------------------------------------
%% @doc Parses a string Str that represents a list of arguments
%% and transforms these arguments to their equivalent in Core Erlang
%% @end
%%--------------------------------------------------------------------
stringToCoreArgs([]) ->
  [];
stringToCoreArgs(Str) ->
  StrDot = Str ++ ".",
  {ok, ParsedStr, _} = erl_scan:string(StrDot),
  {ok, Exprs} = erl_parse:parse_exprs(ParsedStr),
  CoreExprs = [toCore(Expr) || Expr <- Exprs],
  CoreExprs.

%%--------------------------------------------------------------------
%% @doc Transforms an Erlang expression Expr to its equivalent in
%% Core Erlang
%% @end
%%--------------------------------------------------------------------
toCore(Expr) ->
  case Expr of
    {atom, _, Atom} ->
      cerl:c_atom(Atom);
    {integer, _, Int} ->
      cerl:c_int(Int);
    {op, _, '-',{integer, _, Int}} ->
      cerl:c_int(-Int);
    {float, _, Float} ->
      cerl:c_float(Float);
    {string, _, String} ->
      cerl:c_string(String);
    {tuple, _, TupleEs} ->
      cerl:c_tuple_skel([toCore(E) || E <- TupleEs]);
    {cons, _, Head, Tail} ->
      cerl:c_cons_skel(toCore(Head), toCore(Tail));
    {nil, _} ->
      cerl:c_nil()
  end.

toErlang(Expr) ->
  LitExpr =
    case cerl:is_literal(Expr) of
      true -> Expr;
      false -> cerl:fold_literal(Expr)
    end,
  cerl:concrete(LitExpr).

%%--------------------------------------------------------------------
%% @doc Filters the options with identifier Id
%% @end
%%--------------------------------------------------------------------
filter_options([], _) -> [];
filter_options([CurOpt|RestOpts], Id) ->
  #opt{id = OptId} = CurOpt,
  case (OptId == Id) of
    true -> [CurOpt|filter_options(RestOpts,Id)];
    false -> filter_options(RestOpts,Id)
  end.

%%--------------------------------------------------------------------
%% @doc Filters the process options from a list of Options
%% @end
%%--------------------------------------------------------------------
filter_procs_opts([]) -> [];
filter_procs_opts([CurOpt|RestOpts]) ->
  #opt{type = Type} = CurOpt,
  case Type of
    ?TYPE_MSG  -> filter_procs_opts(RestOpts);
    ?TYPE_SIG -> filter_procs_opts(RestOpts);
    ?TYPE_PROC -> [CurOpt|filter_procs_opts(RestOpts)]
  end.

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a forward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_fwd([]) -> false;
has_fwd([#opt{sem = ?FWD_SEM}|_RestOpts]) -> true;
has_fwd([_CurOpt|RestOpts]) -> has_fwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a backward option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_bwd([]) -> false;
has_bwd([#opt{sem = ?BWD_SEM}|_RestOpts]) -> true;
has_bwd([_CurOpt|RestOpts]) -> has_bwd(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if a list of Options has a normalizing option,
%% and false otherwise
%% @end
%%--------------------------------------------------------------------
has_norm([]) -> false;
has_norm([#opt{sem = ?FWD_SEM, rule = Rule}|RestOpts]) ->
  case Rule of
    ?RULE_SCHED -> has_norm(RestOpts);
    _OtherRule -> true
  end;
has_norm([_CurOpt|RestOpts]) -> has_norm(RestOpts).

%%--------------------------------------------------------------------
%% @doc Returns true if Queue\Msg == OtherQueue, and false otherwise
%% @end
%%--------------------------------------------------------------------
is_queue_minus_msg(Queue, Msg, OtherQueue) ->
  ThisQueue = lists:delete(Msg, Queue),
  ThisQueue == OtherQueue.

%%--------------------------------------------------------------------
%% @doc Retrieves the topmost item in a history
%% @end
%%--------------------------------------------------------------------
topmost_rec([]) -> no_rec;
topmost_rec([CurHist|RestHist]) ->
  case CurHist of
    {rec,_,_,_,_} -> CurHist;
    _Other -> topmost_rec(RestHist)
  end.

has_send([], _) -> false;
has_send([{send,_,_,_,{_,Time}}|_], Time) -> true;
has_send([_|RestHist], Time) -> has_send(RestHist, Time).

has_spawn([], _) -> false;
has_spawn([{spawn,_,_,Pid}|_], Pid) -> true;
has_spawn([{spawn_link,_,_,Pid}|_], Pid) -> true;
has_spawn([_|RestHist], Pid) -> has_spawn(RestHist, Pid).

has_rec([], _) -> false;
has_rec([{rec,_,_, {_, Time},_}|_], Time) -> true;
has_rec([_|RestHist], Time) -> has_rec(RestHist, Time).

has_var(Env, Var) ->
  case proplists:get_value(Var, Env) of
    undefined -> false;
    _ -> true
  end.

fresh_var(Name) ->
  VarNum = ref_lookup(?FRESH_VAR),
  ref_add(?FRESH_VAR, VarNum + 1),
  utils:build_var(Name,VarNum).

last_msg_rest(Mail) ->
  LastMsg = lists:last(Mail),
  LenMail = length(Mail),
  RestMail = lists:sublist(Mail,LenMail-1),
  {LastMsg, RestMail}.

rel_binds(Env, Exp) ->
  RelVars = cerl_trees:variables(Exp),
  lists:filter( fun ({Var,_}) ->
                  VarName = cerl:var_name(Var),
                  lists:member(VarName,RelVars)
                end, Env).

gen_log_send(Pid, OtherPid, MsgValue, Time) ->
[["Roll send from ",pp_pid(Pid), " of ",pp(MsgValue), " to ",pp_pid(OtherPid), " (",integer_to_list(Time),")"]].

gen_log_unlink(Pid,LinkPid,Time)->
    [["Roll unlink from ",pp_pid(Pid), " to ",pp_pid(LinkPid), " (",integer_to_list(Time),")"]].

gen_log_exit(Pid,DestPid,Type,_,Time)-> 
  [["Roll exit/2 from ",pp_pid(Pid), " of ",atom_to_list(Type), " to ",pp_pid(DestPid), " (",integer_to_list(Time),")"]].

gen_log_spawn(_Pid, OtherPid) ->
  % [["Roll SPAWN of ",pp_pid(OtherPid)," from ",pp_pid(Pid)]].
  [["Roll spawn of ",pp_pid(OtherPid)]].

gen_log_spawn_link(_Pid, OtherPid) ->
  % [["Roll SPAWN of ",pp_pid(OtherPid)," from ",pp_pid(Pid)]].
  [["Roll spawn_link of ",pp_pid(OtherPid)]].

gen_log_propag(Pid,Signals)->
  OldLinks=[element(1,HistSignal)||HistSignal<-Signals],
  Acc={Pid,""},
  {_,LogSignals}=lists:foldl(fun gen_log_signal/2,Acc,Signals),
  case hd(Signals) of
    {_,error,Reason,_}->
      [[LogSignals++"Roll propagation {error,"++atom_to_list(Reason)++"} to ",pp_links(OldLinks)]];
    _->[[LogSignals++"Roll propagation {normal} to ",pp_links(OldLinks)]]
  end.

gen_log_signal({LinkPid,error,_,_},{Pid,Acc})->
  {Pid,Acc++"Roll signal error from Proc "++pp(Pid)++" to "++pp_pid(LinkPid)++"\n"};
gen_log_signal({LinkPid,normal,_},{Pid,Acc})->
  {Pid,Acc++"Roll signal normal from Proc "++pp(Pid)++" to "++pp_pid(LinkPid)++"\n"}.

empty_log(System) ->
  System#sys{roll = []}.

must_focus_log(System) ->
  Trace = System#sys.roll,
  case Trace of
      [] -> false;
      _  -> true
  end.

parse_replay_info(Line) ->
  Words = string:split(Line, " "),
  case hd(Words) of
    "call" ->
      {call, lists:nth(2, Words)};
    "main_pid" ->
      {pid, lists:nth(2, Words)};
    _ ->
      none
  end.

add_replay_info({pid, Pid}, Data) ->
  Data#replay{main_pid = Pid};
add_replay_info({call, Call}, Data) ->
  NCall = lists:flatten(string:replace(Call, "\n", "")),
  ECall = lists:flatten(string:replace(NCall, "\"", "", all)),
  Data#replay{call = ECall};
add_replay_info(_, Data) ->
  Data.

read_replay_data(File, Data) ->
  case file:read_line(File) of
    eof ->
      Data;
    {ok, Line} ->
      ReplayInfo = parse_replay_info(Line),
      NData = add_replay_info(ReplayInfo, Data),
      read_replay_data(File, NData)
  end.

extract_replay_data(Path) ->
  ReplayData = #replay{log_path = Path},
  ResPath = Path ++ "/trace_result.log",
  {ok, FileHandler} = file:open(ResPath, [read]),
  NReplayData = read_replay_data(FileHandler, ReplayData),
  put(replay_data, NReplayData),
  % io:format("~p~n", [NReplayData]),
  file:close(FileHandler).

parse_proc_data(Line) ->
  Line.

read_replay_proc_data(File, Data) ->
  case file:read_line(File) of
    eof ->
      lists:reverse(Data);
    {ok, Line} ->
      ProcData = parse_proc_data(Line),
      NData = [ProcData | Data],
      read_replay_proc_data(File, NData)
  end.

extract_pid_log_data(Path, Pid) ->
  PidPath = Path ++ "/trace_" ++ Pid ++ ".log",
  {ok, FileHandler} = file:open(PidPath, [read]),
  ReplayProcData = read_replay_proc_data(FileHandler, []),
  file:close(FileHandler),
  ReplayProcData.

get_mod_name(Call) ->
    AExpr =
        case is_list(Call) of
            true ->
                hd(parse_expr(Call++"."));
            false ->
                Call
        end,
    {call,_,{remote,_,{atom,_,ModName},{atom,_,FunName}},Args} = AExpr,
    {ModName,FunName,Args}.

parse_expr(Func) ->
    case erl_scan:string(Func) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, _Term} ->
                    _Term;
                _Err ->
                    {error, parse_error}
            end;
        _Err ->
            {error, parse_error}
    end.

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
