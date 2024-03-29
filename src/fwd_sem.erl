%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).
-export([eval_step/2,eval_signal/2,eval_sched/2,
         eval_opts/1, eval_procs_opts/1,eval_sig_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").

eval_seq(Env,Flag,Exp) ->
  case is_list(Exp) of
    true -> eval_list(Env,Flag,Exp);
    false -> eval_seq_1(Env,Flag,Exp)
  end.

eval_seq_1(Env,Flag,Exp) ->
  case cerl:type(Exp) of
    var ->
      Value = proplists:get_value(Exp, Env),
      {Env,Value,tau};
    cons ->
      ConsHdExp = cerl:cons_hd(Exp),
      ConsTlExp = cerl:cons_tl(Exp),
      case is_exp(cerl:cons_hd(Exp)) of
        true ->
          {NewEnv,NewConsHdExp,Label} = eval_seq(Env,Flag,ConsHdExp),
          NewExp = cerl:c_cons_skel(NewConsHdExp,
                                    ConsTlExp);
        false ->
          {NewEnv,NewConsTlExp,Label} = eval_seq(Env,Flag,ConsTlExp),
          NewExp = cerl:c_cons_skel(ConsHdExp,
                                    NewConsTlExp)
      end,
      {NewEnv,NewExp,Label};
    values ->
      {NewEnv, NewValuesEs, Label} = eval_list(Env,Flag,cerl:values_es(Exp)),
      NewExp = cerl:c_values(NewValuesEs),
      {NewEnv, NewExp, Label};
    tuple ->
      {NewEnv, NewTupleEs, Label} = eval_list(Env,Flag,cerl:tuple_es(Exp)),
      NewExp = cerl:c_tuple_skel(NewTupleEs),
      {NewEnv, NewExp, Label};
    apply -> 
      ApplyArgs = cerl:apply_args(Exp),
      ApplyOp = cerl:apply_op(Exp),
      case is_exp(ApplyArgs) of
        true ->
          {NewEnv,NewApplyArgs,Label} = eval_seq(Env,Flag,ApplyArgs),
          NewExp = cerl:update_c_apply(Exp,
                                       ApplyOp,
                                       NewApplyArgs),
          {NewEnv,NewExp,Label};
        false ->
          FunDefs = ref_lookup(?FUN_DEFS),
          FunDef = utils:fundef_lookup(ApplyOp, FunDefs),
          NewFunDef = utils:fundef_rename(FunDef),
          FunBody = cerl:fun_body(NewFunDef),
          FunArgs = cerl:fun_vars(NewFunDef),
          % standard zip is used here (pretty-printer forces it)
          NewEnv = utils:merge_env(Env, lists:zip(FunArgs,ApplyArgs)),
          {NewEnv,FunBody,tau}
      end;
    'case' ->
      CaseArg = cerl:case_arg(Exp),
      case is_exp(CaseArg) of
        true ->
          {NewEnv,NewCaseArg,Label} = eval_seq(Env,Flag,CaseArg),
          NewExp = cerl:update_c_case(Exp,
                                      NewCaseArg,
                                      cerl:case_clauses(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          %io:format("Env: ~p\n",[Env]),
          %io:format("CaseArg: ~p\n",[CaseArg]),
          CaseClauses = cerl:case_clauses(Exp),
          %io:format("CaseClauses: ~p\n",[CaseClauses]),
          CaseClauses2 = replace_guards(Env,CaseClauses),
          %io:format("CaseClauses2: ~p\n",[CaseClauses2]),
          %CaseClauses3 = init(CaseClauses2),
          CaseArgs =
            case cerl:type(CaseArg) of
              values -> cerl:values_es(CaseArg);
              _ -> [CaseArg]
          end,
          case cerl_clauses:reduce(CaseClauses2,CaseArgs) of
            {true,{Clause,Bindings}} ->
              ClauseBody = cerl:clause_body(Clause),
              NewEnv = utils:merge_env(Env, Bindings),
              {NewEnv,ClauseBody,tau};
            {false,_} ->
              io:fwrite("Error: No matching clause~n") 
          end
      end;
    'let' ->
      LetArg = cerl:let_arg(Exp),
      case is_exp(LetArg) of
        true ->
          {NewEnv,NewLetArg,Label} = eval_seq(Env,Flag,LetArg),
          NewExp = cerl:update_c_let(Exp,
                                     cerl:let_vars(Exp),
                                     NewLetArg,
                                     cerl:let_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          LetVars = cerl:let_vars(Exp),
          LetEnv =
            case cerl:let_arity(Exp) of
              1 -> lists:zip(LetVars,[LetArg]);
              _ ->
                FlatLetArg =
                case cerl:type(LetArg) of
                  values ->
                    cerl:values_es(LetArg);
                  _ -> LetArg
                end,
                lists:zip(LetVars,FlatLetArg)
            end,
          NewEnv = utils:merge_env(Env, LetEnv),
          NewExp = cerl:let_body(Exp),
          {NewEnv,NewExp,tau}
      end;
    call ->
      CallArgs = cerl:call_args(Exp),
      CallModule = cerl:call_module(Exp),
      CallName = cerl:call_name(Exp),

      case is_exp(CallModule) of
        true ->
          {NewEnv,NewCallModule,Label} = eval_seq(Env,Flag,CallModule),
          NewExp = cerl:update_c_call(Exp,
                                      NewCallModule,
                                      CallName,
                                      CallArgs),
          {NewEnv,NewExp,Label};
        false ->
          case is_exp(CallName) of
            true ->
              {NewEnv,NewCallName,Label} = eval_seq(Env,Flag,CallName),
              NewExp = cerl:update_c_call(Exp,
                                          CallModule,
                                          NewCallName,
                                          CallArgs),
              {NewEnv,NewExp,Label};
            false ->
              case is_exp(CallArgs) of
                true ->
                  {NewEnv,NewCallArgs,Label} = eval_list(Env,Flag,CallArgs),
                  NewExp = cerl:update_c_call(Exp,
                                              CallModule,
                                              CallName,
                                              NewCallArgs),
                  {NewEnv,NewExp,Label};
                false ->
                  case {CallModule, CallName} of
                    {{c_literal,_,'erlang'},{c_literal,_,'unlink'}} ->
                      [LinkPid]=CallArgs,
                      {Env,cerl:abstract(true),{unlink,LinkPid}};
                    {{c_literal,_,'erlang'},{c_literal,_,'link'}} ->
                      [LinkPid]=CallArgs,
                      {Env,cerl:abstract(true),{link,LinkPid}};
                    {{c_literal,_,'erlang'},{c_literal,_,'process_flag'}} ->
                      NewFlag=lists:nth(2,CallArgs),
                      {Env,Flag,{process_flag,NewFlag}};
                    {{c_literal,_,'erlang'},{c_literal,_,'error'}} ->
                      Reason=cerl:concrete(hd(CallArgs)),
                      {Env,null,{error,Reason}};
                    {{c_literal,_,'erlang'},{c_literal,_,'exit'}} ->
                      case CallArgs of
                        [CPid,CReason]->{Env,cerl:c_atom(true),{exit,CPid,CReason}};
                        _-> {Env,null,{exit,cerl:concrete(hd(CallArgs))}}
                      end;
                    {{c_literal,_,'erlang'},{c_literal,_,'spawn'}} ->
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
                      FunName = lists:nth(2,CallArgs),
                      FunArgs = utils:list_from_core(lists:nth(3,CallArgs)),
                      {Env,Var,{spawn,{Var,FunName,FunArgs}}};
                    {{c_literal,_,'erlang'},{c_literal,_,'spawn_link'}} ->
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
                      FunName = lists:nth(2,CallArgs),
                      FunArgs = utils:list_from_core(lists:nth(3,CallArgs)),
                      {Env,Var,{spawn_link,{Var,FunName,FunArgs}}};
                    {{c_literal,_,'erlang'},{c_literal, _, 'self'}} ->
                      VarNum = ref_lookup(?FRESH_VAR),
                      ref_add(?FRESH_VAR, VarNum + 1),
                      Var = utils:build_var(VarNum),
                      {Env, Var, {self, Var}};
                    {{c_literal,_,'erlang'},{c_literal, _, '!'}} ->
                      DestPid = lists:nth(1, CallArgs),
                      MsgValue = lists:nth(2, CallArgs),
                      {Env, MsgValue, {send, DestPid, MsgValue}};
                    {{c_literal,_,'timer'},{c_literal,_,'sleep'}} ->
                      NewExp = cerl:c_atom('ok'),
                      {Env, NewExp, tau};
                    _ ->
			  ToggleOpts = utils_gui:toggle_opts(),
			  AddOptimize = proplists:get_value(?COMP_OPT, ToggleOpts),
			  CompOpts =
			      case AddOptimize of
				  true  -> [to_core,binary];
				  false -> [to_core,binary, no_copt]
			      end,
			  Filename = cerl:concrete(CallModule),
			  Path = ets:lookup_element(?GUI_REF,?LAST_PATH,2),
			  File = filename:join(Path,Filename),
			  case compile:file(File, CompOpts) of
			      {ok, _, CoreForms} ->
				  NoAttsCoreForms = cerl:update_c_module(CoreForms,
									 cerl:module_name(CoreForms),
									 cerl:module_exports(CoreForms),
									 [],
									 cerl:module_defs(CoreForms)),
				  Stripper = fun(Tree) -> cerl:set_ann(Tree, []) end,
				  CleanCoreForms = cerl_trees:map(Stripper, NoAttsCoreForms),
				  FunDefs = cerl:module_defs(CleanCoreForms),			  
				  ConcName = cerl:concrete(CallName),
				  %ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
				  %io:fwrite("---------------~n"),
				  %io:write(CallName),
				  %io:fwrite("~n---------------~n"),
				  %FunDef = utils:fundef_lookup(CallName, FunDefs), 
				  FunDef = utils:fundef_lookup(cerl:c_var({ConcName,cerl:call_arity(Exp)}), FunDefs),
				  NewFunDef = utils:fundef_rename(FunDef),
				  FunBody = cerl:fun_body(NewFunDef),
				  FunArgs = cerl:fun_vars(NewFunDef),
						% standard zip is used here (pretty-printer forces it)
				  NewEnv = utils:merge_env(Env, lists:zip(FunArgs,CallArgs)), %ApplyArgs
				  {NewEnv,FunBody,tau};
			      error -> %for builtin
				  ConcModule = cerl:concrete(CallModule), 
				  ConcName = cerl:concrete(CallName),
				  ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
          try
				    ConcExp = apply(ConcModule, ConcName, ConcArgs),
            StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
            {ok, ParsedExp, _} = erl_scan:string(StrExp),
            {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
            CoreExp = hd([utils:toCore(Expr) || Expr <- TypedExp]),
            NewExp = CoreExp,
            {Env, NewExp, tau}
          catch 
              _:Reason->
                {Env,null,{error,Reason}}
          end
			  end
                  end
              end
          end
      end;
    seq ->
      SeqArg = cerl:seq_arg(Exp),
      case is_exp(SeqArg) of
        true ->
          {NewEnv,NewSeqArg,Label} = eval_seq(Env,Flag,SeqArg),
          NewExp = cerl:update_c_seq(Exp,
                                     NewSeqArg,
                                     cerl:seq_body(Exp)),
          {NewEnv,NewExp,Label};
        false ->
          NewExp = cerl:seq_body(Exp),
          {Env,NewExp,tau}
      end;
    'receive' ->
        VarNum = ref_lookup(?FRESH_VAR),
        ref_add(?FRESH_VAR, VarNum + 1),
        Var = utils:build_var(VarNum),
        % SubsExp = utils:substitute(Exp, Env),
        % {Env, Var, {rec, Var, cerl:receive_clauses(SubsExp)}}
        ReceiveClauses = cerl:receive_clauses(Exp),
        %%ReceiveClauses2 = replace_guards(Env,ReceiveClauses),
        {Env, Var, {rec, Var, ReceiveClauses}}
  end.

%init([_X]) -> [];
%nit([A|R]) -> [A|init(R)].

replace_guards(Bindings,Exps) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) -> 
          Guard2 = utils:replace_all(Bindings,Guard),
          Guard3 = eval_guard(Guard2),
          {c_clause,L,Pats,Guard3,Exp}
          %case ReducedGuard of
          %    {value,true} -> {c_clause,L,Pats,true,Exp};
          %    _Other -> {c_clause,L,Pats,ReducedGuard,Exp}
          %end 
        end, Exps).  

  eval_guard(Exp) ->
    case cerl:type(Exp) of
	call ->
	    CallArgs = cerl:call_args(Exp),
	    CallModule = cerl:call_module(Exp),
	    CallName = cerl:call_name(Exp),
	    ConcModule = cerl:concrete(CallModule),
	    ConcName = cerl:concrete(CallName),
	    ConcArgs = [utils:toErlang(Arg) || Arg <- CallArgs],
	    ConcExp = apply(ConcModule, ConcName, ConcArgs),
	    StrExp = lists:flatten(io_lib:format("~p", ([ConcExp]))) ++ ".",
	    %%io:format("ConcModule: ~p\nConcName: ~p\nConcArgs: ~p\nConcExp: ~p\nStrExp: ~p\n",[ConcModule,ConcName,ConcArgs,ConcExp,StrExp]),
	    {ok, ParsedExp, _} = erl_scan:string(StrExp),
	    {ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
	    hd([utils:toCore(Expr) || Expr <- TypedExp]);
	'let' -> 
	    %io:format("1)~w~n",[Exp]),
	    LetArg = cerl:let_arg(Exp),
	    case is_exp(LetArg) of
		true ->
		    NewLetArg=eval_guard(LetArg),
		    NewExp = cerl:update_c_let(Exp,
					       cerl:let_vars(Exp),
					       NewLetArg,
					       cerl:let_body(Exp)),
		    eval_guard(NewExp);
		false ->
		    LetVars = cerl:let_vars(Exp), 
		    LetEnv =
			case cerl:let_arity(Exp) of
			    1 -> lists:zip(LetVars,[LetArg]);
			    _ ->
				FlatLetArg =
				    case cerl:type(LetArg) of
					values ->
					    cerl:values_es(LetArg);
					_ -> LetArg
				    end,
				lists:zip(LetVars,FlatLetArg)
			end,
		    NewExp = cerl:let_body(Exp),
		    %io:format("2)~w~n",[NewExp]),
		    %io:format("2e)~w~n",[LetEnv]),
		    SubstExp=utils:replace_all(LetEnv,NewExp),
		    %io:format("3)~w~n",[SubstExp]),
		    %StrExp = lists:flatten(io_lib:format("~p", ([SubstExp]))) ++ ".",
		    %%%io:format("ConcModule: ~p\nConcName: ~p\nConcArgs: ~p\nConcExp: ~p\nStrExp: ~p\n",[ConcModule,ConcName,ConcArgs,ConcExp,StrExp]),
		    %{ok, ParsedExp, _} = erl_scan:string(StrExp),
		    %{ok, TypedExp} = erl_parse:parse_exprs(ParsedExp),
		    %TypExpr=hd([utils:toCore(Expr) || Expr <- TypedExp]),
		    %FinalExp=eval_guard(TypExpr),
		    %io:format("4)~w~n",[FinalExp]),
		    eval_guard(SubstExp)
		end;
	_Other -> Exp
    end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in process Pid, given System
%% @end
%%--------------------------------------------------------------------
eval_step(System, Pid) ->
  Msgs = System#sys.msgs,
  Signals=System#sys.signals,
  Procs = System#sys.procs,
  Trace = System#sys.trace,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid,flag=Flag,links=Links,hist = Hist,env = Env, exp = Exp, mail = Mail} = Proc,
  NewSystem=
  case is_exp(Exp) of
      false->%%fine codice o uscita anomala,inizio a guardare i link del processo morente
        case cerl:concrete(Exp) of
          {error,Reason,stack}->
            ReasonVal=case Reason of {R,_}->R; _->Reason end,
            Acc={Signals,Pid,[],error,ReasonVal},
            {NewSignals,_,HistVal,Type,_}=lists:foldl(fun utils:fwd_propag/2,Acc,Links);
          {exit,Reason}when Reason/=normal->
            ReasonVal=case Reason of {R,_}->R; _->Reason end,
            Acc={Signals,Pid,[],error,ReasonVal},
            {NewSignals,_,HistVal,Type,_}=lists:foldl(fun utils:fwd_propag/2,Acc,Links);
          {exit,normal}->
            Acc={Signals,Pid,[],normal},
            {NewSignals,_,HistVal,Type}=lists:foldl(fun utils:fwd_propag/2,Acc,Links);
          _->%%terminazione normale del codice
            Acc={Signals,Pid,[],normal},
            {NewSignals,_,HistVal,Type}=lists:foldl(fun utils:fwd_propag/2,Acc,Links)
        end,
        NewProc=Proc#proc{links=[],hist=[{propag,Env,Exp,Mail,Type,HistVal}|Hist]},%"uccidi" il processo,rompendo tutti i link
        TraceItem = #trace{type = ?RULE_PROPAG,from = Pid,to=Links,val=HistVal},
        NewTrace = [TraceItem|Trace],
        System#sys{procs=[NewProc|RestProcs],signals=NewSignals,trace=NewTrace};
      true->
        {NewEnv, NewExp, Label} = eval_seq(Env,Flag,Exp),
        case Label of
        tau ->
          NewProc = Proc#proc{hist = [{tau,Env,Exp}|Hist], env = NewEnv, exp = NewExp},
          System#sys{msgs = Msgs,signals=Signals, procs = [NewProc|RestProcs]};
        {unlink,LinkPid}->
          Time = ref_lookup(?FRESH_TIME),
          ref_add(?FRESH_TIME, Time + 1),
          NewSignal = #signal{dest = LinkPid,from=Pid,type=unlink,time = Time},
          NewProc=case lists:member(LinkPid,Links) of
            true->
              NewHist=[{unlink,exist,Env,Exp,LinkPid,Time}|Hist],
              Proc#proc{hist=NewHist,env=NewEnv,exp=NewExp,links=Links--[LinkPid]};
            false->
              NewHist=[{unlink,no_exist,Env,Exp,LinkPid,Time}|Hist],
              Proc#proc{hist=NewHist,env=NewEnv,exp=NewExp}
          end,
          TraceItem = #trace{type = ?RULE_UNLINK,from = Pid,to=LinkPid,time=Time},
          NewTrace = [TraceItem|Trace],
          System#sys{msgs = Msgs,signals=[NewSignal|Signals], procs = [NewProc|RestProcs],trace=NewTrace};
         {link,LinkPid}->
          Time = ref_lookup(?FRESH_TIME),
          ref_add(?FRESH_TIME, Time + 1),
          {LinkProc,_} = utils:select_proc(RestProcs, LinkPid),
          TraceItem = #trace{type = ?RULE_LINK,from = Pid,to=LinkPid,time=Time},
          NewTrace = [TraceItem|Trace],
          case is_exp(LinkProc#proc.exp) of
              true->%%if the dest proc is alive
                case utils:exist_link(Proc,LinkProc,half) of
                  true->NewHist=[{link,no_link,Env,Exp,LinkPid,Time}|Hist],
                        NewProc=Proc#proc{hist=NewHist,env=NewEnv,exp=NewExp},
                        System#sys{msgs = Msgs,signals=Signals,procs=[NewProc|RestProcs],trace=NewTrace};
                  false->NewHist=[{link,set_link,Env,Exp,LinkPid,Time}|Hist],
                        NewProc=Proc#proc{hist=NewHist,env=NewEnv,exp=NewExp},
                        NewSignal = #signal{dest = LinkPid,from=Pid,type=link,time = Time},
                        System#sys{msgs = Msgs,signals=[NewSignal|Signals],procs=[NewProc|RestProcs],trace=NewTrace}
                end;
              false->%%if the dest in not alive
                  NewHist=[{link,error,Env,Exp,LinkPid,Time}|Hist],
                  NewProc=Proc#proc{hist=NewHist,env=NewEnv,exp=NewExp},
                  NewSignal = #signal{dest =Pid,from=Pid,type=error, reason=noproc,time = Time},
                  System#sys{msgs = Msgs,signals=[NewSignal|Signals],procs=[NewProc|RestProcs],trace=NewTrace}
          end;
        {process_flag,NewFlag}->
          NewHist=[{process_flag,Env,Exp,Flag}|Hist],
          NewProc=Proc#proc{flag=NewFlag,hist=NewHist,env=NewEnv,exp=NewExp},
          TraceItem = #trace{type = ?RULE_PROCESS_FLAG,from = Pid,val=NewFlag},
          NewTrace = [TraceItem|Trace],
          System#sys{msgs = Msgs,signals=Signals,procs=[NewProc|RestProcs], trace = NewTrace};
        {exit,Reason}->
          NewHist=[{exit,Env,Exp,Reason}|Hist],
          NewProc=Proc#proc{hist=NewHist,exp=cerl:abstract({exit,Reason})},
          System#sys{msgs = Msgs,signals=Signals,procs=[NewProc|RestProcs]};
        {exit,DestPid,R}->
          Time = ref_lookup(?FRESH_TIME),
          ref_add(?FRESH_TIME, Time + 1),
          {Type,Reason}=case cerl:concrete(R) of
              normal->{normal,undefined};
              kill->{killer,R};
              _->{error,R}
            end,
          TraceItem = #trace{type = ?RULE_EXIT, from = Pid, to = DestPid, val = R, time = Time},
          NewTrace = [TraceItem|Trace],
          NewSignal = #signal{dest = DestPid,from=Pid,type=Type,reason=Reason,time = Time},
          NewSignals = [NewSignal|Signals],
          NewHist=[{exit,Env,Exp,Type,DestPid,Reason,Time}|Hist],
          NewProc=Proc#proc{hist=NewHist,exp=NewExp},
          System#sys{msgs = Msgs,signals=NewSignals,procs=[NewProc|RestProcs],trace=NewTrace};
        {error,Reason}->
          NewHist=[{error,Env,Exp,Reason}|Hist],
          NewProc=Proc#proc{hist=NewHist,exp=cerl:abstract({error,Reason,stack})},
          System#sys{msgs = Msgs,signals=Signals,procs=[NewProc|RestProcs]};
        {self, Var} ->
          NewHist = [{self, Env, Exp}|Hist],
          RepExp = utils:replace(Var, Pid, NewExp),
          NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = RepExp},
          System#sys{msgs = Msgs, signals=Signals,procs = [NewProc|RestProcs]};
        {send, DestPid, MsgValue} ->
          Time = ref_lookup(?FRESH_TIME),
          ref_add(?FRESH_TIME, Time + 1),
          NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},
          NewMsgs = [NewMsg|Msgs],
          NewHist = [{send, Env, Exp, DestPid, {MsgValue, Time}}|Hist],
          NewProc = Proc#proc{hist = NewHist, env = NewEnv, exp = NewExp},
          TraceItem = #trace{type = ?RULE_SEND, from = Pid, to = DestPid, val = MsgValue, time = Time},
          NewTrace = [TraceItem|Trace],
          System#sys{msgs = NewMsgs,signals=Signals, procs = [NewProc|RestProcs], trace = NewTrace};
        {spawn, {Var, FunName, FunArgs}} ->
          PidNum = ref_lookup(?FRESH_PID),
          ref_add(?FRESH_PID, PidNum + 1),
          SpawnPid = cerl:c_int(PidNum),
          ArgsLen = length(FunArgs),
          FunCall = cerl:c_var({cerl:concrete(FunName), ArgsLen}),
          SpawnProc = #proc{pid = SpawnPid,
                          env = [],
                          exp = cerl:c_apply(FunCall,FunArgs),
                          spf = cerl:var_name(FunCall)},
          NewHist = [{spawn, Env, Exp, SpawnPid}|Hist],
          RepExp = utils:replace(Var, SpawnPid, NewExp),
          NewProc = Proc#proc{hist = NewHist,env = NewEnv, exp = RepExp},
          TraceItem = #trace{type = ?RULE_SPAWN, from = Pid, to = SpawnPid},
          NewTrace = [TraceItem|Trace],
          System#sys{msgs = Msgs,signals=Signals, procs = [NewProc|[SpawnProc|RestProcs]], trace = NewTrace};
        {spawn_link, {Var, FunName, FunArgs}} ->
          PidNum = ref_lookup(?FRESH_PID),
          ref_add(?FRESH_PID, PidNum + 1),
          SpawnPid = cerl:c_int(PidNum),
          ArgsLen = length(FunArgs),
          FunCall = cerl:c_var({cerl:concrete(FunName), ArgsLen}),
          SpawnProc = #proc{pid = SpawnPid,
                          links=[Pid],
                          env = [],
                          exp = cerl:c_apply(FunCall,FunArgs),
                          spf = cerl:var_name(FunCall)},
          NewHist = [{spawn_link, Env, Exp, SpawnPid}|Hist],
          RepExp = utils:replace(Var, SpawnPid, NewExp),
          NewLinks=[SpawnPid|Links],
          NewProc = Proc#proc{hist = NewHist,links=NewLinks,env = NewEnv, exp = RepExp},
          TraceItem = #trace{type = ?RULE_SPAWN_LINK, from = Pid, to = SpawnPid},%%aggiungere la macro ?RULE_SPAWN_LINK
          NewTrace = [TraceItem|Trace],
          System#sys{msgs = Msgs,signals=Signals, procs = [NewProc|[SpawnProc|RestProcs]], trace = NewTrace};
        {rec, Var, ReceiveClauses} ->
          {Bindings, RecExp, ConsMsg, NewMail} = matchrec(ReceiveClauses, Mail, NewEnv),
          UpdatedEnv = utils:merge_env(NewEnv, Bindings),
          RepExp = utils:replace(Var, RecExp, NewExp),
          NewHist = [{rec, Env, Exp, ConsMsg, Mail}|Hist],
          NewProc = Proc#proc{hist = NewHist, env = UpdatedEnv, exp = RepExp, mail = NewMail},
          {MsgValue, Time} = ConsMsg, 
          TraceItem = #trace{type = ?RULE_RECEIVE, from = Pid, val = MsgValue, time = Time},
          NewTrace = [TraceItem|Trace],
          System#sys{msgs = Msgs,signals=Signals, procs = [NewProc|RestProcs], trace = NewTrace}
      end
    end,
  NewSystem.

eval_signal(System,Id)->
  Procs = System#sys.procs,
  Trace=System#sys.trace,
  Signals = System#sys.signals,
  {Signal,RestSignals}=utils:select_signal(Signals,Id),
  #signal{dest=LinkPid,from=From,type=Type,reason=Reason,time=Time}=Signal,
  {Proc,RestProcs}=utils:select_proc(Procs,LinkPid),
  TraceItem = #trace{type = ?RULE_SIGNAL, from = From, val = {Type,Reason},to=LinkPid, time = Time},
  NewTrace = [TraceItem|Trace],
  case utils:deliver_signal(Procs,Proc,Signal) of
    {NewProc,NewLink}->
      {_,OtherProcs}=utils:select_proc(RestProcs,NewLink#proc.pid),
      System#sys{signals=RestSignals,procs=[NewProc|[NewLink|OtherProcs]],trace=NewTrace};
    NewProc->System#sys{signals=RestSignals,procs=[NewProc|RestProcs],trace=NewTrace}
  end.

%%--------------------------------------------------------------------
%% @doc Performs an evaluation step in message Id, given System
%% @end
%%--------------------------------------------------------------------
eval_sched(System, Id) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  {Msg, RestMsgs} = utils:select_msg(Msgs, Id),
  #msg{dest = DestPid, val = Value, time = Id} = Msg,
  {Proc, RestProcs} = utils:select_proc(Procs, DestPid),
  Mail = Proc#proc.mail,
  NewMail = Mail ++ [{Value, Id}],
  NewProc = Proc#proc{mail = NewMail},
  System#sys{msgs = RestMsgs, procs = [NewProc|RestProcs]}.

is_exp([]) -> false;
is_exp(Exp) when is_list(Exp) ->
  lists:any(fun is_exp/1, Exp);
is_exp(Exp) -> 
  case cerl:type(Exp) of
    literal -> false;
    nil -> false;
    cons -> is_exp(cerl:cons_hd(Exp)) or is_exp(cerl:cons_tl(Exp));
    values -> is_exp(cerl:values_es(Exp));
    tuple -> is_exp(cerl:tuple_es(Exp));
    _Other -> true
  end.

eval_list(Env,Hist,[Exp|Exps]) ->
  case is_exp(Exp) of
    true ->
      {NewEnv,NewExp,Label} = eval_seq(Env,Hist,Exp),
      {NewEnv,[NewExp|Exps],Label};
    false ->
      {NewEnv,NewExp,Label} = eval_list(Env,Hist,Exps),
      {NewEnv,[Exp|NewExp],Label}
  end.

matchrec(Clauses, Mail,Env) ->
  matchrec(Clauses, Mail, [],Env).

matchrec(_, [], _, _) ->
  no_match;
matchrec(Clauses, [CurMsg|RestMsgs], AccMsgs, Env) ->
  {MsgValue, _MsgTime} = CurMsg,
  %io:format("matchrec (MsgValue): ~p~n",[MsgValue]),
  %io:format("matchrec (Clauses): ~p~n",[Clauses]),
  %%preprocessing is used to propagate matching bindings to guards
  NewClauses = preprocessing_clauses(Clauses,MsgValue,Env),
  %io:format("matchrec (NewClauses): ~p~n",[NewClauses]),
  case cerl_clauses:reduce(NewClauses, [MsgValue]) of
    {true, {Clause, Bindings}} -> 
      ClauseBody = cerl:clause_body(Clause),
      NewMsgs =  AccMsgs ++ RestMsgs,
      {Bindings, ClauseBody, CurMsg, NewMsgs};
    {false, []} -> 
	  matchrec(Clauses, RestMsgs, AccMsgs ++ [CurMsg],Env);
      {false, [Clause|OtherClauses]} -> io:format("CauDEr: Unsupported pattern, some behaviours may be missed ~n~w~n",[Clause]),
			matchrec(Clauses, RestMsgs, AccMsgs ++ [CurMsg],Env)		
  end.

preprocessing_clauses(Clauses,Msg,Env) ->
  lists:map(fun({c_clause,L,Pats,Guard,Exp}) -> 
  	%io:format("Clauses: ~p~n",[Clauses]),
  	%io:format("match (Pats/[Msg]) ~p~n~p~n",[Pats,[Msg]]),
  	%io:format("--result: ~p~n",[cerl_clauses:match_list(Pats,[Msg])]),
    case cerl_clauses:match_list(Pats,[Msg]) of
      {true,Bindings} -> Guard2 = utils:replace_all(Bindings++Env,Guard),
      					 %io:format("calling eval_guard (Bindings/Guard/Guard2): ~n~p~n~p~n~p~n",[Bindings++Env,Guard,Guard2]),
                         Guard3 = eval_guard(Guard2),
                         {c_clause,L,Pats,Guard3,Exp};
      _ -> {c_clause,L,Pats,Guard,Exp}
    end
  end, Clauses).

%%--------------------------------------------------------------------
%% @doc Gets the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SigOpts=eval_sig_opts(System),
  SchedOpts ++SigOpts++ ProcsOpts.

eval_sig_opts(#sys{signals=[]})->
  [];
eval_sig_opts(#sys{signals=[CurSig|RestSig],procs=Procs})-> 
  DestPid=CurSig#signal.dest,
  DestProcs = [ P || P <- Procs, P#proc.pid == DestPid],
  case DestProcs of
    [] ->eval_sig_opts(#sys{signals = RestSig, procs = Procs});
    [Proc]->
      case is_exp(Proc#proc.exp) of
        true->
          Time = CurSig#signal.time,
          [#opt{sem = ?MODULE, type = ?TYPE_SIG, id = Time, rule = ?RULE_SIGNAL}|eval_sig_opts(#sys{signals = RestSig, procs = Procs})];
        false->eval_sig_opts(#sys{signals = RestSig, procs = Procs})
      end
  end.

eval_sched_opts(#sys{msgs = []}) ->
  [];
eval_sched_opts(#sys{msgs = [CurMsg|RestMsgs], procs = Procs}) ->
  DestPid = CurMsg#msg.dest,
  DestProcs = [ P || P <- Procs, P#proc.pid == DestPid],
  case DestProcs of
    [] ->
      eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs});
    _Other ->
      Time = CurMsg#msg.time,
      [#opt{sem = ?MODULE, type = ?TYPE_MSG, id = Time, rule = ?RULE_SCHED}|eval_sched_opts(#sys{msgs = RestMsgs, procs = Procs})]
  end.

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(#sys{procs = [CurProc|RestProcs]}) ->
  Exp = CurProc#proc.exp,
  Env = CurProc#proc.env,
  Links =CurProc#proc.links,
  Pid = CurProc#proc.pid,
  Mail = CurProc#proc.mail,
  case eval_exp_opt(Exp, Env, Mail) of
    ?NOT_EXP ->
      case cerl:type(Exp)==literal andalso Links/=[] of
        true->
          [#opt{sem = ?MODULE, type = ?TYPE_PROC, id = cerl:concrete(Pid),rule=?RULE_PROPAG}|eval_procs_opts(#sys{procs = RestProcs})];
        false->
          eval_procs_opts(#sys{procs = RestProcs})%%sarebbe solo questa linea senza il case sopra!!!
      end;
    Opt ->
      [Opt#opt{sem = ?MODULE, type = ?TYPE_PROC, id = cerl:concrete(Pid)}|eval_procs_opts(#sys{procs = RestProcs})]
  end.

eval_exp_opt(Exp, Env, Mail) ->
  case is_exp(Exp) of
    false ->
      ?NOT_EXP;
    true ->
      case cerl:type(Exp) of
        var ->
          #opt{rule = ?RULE_SEQ};
        cons ->
          ConsHdExp = cerl:cons_hd(Exp),
          ConsTlExp = cerl:cons_tl(Exp),
          case is_exp(ConsHdExp) of
            true ->
              eval_exp_opt(ConsHdExp, Env, Mail);
            false ->
              case is_exp(ConsTlExp) of
                true ->
                  eval_exp_opt(ConsTlExp, Env, Mail);
                false ->
                  ?NOT_EXP
              end
          end;
        values ->
          eval_exp_list_opt(cerl:values_es(Exp), Env, Mail);
        tuple ->
          eval_exp_list_opt(cerl:tuple_es(Exp), Env, Mail);
        apply ->
          ApplyArgs = cerl:apply_args(Exp),
          case is_exp(ApplyArgs) of
            true ->
              eval_exp_list_opt(ApplyArgs, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        'let' ->
          LetArg = cerl:let_arg(Exp),
          case is_exp(LetArg) of
            true ->
              eval_exp_opt(LetArg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        seq ->
          SeqArg = cerl:seq_arg(Exp),
          case is_exp(SeqArg) of
            true ->
              eval_exp_opt(SeqArg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        'case' ->
          CaseArg = cerl:case_arg(Exp),
          case is_exp(CaseArg) of
            true ->
              eval_exp_opt(CaseArg, Env, Mail);
            false ->
              #opt{rule = ?RULE_SEQ}
          end;
        call ->
          CallModule = cerl:call_module(Exp),
          case is_exp(CallModule) of
            true ->
              eval_exp_opt(CallModule, Env, Mail);
            false ->
              CallName = cerl:call_name(Exp),
              case is_exp(CallName) of
                true ->
                  eval_exp_opt(CallName, Env, Mail);
                false ->
                  CallArgs = cerl:call_args(Exp),
                  case is_exp(CallArgs) of
                    true ->
                      eval_exp_list_opt(CallArgs, Env, Mail);
                    false ->
                      case {CallModule, CallName} of
                        {{c_literal, _, 'erlang'},{c_literal, _, 'process_flag'}} -> #opt{rule = ?RULE_PROCESS_FLAG};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'spawn'}} -> #opt{rule = ?RULE_SPAWN};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'spawn_link'}} -> #opt{rule = ?RULE_SPAWN_LINK};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'self'}} -> #opt{rule = ?RULE_SELF};
                        {{c_literal, _, 'erlang'},{c_literal, _, '!'}} -> #opt{rule = ?RULE_SEND};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'unlink'}} -> #opt{rule = ?RULE_UNLINK};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'link'}} -> #opt{rule = ?RULE_LINK};
                        {{c_literal, _, 'erlang'},{c_literal, _, 'exit'}}when length(CallArgs)==2 -> #opt{rule = ?RULE_EXIT};
                        _ -> #opt{rule = ?RULE_SEQ}
                      end
                  end
              end
          end;
        'receive' ->
          % SubsExp = utils:substitute(Exp, Env),
          % ?LOG("Exp: " ++ ?TO_STRING(Exp) ++ "\n" ++
          %      "SUB: " ++ ?TO_STRING(SubsExp)),
          % ReceiveClauses = cerl:receive_clauses(SubsExp),
          ReceiveClauses = cerl:receive_clauses(Exp),
          case matchrec(ReceiveClauses, Mail, Env) of
            no_match ->
              ?NOT_EXP;
            _Other ->
              #opt{rule = ?RULE_RECEIVE}
          end
      end
  end.

eval_exp_list_opt([], _, _) ->
  ?NOT_EXP;
eval_exp_list_opt([CurExp|RestExp], Env, Mail) ->
  case is_exp(CurExp) of
    true -> eval_exp_opt(CurExp, Env, Mail);
    false -> eval_exp_list_opt(RestExp, Env, Mail)
  end.

ref_add(Id, Ref) ->
    ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
    ets:lookup_element(?APP_REF, Id, 2).
