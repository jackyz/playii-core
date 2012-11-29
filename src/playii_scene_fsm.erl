-module(playii_scene_fsm).

-behaviour(gen_fsm).
-export([active/2]).
-export([init/1, handle_event/3, handle_sync_event/4, 
	 handle_info/3, terminate/3, code_change/4]).

-record(state, {
	  scene,               %% 名称
	  eval=playii_js,      %% 脚本执行模块
	  data,                %% 数据
	  lmap=[]              %% 监听日志的player用户
	 }).

-define(timerref, timerref).   %% 超时句柄(取消用)

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test_() ->
    [
     {timeout, 60, fun t2/0},
     {timeout, 60, fun t3/0}
    ].

t1() ->
    {ok, Pid} = gen_fsm:start(?MODULE, ["test_full", playii_js, ["args"]], []),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["test0", "u0"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["test1", "u1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["test2", "u2"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["test3", "u3"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["test4", "u4"]}}),
    %% simple bench mark
    N = 20,
    TA1 = now(),
    lists:map(fun(_) ->
		      gen_fsm:send_all_state_event(
			Pid, {fire, {"say", ["test0", "hi"]}})
 	      end, lists:seq(1, N)),
    TA2 = now(),
    TAX = timer:now_diff(TA2, TA1),
    ?debugFmt("~7.2fms/~p => ~4.2fms once", [TAX/1000, N, TAX/N/1000]),
    gen_fsm:send_all_state_event(Pid, halt),
    ok.

t2() ->
    {ok, Pid} = gen_fsm:start(?MODULE, ["test_half", playii_js, ["god", "/test/half"]], []),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["u1", "p1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"poll",  ["u1", "url1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["u2", "p2"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"poll",  ["u2", "url1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"push",  ["url1","f","args"]}}),
    gen_fsm:send_all_state_event(Pid, halt),
    ok.

t3() ->
    {ok, Pid} = gen_fsm:start(?MODULE, ["test_half", playii_erl, ["god", "/test/half"]], []),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["u1", "p1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"poll",  ["u1", "url1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"enter", ["u2", "p2"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"poll",  ["u2", "url1"]}}),
    gen_fsm:send_all_state_event(Pid, {fire, {"push",  ["url1","f","args"]}}),
    gen_fsm:send_all_state_event(Pid, halt),
    ok.

-endif.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

%% 初始化：加载/初始化脚本，进入 active 状态
init([Scene, Eval, Args]) when is_list(Args) ->
    %% process_flag(trap_exit, true),
    State = case ?snap:load(Scene) of
		undefined ->
		    State0 = #state{scene=Scene, eval=Eval},
		    State1 = load(State0), %% load code
		    State2 = init(State1, Args), %% init data
		    ?snap:save(Scene, State2, State0),
		    ?log_info("[~p] init(~p, ~p) spawn", [Scene, Eval, Args]),
		    State2;
		State0 -> %% recover from snap
		    State0 = load(State0), %% init code
		    ?log_info("[~p] init(~p) restore", [Scene, Args]),
		    State0
	     end,
    reset_timer(),
    {ok, active, State}.

%% 收到异步消息
%% 执行事件
handle_event({fire, Event}, StateName, #state{scene=Scene}=State) ->
    %%?debugFmt("[~p] fire(~p)", [Scene, Event]),
    State1 = eval(State, Event),
    ?snap:save(Scene, State1, State),
    reset_timer(),
    {next_state, StateName, State1};
%% 正常退出，清理现场
handle_event(halt, _StateName, #state{scene=Scene}=State) ->
    %%?debugFmt("[~p] halt", [Scene]),
    ?snap:delete(Scene),
    {stop, normal, State};
%% 非正常退出，不清理现场
handle_event(stop, _StateName, #state{scene=Scene}=State) ->
    %%?debugFmt("[~p] stop", [Scene]),
    {stop, normal, State}.

%% 收到同步消息
handle_sync_event(_Event, _From, StateName, State) ->
    {next_state, StateName, State}.

%% 其他事件
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% 进程退出
terminate(Reason, StateName, #state{scene=Scene}=State) ->
    ?log_info("[~p] terminate(~p) on ~p", [Scene, Reason, StateName]),
    unload(State),
    ok.

%% 代码更新
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% 在 active 状态下触发超时
active({timeout,_,_}, #state{scene=Scene}=State) ->
    State1 = eval(State, {"idle", []}),
    ?snap:save(Scene, State1, State),
    reset_timer(),
    {next_state, active, State1}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%---- timer

reset_timer() ->
    case get(?timerref) of
	undefined -> ok;
	TimerRef -> gen_fsm:cancel_timer(TimerRef)
    end,
    TimerRef2 = gen_fsm:start_timer(?SceneIdle, "TimeOut"),
    put(?timerref, TimerRef2).

%%---- logic

load(#state{scene=Scene,eval=Eval}=State) ->
    Eval:open(),
    {ok, Cmds, Logs} = Eval:load(Scene),
    perform(State, Cmds, Logs),
    State.
    
init(#state{scene=Scene,eval=Eval}=State, Args) ->
    {ok, Data, Cmds, Logs} = Eval:init(Scene, Args),
    State2 = State#state{data=Data},
    perform(State2, Cmds, Logs),
    State2.

eval(#state{scene=Scene,lmap=Lmap}=State, {"enter_debug", [Player]}=E) ->
    %%?log_debug("[~p] trap <- ~p | ~p", [Scene, E, State]),
    case lists:member(Player, Lmap) of
	false -> State#state{lmap= Lmap ++ [Player]};
	_ -> State
    end;
eval(#state{scene=Scene,lmap=Lmap}=State, {"leave_debug", [Player]}=E) ->
    %%?log_debug("[~p] trap <- ~p | ~p", [Scene, E, State]),
    case lists:member(Player, Lmap) of
	true -> State#state{lmap= Lmap -- [Player]};
	_ -> State
    end;
eval(#state{scene=Scene,eval=Eval,data=Data}=State, {Func, Args}=E) ->
    %%?log_debug("[~p] pass <- ~p | ~p", [Scene, E, State]),
    {ok, Data2, Cmds, Logs} = Eval:exec(Scene, Data, Func, Args),
    State2 = State#state{data=Data2},
    perform(State2, Cmds, Logs),
    State2.

unload(#state{scene=Scene,eval=Eval}=State) ->
    Eval:unload(Scene),
    Eval:close(),
    State.

%%---- command execute

perform(#state{scene=Scene, lmap=Lmap}=State, Cmds, Logs) ->
    [?log_debug("~p", [Cmd]) || Cmd <- Cmds],
    [?log_debug("~s", [Log]) || Log <- Logs],
    [ cmd(Cmd, State) || Cmd <- Cmds ],
    [ log(Player, Scene, Logs) || Player <- Lmap ],
    ok.


cmd(["spawn", Scene2, Args], #state{scene=Scene}=_State) ->
    %%?debugFmt("[~p] cmd spawn (~p, ~p)", [Scene, Scene2, Args]),
    playii_scene:open(Scene2, [Scene | Args]);
cmd(["async", Scene2, Func, Args], #state{scene=Scene}=_State) ->
    %%?debugFmt("[~p] cmd async (~p, ~p, ~p)", [Scene, Scene2, Func, Args]),
    playii_scene:fire(Scene2, {Func, [Scene | Args]});
cmd(["cast", Player, Func, Args], #state{scene=Scene}=_State) ->
    %%?debugFmt("[~p] cmd cast (~p, ~p, ~p)", [Scene, Player, Func, Args]),
    playii_player:cast(Player, Scene, {Func, Args});
cmd(["endup"], #state{scene=_Scene}=_State) ->
    %%?debugFmt("[~p] cmd endup ()", [Scene]),
    self_halt();
cmd(Any, #state{scene=Scene}=_State) ->
    ?log_error("[~p] cmd unknown ~p", [Scene, Any]).

log(Player, Scene, Logs) ->
    case playii_player:cast(Player, Scene, {"debug", Logs}) of
	ok -> ok;
	_ -> playii_scene:fire(self(), {"leave_debug", [Player]})
    end.

%%---- local command

self_halt() ->
    gen_fsm:send_all_state_event(self(), halt).

