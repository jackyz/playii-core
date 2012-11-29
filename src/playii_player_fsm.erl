-module(playii_player_fsm).

-behaviour(gen_fsm).
-export([lock/2, free/2]).
-export([init/1, handle_event/3, handle_sync_event/4, 
	 handle_info/3, terminate/3, code_change/4]).

-record(state, {
	  player,              %% 标识(游客有标识)
	  id,                  %% 身份(游客身份为guest非游客身份为string())
	  param,               %% 初始化参数
	  queue=[],            %% 消息队列暂存
	  dmap=[]              %% 关联 scene 退出时需清理
	 }).

-define(client, client).       %% 等待消息的 web 进程

-define(timerref, timerref).   %% 超时句柄(取消用)

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test_() ->
    {timeout, 60, fun t1/0}. %% eunit timeout workaround

t1() ->
    Player = "abcdef",
    Scene = "test",
    
    {ok, Pid} = gen_fsm:start(?MODULE, [Player, "test0", "param"], []),
    ?debugFmt("init ok!!!", []),

    timer:sleep(500),

    ok = gen_fsm:send_all_state_event(Pid, {bind, self()}),
    timeout = recv(),
    ?debugFmt("recv0 ok!!!", []),

    timer:sleep(500),

    {error, _} = gen_fsm:sync_send_all_state_event(Pid, {fire, Scene, {"enter", []}}),
    ?debugFmt("fire ok!!!", []),

    timer:sleep(500),

    ok = gen_fsm:send_all_state_event(Pid, {bind, self()}),
    timeout = recv(),
    ?debugFmt("recv1 ok!!!", []),

    timer:sleep(500),

    F = "test",
    A = ["args"],
    ok = gen_fsm:send_all_state_event(Pid, {cast, Scene, {F, A}}),
    ?debugFmt("cast ok!!!", []),

    timer:sleep(500),

    ok = gen_fsm:send_all_state_event(Pid, {bind, self()}),
    {ok, [{Scene, F, A}]} = recv(),
    ?debugFmt("recv2 ok!!!", []),

    timer:sleep(500),

    ok = gen_fsm:send_all_state_event(Pid, {bind, self()}),
    timeout = recv(),
    ?debugFmt("recv3 ok!!!", []),

    timer:sleep(500),

    gen_fsm:send_all_state_event(Pid, stop),
    ?debugFmt("stop ok!!!", []),

    ok.

recv() ->
    R = receive
	    Any -> Any
	after
	    ?PlayerWait -> timeout
	end,
    ?debugVal(R),
    R.

-endif.

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

%% 初始化：设置客户端进程，进入 active 状态
init([Player, Id, Param]) ->
    %% process_flag(trap_exit, true),
    State = case ?snap:load(Player) of
		 undefined ->
		    State0 = #state{},
		    State1 = #state{player=Player,id=Id,param=Param},
		    ?snap:save(Player, State1, State0),
		    ?log_info("[~p] init(~p,~p) spawn", [Player, Id, Param]),
		    State1;
		Any -> %% recover from snap
		    ?log_info("[~p] init(~p,~p) restore", [Player, Id, Param]),
		    Any
	    end,
    unbind(),
    {ok, free, State}.

%% 收到异步消息
%% 绑定客户端，尝试下发消息
handle_event({bind, From}, _StateName, #state{player=Player,queue=Q}=State) ->
    %%?log_debug("[~p] bind(~p) | ~p", [Player, From, State]),
    case bind() of
	undefined -> ok;
	OldFrom ->
	    %%?debugFmt("bind :: drop ~p", [OldFrom]),
	    OldFrom ! {error, reset}
    end,
    bind(From),
    Q2 = notify(Q),
    State1 = State#state{queue=Q2},
    ?snap:save(Player, State1, State),
    StateName2 = case bind() of
		     undefined -> free;
		     _ -> lock
		 end,
    {next_state, StateName2, State1};
%% 释放客户端
handle_event({unbind}, _StateName, #state{player=Player}=State) ->
    %%?log_debug("[~p] unbind()  | ~p", [Player, State]),
    unbind(),
    {next_state, free, State};
%% 收到事件，解析执行事件
handle_event({cast, Scene, Event}, StateName, #state{player=Player}=State) ->
    %%?log_debug("[~p] cast(~p,~p) | ~p", [Player, Scene, Event, State]),
    {State1, _Reply} = down(State, {Scene, Event}),
    ?snap:save(Player,State1,State),
    {next_state, StateName, State1};
%% 非正常退出，不清理现场（可异地恢复）
handle_event(stop, _StateName, State) ->
    {stop, normal, State}.

%% 收到同步消息
%% 向 scene 进程转发事件
handle_sync_event({fire, Scene, Event}, _From, StateName, #state{player=Player}=State) ->
    %%?log_debug("[~p] fire(~p,~p) | ~p", [Player, Scene, Event, State]),
    {State1, Reply} = up(State, {Scene, Event}),
    ?snap:save(Player,State1,State),
    {reply, Reply, StateName, State1}.

%% 其他事件
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

%% 进程退出
terminate(Reason, StateName, #state{player=Player}) ->
    ?log_info("[~p] terminate(~p) on ~p", [Player, Reason, StateName]),
    ok.

%% 代码更新
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%% 在 lock 状态下触发超时
%% 清除 client
lock({timeout,_,_}, #state{player=Player}=State) ->
    %%?log_debug("[~p] timeout :: lock", [Player]),
    unbind(),
    {next_state, free, State}.

%% 在 free 状态下触发超时
%% (清场)退出
free({timeout,_,_}, #state{player=Player,dmap=Dmap}=State) ->
    %%?log_debug("[~p] timeout :: free", [Player]),
    F = fun(Scene) ->
		playii_scene:fire(Scene, {"leave", [Player]})
	end,
    lists:foreach(F, Dmap),
    ?snap:delete(Player),
    {stop, normal, State}.

%%--------------------------------------------------------------------
%% Logic functions
%%--------------------------------------------------------------------

%% 上行
up(#state{player=Player,id=Id,param=Param,dmap=Dmap}=State, 
   {Scene,{"enter",[]}}=E) ->
    %%?log_debug("[~p] trap <- ~p | ~p", [Player, E, State]),
    case lists:member(Scene, Dmap) of
	false ->
	    Data = ?state:load(Id, Scene),
	    Reply = playii_scene:fire(Scene, {"enter", [Player, Param, Data]}),
	    State2 = case Reply of
			 ok -> State#state{dmap= Dmap ++ [Scene]};
			 _  -> State
		     end,
	    {State2, Reply};
	_ ->
	    {State, ok}
    end;
up(#state{player=Player,dmap=Dmap}=State,
   {Scene,{"leave",[]}}=E) ->
    %%?log_debug("[~p] trap <- ~p | ~p", [Player, E, State]),
    case lists:member(Scene, Dmap) of
	true ->
	    State2 = State#state{dmap= Dmap -- [Scene]},
	    Reply = playii_scene:fire(Scene, {"leave", [Player]}),
	    {State2, Reply};
	_ ->
	    {State, ok}
    end;
up(#state{player=Player}=State,
   {Scene,{Func,Args}}=E) ->
    %%?log_debug("[~p] pass <- ~p | ~p", [Player, E, State]),
    Reply = playii_scene:fire(Scene, {Func, [Player | Args]}),
    {State, Reply}.

%% 下行
down(#state{player=Player, id=Id}=State,
     {Scene,{"save",Data}}=E) ->
    %%?log_debug("[~p] trap -> ~p | ~p", [Player, E, State]),
    Reply = ?state:save(Id, Scene, Data),
    {State, Reply};
down(#state{player=Player, queue=Q}=State,
     {Scene,{Func,Args}}=E) ->
    ?log_debug("[~p] pass -> ~p | ~p", [Player, E, State]),
    Q1 = Q++[{Scene,Func,Args}],
    Q2 = notify(Q1),
    State2 = State#state{queue=Q2},
    Reply = ok,
    {State2, Reply}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%---- client

notify(Q) ->
    Pid = bind(),
    Live = islive(Pid),
    if
	is_pid(Pid), Live =:= true, length(Q) > 0 ->
	    Pid ! {ok, Q},
	    %%?debugFmt("notify :: send ~p, ~p", [Pid, Q]),
	    unbind(),
	    [];
	is_pid(Pid), Live =:= false ->
	    %%?debugFmt("notify :: reset ~p", [Pid]),
	    unbind(),
	    Q;
	true ->
	    %%?debugFmt("notify :: noop", []),
	    Q
    end.

%%---- client

bind() ->
    get(?client).

bind(From) ->
    %%?debugFmt("bind(~p)", [From]),
    reset_timer(?PlayerWait),
    put(?client, From).

unbind()->
%%     case bind() of
%% 	undefined -> ok;
%% 	OldFrom ->
%% 	    ?debugFmt("unbind :: drop ~p", [OldFrom])
%%     end,
    %%?debugFmt("unbind()", []),
    reset_timer(?PlayerIdle),
    put(?client, undefined).

islive(Pid) when is_pid(Pid) andalso node(Pid) =:= node() ->
    is_process_alive(Pid);
islive(Pid) when is_pid(Pid) ->
    rpc:call(node(Pid), erlang, is_process_alive, [Pid]);
islive(_) ->
    false.

%%---- timer

reset_timer(Timeout) ->
    case get(?timerref) of
	undefined -> ok;
	TimerRef -> gen_fsm:cancel_timer(TimerRef)
    end,
    TimerRef2 = gen_fsm:start_timer(Timeout, "TimeOut"),
    put(?timerref, TimerRef2).
