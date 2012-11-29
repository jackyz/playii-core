-module(playii_logger).
%% 职责
%% logger 日志记录者
%%   保存，开启/关闭，调试，查找日志
%% 全局的日志记录器
%%   提供 log
%%   提供 subscribe unsubscribe
%%   提供 debug
%%   提供 list
%%   实现日志的记录和查询 DETS

%% ------------------------ Messages
%% who		scene/player
%% type		in/out/log/err
%% data		the string
%% ------------------------ 组合
%% player.in	某player的输入消息：所有收到的消息
%% player.log	某player的运行日志：如，新建/关闭，加载/保存数据，进入/离开scene，开启/关闭日志等
%% scene.in	某scene的输入消息：所有收到的消息
%% scene.out	某scene的输出消息：所有发出的消息
%% scene.log	某scene的运行日志：如，新建/关闭，脚本输出，运行异常，engine切换等
%% player.out	某player的输出消息：所有发出的消息
%% ------------------------ 匹配
%% "", any			所有
%% "/", any			所有scene消息
%% "/chat/room/", in		/chat/room/* 输入消息
%% "/chat/room/", out		/chat/room/* 输出消息
%% "/chat/room/1234", any	/chat/room/* 所有消息
%% "/chat/room/1234", log	/chat/room/1234 日志消息
%% "/chat/room/1234", err	/chat/room/1234 错误消息
%% "/chat/room/1234", out	/chat/room/1234 输出消息
%% "abckxu", in			abckxu 输入消息
%% "abckxu", out		abckxu 输出消息
%% "abckxu", log		abckxu 日志消息
%% "abckxu", err		abckxu 错误消息
%% "abckxu", any		abckxu 所有消息
%% ------------------------ 存储到DETS
%% ---------------------------------
%% | name | type | datetime | data |
%% ---------------------------------

%% API
%% for player/scene
-export([log/3]).
%% for player
-export([subscribe/3, unsubscribe/3]).
%% for console/debug
-export([watch/2, unwatch/2, match/4]).
%% for supervisor
-export([start_link/0]).

-behaviour(gen_event).
%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

%% -record(state, {map=[{"", any, console}]}).
-record(state, {map=[]}).

%%====================================================================
%% Test
%%====================================================================
-include("playii.hrl").
-ifdef(TEST).

-export([start/0, stop/0]).

do_test() ->
    start(),
    playii_ran:start(),
    %% ?engine:start(),
    playii_fsm_sup:start(?PlayerFsmSup, playii_player),
    playii_fsm_sup:start(?SceneFsmSup, playii_scene),

    N = 4,

    Where = "/chat/room/1",

    ok = playii_scene:ensure(Where),
    %% {new, P} = playii_player:conn(undefined),

    T1 = now(),

    reduce(map(fun() -> a_test(Where) end, N), []),

    subscribe(Where, any, console),
    %% unsubscribe(Where, any, console),
    reduce(map(fun() -> a_test(Where) end, N), []),

    TX = timer:now_diff(now(), T1),
    ?debugVal(TX),
    
    playii_fsm_sup:stop(?SceneFsmSup),
    playii_fsm_sup:stop(?PlayerFsmSup),
    timer:sleep(1000),
    %% ?engine:stop(),
    playii_ran:stop(),
    stop(),
    ok.

a_test(Where) ->
    {new, Who} = playii_player:conn(undefined),
    timer:sleep(100),
    %% ?debugVal(R),
    log(Where, log, "test"),
    %% ?debugVal(R),
    timer:sleep(100),
    {Who, ok}.

map(Func, N) ->
    This = self(),
    lists:map(fun(_) ->
                  spawn(fun() -> This ! {self(), Func()} end)
              end, lists:seq(1, N)).

reduce([], RL) ->
    lists:reverse(RL);
reduce(PL, RL) ->
    receive
        {Pid, Resoult} ->
            reduce([X || X <- PL, X =/= Pid], [Resoult | RL])
    end.

start() ->
    R = gen_event:start({local, ?MODULE}),
    gen_event:add_handler(?MODULE, ?MODULE, []),
    R.

stop() ->
    R = gen_event:stop(?MODULE),
    R.

-endif.

%%====================================================================
%% API
%%====================================================================

%% --------- for export

log(Who, Type, Data) ->
    gen_event:notify(?MODULE, {log, Who, Type, Data}).

subscribe(Who, Type, Player) ->
    gen_event:notify(?MODULE, {subscribe, Who, Type, Player}).

unsubscribe(Who, Type, Player) ->
    gen_event:notify(?MODULE, {unsubscribe, Who, Type, Player}).

watch(Who, Type) ->
    subscribe(Who, Type, console).

unwatch(Who, Type) ->
    unsubscribe(Who, Type, console).

match(Who, Type, Datetime1, Datetime2) ->
    gen_event:call(?MODULE, {match, Who, Type, Datetime1, Datetime2}).

%% --------- for supervisor

%% @spec start_link() ->
%%   {ok, Pid}
start_link() ->
    R = gen_event:start_link({local, ?MODULE}),
    gen_event:add_handler(?MODULE, ?MODULE, []),
    R.

%%====================================================================
%% gen_event callbacks
%%====================================================================

init(_Args)->
    State = #state{},
    {ok, State}.

handle_event({log, Who, Type, Data}, State) ->
    D = {Who, Type, Data},
    [ dispatch(F, D) || F <- State#state.map ],
    {ok, State};
handle_event({subscribe, Who, Type, Player}, State) ->
    F1 = {Who, Type, Player},
    Map1 = [ F || F <- State#state.map, F =/= F1 ],
    Map2 = [F1 | Map1],
    State2 = State#state{map=Map2},
    {ok, State2};
handle_event({unsubscribe, Who, Type, Player}, State) ->
    F1 = {Who, Type, Player},
    Map2 = [ F || F <- State#state.map, F =/= F1 ],
    State2 = State#state{map=Map2},
    {ok, State2}.

handle_call({match, Who, Type, Datetime1, Datetime2}, State) ->
    Reply = lookup(Who, Type, Datetime1, Datetime2),
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

dispatch({Who, Type, Player}, {DWho, DType, DData}) ->
    R = i_match({Who, DWho}, {Type, DType}),
    %% ?debugVal({dispatch, {Who, Type, Player}, {DWho, DType, DData}, R}),
    case R of
        true -> i_send(Player, DWho, DType, DData);
        _ -> ok
    end,
    i_send(db, DWho, DType, DData).

i_match({"", _DWho}, {any, _DType}) -> true;
i_match({_Who, _DWho}, {Type, DType}) when Type =/= any andalso Type =/= DType -> false;
i_match({Who, DWho}, {_Type, _DType}) when Who =:= DWho -> true;
i_match({Who, DWho}, {_Type, _DType}) ->
    case lists:prefix(Who, DWho) of
        true -> true;
        _ -> false
    end.
    
i_send(db, _DWho, _DType, _DData) ->
    %% TODO save(dets, {DWho, DType, DData});
    ok;
i_send(console, DWho, DType, DData) ->
    io:format("[~p]~p~p~n", [DType, DWho, DData]);
i_send(Player, DWho, _DType, DData) ->
    playii_player:cast(Player, DWho, "server_log", DData).

lookup(_Who, _Type, _Datetime1, _Datetime2) ->
    %% TODO query(dets, Query),
    todo.
