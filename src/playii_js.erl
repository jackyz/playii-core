%%%-------------------------------------------------------------------
%%% File    : playii_js.erl
%%% Author  : jacky zhao <>
%%% Description : playii_js the javascript evaluator
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_js).

%% API
-export([open/0, load/1, init/2, exec/4, unload/1, close/0]).

-define(port, port).           %% 脚本执行器，暂存在 process dict

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test_() ->
    [
     %% {timeout, 60, fun t1/0},
     {timeout, 60, fun t2/0}
    ].

t1() ->    
    Scene = "test_full",
    ok = open(),
    {ok, _, _} = load(Scene),
    {ok, S0, _, _} = init(Scene, ["args"]),
    {ok, S1, _, _} = exec(Scene, S0, "enter", ["test0"]),
    {ok, S2, _, _} = exec(Scene, S1, "enter", ["test1"]),
    {ok, S3, _, _} = exec(Scene, S2, "enter", ["test2"]),
    {ok, S4, _, _} = exec(Scene, S3, "enter", ["test3"]),
    {ok, S5, _, _} = exec(Scene, S4, "enter", ["test4"]),
    %% simple bench mark
    N = 20,
    TA1 = now(),
    lists:map(fun(_) ->
		      exec(Scene, S5, "say", ["test0", "hi!"])
 	      end, lists:seq(1, N)),
    TA2 = now(),
    TAX = timer:now_diff(TA2, TA1),
    ?debugFmt("~7.2fms/~p => ~4.2fms once", [TAX/1000, N, TAX/N/1000]),
    ok = unload(Scene),
    close().

t2() ->    
    Scene = "test_half",
    ok = open(),
    {ok, _, _} = load(Scene),
    {ok, S0, _, _} = init(Scene, []),
    {ok, S1, _, _} = exec(Scene, S0, "enter", ["u0", "p0"]),
    {ok, S2, _, _} = exec(Scene, S1, "enter", ["u1", "p1"]),
    {ok, S3, _, _} = exec(Scene, S2, "poll", ["u0", "url1"]),
    {ok, S4, _, _} = exec(Scene, S3, "poll", ["u1", "url1"]),
    {ok, S5, _, _} = exec(Scene, S4, "push", ["url1", "f", "a"]),
    ok = unload(Scene),
    close().

-endif.

%%====================================================================
%% API
%%====================================================================

%% @spec open() -> ok | throw error()
open() ->
    Port = playii_js_port:open(),
    put(?port, Port),
    ok.

%% @spec load(Scene) -> {ok, Cmds, Logs} | {error, error()}
load(Scene) ->
    Port = get(?port),
    playii_js_port:load(Port, code_name(Scene)).
    
%% @spec init(Scene, Args) -> {ok, Data, Cmds, Logs} | {error, error()}
init(Scene, Args) ->
    Port = get(?port),
    playii_js_port:init(Port, code_name(Scene), Args).

%% @spec exec(Scene, Data, Func, Args) -> {ok, Data2, Cmds, Logs} | {error, error()}
exec(Scene, Data, Func, Args) ->
    Port = get(?port),
    ?mark(fire),
    R = playii_js_port:exec(Port, code_name(Scene), Data, Func, Args),
    ?pass(fire),
    R.

%% @spec unload(Scene) -> ok
unload(Scene) ->
    ?debugFmt("total exec time :~p", [?time(fire)]),
    ok.

%% @spec close() -> ok
close() ->
    Port = get(?port),
    playii_js_port:close(Port).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%%---- code mapping

code_name(Scene) ->
    %% TODO test here
    A = string:tokens(Scene, "_"),
    if
        length(A) >= 2 ->
            "/"++lists:nth(1,A)++"/"++lists:nth(2,A)++".js";
        true ->
            "/"++Scene++"/main.js"
    end.
