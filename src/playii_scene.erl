%%%-------------------------------------------------------------------
%%% File    : playii_scene.erl
%%% Author  : jacky zhao <>
%%% Description : scene the interface
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_scene).

%% API
-export([open/2, open/3]).
-export([fire/2]).

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test_() ->
    [
     {timeout, 60, fun t2/0}
    ].

t1() ->
    Scene = "test_full",
    {ok, Scene} = open(Scene, ["args"]),
    ok = fire(Scene, {"enter", ["test0"]}),
    ok = fire(Scene, {"enter", ["test1"]}),
    ok = fire(Scene, {"enter", ["test2"]}),
    ok = fire(Scene, {"enter", ["test3"]}),
    ok = fire(Scene, {"enter", ["test4"]}),
    %% simple bench mark
    N = 20,
    TA1 = now(),
    lists:map(fun(_) ->
		      fire(Scene, {"say", ["test0", "hi"]})
 	      end, lists:seq(1, N)),
    TA2 = now(),
    TAX = timer:now_diff(TA2, TA1),
    ?debugFmt("~7.2fms/~p => ~4.2fms once", [TAX/1000, N, TAX/N/1000]),
    ok.

t2() ->
    Scene = "test_half",
    {ok, Scene} = open("test_half", playii_erl, ["god", "/test/half"]),
    ok = fire(Scene, {"enter", ["u1", "p1", "d1"]}),
    ok = fire(Scene, {"poll",  ["u1", "url1"]}),
    ok = fire(Scene, {"enter", ["u2", "p2", "d2"]}),
    ok = fire(Scene, {"poll",  ["u2", "url1"]}),
    ok = fire(Scene, {"push",  ["url1","f","args"]}),
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

%% @spec open(Scene, Args) ->
%%     ok | {error, error()}
%% @doc new scene named Scene
%%     if anything error, {error, error()}
%%     else, ok
open(Scene, Args) ->
    open(Scene, playii_js, Args).

%% @spec open(Scene, Eval, Args) ->
%%     ok | {error, error()}
%% @doc new scene named Scene
%%     if anything error, {error, error()}
%%     else, ok
open(Scene, Eval, Args) when is_list(Scene), is_list(Args) ->
    case playii_cloud:spawn({scene, Scene}, [Scene, Eval, Args]) of
	{ok, Pid} when is_pid(Pid) ->
	    {ok, Scene};
	E ->
	    ?log_error("open(~p, ~p, ~p) fail:~p", [Scene, Eval, Args, E]),
	    {error, E}
    end;
open(Scene, Eval, Args) ->
    ?log_error("open(~p, ~p, ~p) fail:badargs", [Scene, Eval, Args]),
    {error, badargs}.

%% @spec fire(Scene, Event) ->
%%     ok | {error, error()}
%% @doc fire event(Func,Args) to scene(Scene) by player(Who)
%%     if anything error, {error, error()}
%%     else, ok
fire(undefined, _) ->
    {error, scene_not_found};
fire(Scene, Event) when is_list(Scene) ->
    fire(playii_cloud:find({scene, Scene}), Event);
fire(Pid, Event) when is_pid(Pid) ->
    gen_fsm:send_all_state_event(Pid, {fire, Event});
fire(Scene, Event) ->
    ?log_error("fire(~p,~p) fail:badargs", [Scene, Event]),
    {error, badargs}.
