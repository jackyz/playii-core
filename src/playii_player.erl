%%%-------------------------------------------------------------------
%%% File    : playii_player.erl
%%% Author  : jacky zhao <>
%%% Description : player the interface
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_player).

%% API
-export([is_open/1, open/2, take/1, fire/3]).
-export([cast/3]).

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test_() ->
    {timeout, 60, fun t1/0}. %% eunit timeout workaround

t1() ->
    Scene = "test_full",
    {ok, Scene} = playii_scene:open(Scene, ["args"]),
    
    Id = "test0",
    Param = "param",
    
    {ok, Player} = open(Id, Param),
    ?debugFmt("init | ~p", [Player]),
    ?assert(is_list(Player)),

    timer:sleep(500),

    R0 = take(Player),
    ?debugFmt("recv0 | ~p", [R0]),
    ?assert({ok, timeout} =:= R0),
    
    timer:sleep(500),

    F0 = fire(Player, Scene, {"enter", []}),
    ?debugFmt("fire0 | ~p", [F0]),
    %% ?assert({error, scene_not_found} =:= F0),
    ?assert(ok =:= F0),

    timer:sleep(500),

    R1 = take(Player),
    ?debugFmt("recv1 | ~p", [R1]),
    %% ?assert({ok, timeout} =:= R1),
    
    timer:sleep(500),

    F = "say",
    A = ["what"],
    C1 = cast(Player, Scene, {F, A}),
    ?debugFmt("cast1 | ~p", [C1]),
    ?assert(ok =:= C1),

    timer:sleep(500),

    R2 = take(Player),
    ?debugFmt("recv2 | ~p", [R2]),
    ?assert({ok, [{Scene, F, A}]} =:= R2),

    timer:sleep(500),
    
    R3 = take(Player),
    ?debugFmt("recv3 | ~p", [R3]),
    ?assert({ok, timeout} =:= R3),
    
    timer:sleep(500),

    S1 = is_open(Player),
    ?debugFmt("stop1 | ~p", [S1]),
    ?assert(true =:= S1),

    timer:sleep(?PlayerIdle),
    
    S2 = is_open(Player),
    ?debugFmt("stop2 | ~p", [S2]),
    ?assert(false =:= S2),
    
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

%% @spec is_open(Player) -> true | false
%% @doc check if player(Identify) existed
is_open(undefined) -> false;
is_open(Pid) when is_pid(Pid) -> true;
is_open(Player) when is_list(Player) ->
    is_open(playii_cloud:find({player, Player}));
is_open(_) ->
    false.

%% @spec open(Player, Args) ->
%%     {ok, Player} | {error, error()}
%% @doc open for player(Identify)
%%     if success {ok, Player}
%%     else, anything error, {error, error()}
open(Id, Args) when is_list(Id), is_list(Args) ->
    Player = case Id of
		 "guest" -> "p_"++playii_rand:gen_str(6);
		 _ -> "p_"++Id
	     end,
    case playii_cloud:spawn({player, Player}, [Player, Id, Args]) of
	{ok, Pid} when is_pid(Pid) ->
	    {ok, Player};
	E ->
	    ?log_error("new(~p, ~p) fail:~p", [Player, Args, E]),
	    case E of
		{error,{already_started,_}} -> already_started;
		Any -> Any
	    end
    end;
open(Id, Args) ->
    ?log_error("open(~p, ~p) fail:badargs", [Id, Args]),
    {error, badargs}.

%% @spec take(Player) ->
%%     {ok, Data} | {ok, timeout} |
%%     {error, {tcp_closed, Socket}} | {error, multi_connect} |
%%     {error, player_not_found}
%% @doc take the queue of a client from a player(Player)
%%     if player does not exist or died, {error, bardargs}
%%     if take is not permit {error, multi_connect}
%%     if socket closed (set to active once outer this function)
%%     if take some data {ok, Data}
%%     else, anything error, {error, error()}
take(undefined) ->
    {error, player_not_found};
take(Player) when is_list(Player) ->
    take(playii_cloud:find({player, Player}));
take(Pid) when is_pid(Pid) ->
    long_poll(Pid);
take(Player) ->
    ?log_error("take(~p) fail:badargs", [Player]),
    {error, badargs}.

%% @spec fire(Player, Scene, Func, Args) ->
%%     ok | {error, error()}
%% @doc fire event(Func, Args) to scene(Scene) by player(Player)
%%     if anything error, {error, error()}
%%     else, ok
fire(undefined, _, _) ->
    {error, player_not_found};
fire(Player, Scene, Event) when is_list(Player) ->
    fire(playii_cloud:find({player, Player}), Scene, Event);
fire(Pid, Scene, Event) when is_pid(Pid), is_list(Scene) ->
    gen_fsm:sync_send_all_state_event(Pid, {fire, Scene, Event});
fire(Player, Scene, Event) ->
    ?log_error("fire(~p,~p,~p) fail:badargs", [Player, Scene, Event]),
    {error, badargs}.

%% @spec cast(Player, Scene, Func, Args) ->
%%     ok | {error, error()}
%% @doc server api s_cast event(Func, Args) to player(Player) by scene(Scene)
%%     if anything error, {error, error()}
%%     else, ok
cast(undefined, _, _) ->
    {error, player_not_found};
cast(Player, Scene, Event) when is_list(Player) ->
    cast(playii_cloud:find({player, Player}), Scene, Event);
cast(Pid, Scene, Event) when is_pid(Pid), is_list(Scene) ->
    gen_fsm:send_all_state_event(Pid, {cast, Scene, Event});
cast(Player, Scene, Event) ->
    ?log_error("cast(~p,~p,~p) fail:badargs", [Player, Scene, Event]),
    {error, badargs}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec long_poll(Pid) ->
%%     {ok, Data} | {ok, timeout} |
%%     {error, {tcp_closed, Socket}} | {error, multi_connect} |
%%     {error, player_not_found}
%% @doc take the queue of a client from a player(Player)
%%     if player does not exist or died, {error, bardargs}
%%     if take is not permit {error, multi_connect}
%%     if socket closed (set to active once outer this function)
%%     if take some data {ok, Data}
%%     else, anything error, {error, error()}
long_poll(Pid) ->
    gen_fsm:send_all_state_event(Pid, {bind, self()}),
    receive
	{ok, Data} -> {ok, Data};
	{error, Error} -> {error, Error};
	{tcp_closed, _} ->
	    gen_fsm:send_all_state_event(Pid, {unbind}),
	    {error, tcp_closed};
	Else -> {error, Else}
    after
	?PlayerWait ->
	    {ok, timeout}
    end.
