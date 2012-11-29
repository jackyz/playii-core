%%%-------------------------------------------------------------------
%%% File    : playii_cloud.erl
%%% Author  : jacky zhao <>
%%% Description : cloud - the process spawn manager
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_cloud).

%% API
-export([spawn/2, find/1, kill/1]).

-include("playii.hrl").

%%====================================================================
%% Test
%%====================================================================

-ifdef(TEST).

do_test() ->
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

%% @spec spawn(Identify, Args) -> {ok, Pid} | {error, E}
%%   Identify := {player, Id} | {scene, Id}
%% @doc spawn process in cloud
spawn({player,Player}=Any, Args) ->
    on_idle_node(
      player,
      fun(Node) ->
	      rpc:call(Node, gen_fsm, start, 
		       [{global, mk_key(Any)}, playii_player_fsm, Args, []])
      end);
spawn({scene,Scene}=Any, Args) ->
    ?debugFmt("spawn scene :: ~p", [Args]),
    on_idle_node(
      scene,
      fun(Node) ->
	      rpc:call(Node, gen_fsm, start, 
		       [{global, mk_key(Any)}, playii_scene_fsm, Args, []])
      end);
spawn(Any, Args) ->
    ?log_error("spawn(~p,~p) fail: unsupport", [Any, Args]),
    {error, unsupport}.
    
%% @spec find(Identify) -> Pid | undefined
%%   Identify := {player, Id} | {scene, Id}
%% @doc find process pid in cloud
find({player,_}=Any) ->
    global:whereis_name(mk_key(Any));
find({scene,_}=Any) ->
    global:whereis_name(mk_key(Any));
find(Any) ->
    ?log_error("find(~p) fail: unsupport", [Any]),
    {error, unsupport}.

%% @spec kill(Identify) -> no_return
%%   Identify := {player, Id} | {scene, Id}
%% @doc kill process in cloud
kill({player,_}=Any) ->
    gen_fsm:send_all_state_event({global, mk_key(Any)}, stop);
kill({scene,_}=Any) ->
    gen_fsm:send_all_state_event({global, mk_key(Any)}, stop);
kill(Any) ->
    ?log_error("kill(~p) fail: unsupport", [Any]),
    {error, unsupport}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec on_idle_node(Service, Fun) ->
%%   Any | {error, Error}
%%   Fun(Node)                  % found ok, Fun(Node)
%%   {error, reached_limit}     % reached the limits due to the config
%% @doc run fun(Node) when found idle node of service
on_idle_node(Service, Fun) ->
    RegList = global:registered_names(),
    L2 = [ {Node, max_of(Node, Service) - now_of(RegList, Node, Service)}
	   || Node <- [node() | nodes()] ],
    [{N, C} | _] = lists:reverse(lists:keysort(2, L2)),
    if
	C > 0 -> Fun(N);
	true  ->
	    ?log_error("on_idle(~p) fail: reached_limit", 
		       [Service]),
	    {error, reached_limits}
    end.

config_of(Node, player) ->
    Yes = playii_config:get_val(Node, 'player', false),
    Max = playii_config:get_val(Node, 'player.max', 0),
    {Yes, Max};
config_of(Node, scene) ->
    Yes = playii_config:get_val(Node, 'scene', false),
    Max = playii_config:get_val(Node, 'scene.max', 0),
    {Yes, Max}.

%% @spec max_of(Node, Service) ->
%%   int() | 0
%% @doc get max number of process in service on node
max_of(Node, Service) ->
    case config_of(Node, Service) of
        {true, Int} when is_integer(Int) -> Int;
	_ -> 0
    end.

%% @spec now_of(RegList, Node, Service) ->
%%   int() | 0
%% @doc get number of process in service on node
now_of(RegList, Node, player) ->
    L = [ R || R <- RegList, is_key(player, R)],
    count_by_node(L, Node);
now_of(RegList, Node, scene) ->
    L = [ R || R <- RegList, is_key(scene, R)],
    count_by_node(L, Node).

%% @spec count_by_node(RegList, Node) ->
%%   int() | 0
%% @doc get count of nodes in reglist
count_by_node(RegList, Node) ->
    L1 = [ node(global:whereis_name(R)) || R <- RegList ],
    L2 = [ N || N <- L1, N =:= Node ],
    length(L2).

%% @spec mk_key(Any) ->
%%   string()
%% @doc make key of service
mk_key({player, Str}) ->
    "player_" ++ Str;
mk_key({scene, Str}) ->
    "scene_" ++ Str.

%% @spec is_key(Service, Str) ->
%%   true | false
%% @doc check if a str is a key of service
is_key(player, Str) ->
    string:str(Str, "player_") =:= 1;
is_key(scene, Str) ->
    string:str(Str, "scene_") =:= 1.
