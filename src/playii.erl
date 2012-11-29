-module(playii).

%% admin
-export([start/0, stop/0, identify/0]).

%% config

-include("playii.hrl").

%%====================================================================
%% API
%%====================================================================

%% admin

start() ->
    ok = application:start(mnesia),
    ok = application:start(playii, permanent),
    ok = crypto:start(), %% start crypto as a basic service
    lists:foreach(fun start/1, [log, db, web, player, scene]).

stop() ->
    application:stop(playii).

identify() ->
    playii_config:identify().

%%====================================================================
%% Internal functions
%%====================================================================

%% start up

start(log) ->
    Log = {log, 
 	   {playii_log_server, start_link, []},
 	   permanent, 5000, worker, [playii_log_server]},
    supervisor:start_child(playii_sup, Log),
    Max = playii_config:get_val(node(), 'player.max', 512),
    ?log_info("Playii Realtime System ~s Version.", [mode(Max)]),
    ?log_info("start log", []);
start(db) ->
    ok = playii_db:sync(),
    ?log_info("start db", []);
start(web) ->
    case playii_config:get_val(node(), 'web', false) of
	true ->
	    Addr = playii_config:get_val(node(), 'web.addr', "0.0.0.0"),
	    Port = playii_config:get_val(node(), 'web.port', 8880),
	    Max  = playii_config:get_val(node(), 'web.max', 2048),
	    DocRoot = playii_deps:local_path(["priv/root"]),
	    Web = {web,
		   {playii_web, start, 
		    [[{ip, Addr},
		      {port, Port},
		      {max, Max},
		      {docroot, DocRoot}
		     ]]},
		   permanent, 5000, worker, dynamic},
	    supervisor:start_child(playii_sup, Web),
	    ?log_info("start web ~p:~p max=~p,docroot=~p",
		   [Addr, Port, Max, DocRoot]);
	_ -> ok
    end;
start(player) ->
    case playii_config:get_val(node(), 'player', false) of
	true ->
	    Ran = {ran, 
		   {playii_rand, start_link, []},
		   permanent, 5000, worker, [playii_rand]},
	    supervisor:start_child(playii_sup, Ran),
	    Max = playii_config:get_val(node(), 'player.max', 512),
	    ?log_info("start player max=~p", [Max]);
	_ -> ok
    end;
start(scene) ->
    case playii_config:get_val(node(), 'scene', false) of
	true ->
	    Max = playii_config:get_val(node(), 'scene.max', 128),
	    ?log_info("start scene max=~p", [Max]);
	_ -> ok
    end;
start(_Any) ->
    {error, badargs}.

%% ----

mode(?EvalMax) -> "Evalation";
mode(_) -> "Release".

