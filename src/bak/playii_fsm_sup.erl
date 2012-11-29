-module(playii_fsm_sup).

%% API
-export([start_link/2]).

-behaviour(supervisor).
%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% Test
%%====================================================================
-include("playii.hrl").
-ifdef(TEST).

-export([start/2, stop/1]).

do_test_() ->
    {"test2",
     ?_test(begin
		playii_db:start(),
		{ok, Pid} = start(?PlayerSup, playii_player_fsm),
		
		?debugVal(Pid),
		?debugVal(supervisor:which_children(Pid)),
		?debugVal(supervisor:which_children(?PlayerSup)),
		R = supervisor:start_child(?PlayerSup, ["C12345"]),
		?debugVal(R),
		?debugVal(supervisor:which_children(?PlayerSup)),
		
		%stop(Pid),
		playii_db:stop()
	    end)}.

start(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, [Module]).

stop(Pid) when is_pid(Pid) ->
    exit(Pid, shutdown);
stop(Reg) when is_atom(Reg) ->
    case whereis(Reg) of
	Pid when is_pid(Pid) ->
	    stop(Pid);
	Any ->
	    ?debugVal({Reg, Any}),
	    ok
    end.
	    

-endif.

%%====================================================================
%% API
%%====================================================================

start_link(Name, Module) ->
    supervisor:start_link({local, Name}, ?MODULE, [Module]).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([Module]) ->
    ?debugVal({init, Module}),
    {ok, {{simple_one_for_one, 10, 1},
          [{undefined, {Module, start_link, []},
            temporary, 100, worker, [Module]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
