-module(playii_sup).

-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-include("playii.hrl").

%%====================================================================
%% API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
    %% empty sup, will dynamic add children in start phase
    {ok,{{one_for_one, 10, 10}, []}}.
