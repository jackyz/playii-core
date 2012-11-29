-module(playii_app).

-behaviour(application).
-export([start/2, stop/1]).

%%====================================================================
%% application callbacks
%%====================================================================

start(_Type, _StartArgs) ->
    playii_sup:start_link().
    
stop(_State) ->
    ok.
