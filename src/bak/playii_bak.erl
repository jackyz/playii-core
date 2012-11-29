%%%-------------------------------------------------------------------
%%% File    : playii_bak.erl
%%% Author  : jacky zhao <>
%%% Description : 
%%%
%%% Created : 17 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------
-module(playii_bak).

%% API
-export([load/1, save/2, delete/1]).

%%====================================================================
%% Test
%%====================================================================
-include("playii.hrl").

-ifdef(TEST).

do_test() ->
    playii_db:sync(),
    playii_db:clear(backup),
    Key = "1234",
    Value = "test-value",
    save(Key, Value),
    timer:sleep(10),
    R1 = load(Key),
    ?assert(R1 =:= Value),
    delete(Key),
    timer:sleep(10),
    R2 = load(Key),
    ?assert(R2 =:= undefined),
    playii_db:clear(backup),
    ok.

-endif.

%%====================================================================
%% API
%%====================================================================

load(Key) ->
    case playii_db:load({backup, Key}) of
        #backup{value=Val} -> Val;
	_ -> undefined
    end.

save(Key, Value) ->
    spawn(fun() -> playii_db:save(#backup{key=Key, value=Value}) end),
    ok.

delete(Key) ->
    spawn(fun() -> playii_db:delete({backup, Key}) end),
    ok.
		  

%%--------------------------------------------------------------------

%%====================================================================
%% Internal functions
%%====================================================================
