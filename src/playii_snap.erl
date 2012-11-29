%%%-------------------------------------------------------------------
%%% File    : playii_snap.erl
%%% Author  : jacky zhao <>
%%% Description : snap the interface
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_snap).

%% API
-export([load/1, save/3, delete/1]).

-include("playii.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec load(Key) -> State | undefined
load(Key) ->
    playii_db:load({backup, Key}).

%% @spec save(Key, State, State0) -> ok
save(Key, State, State0) ->
    if
	State =:= State0 -> ok;
	true -> playii_db:save(#backup{key=Key, value=State})
    end.

%% @spec delete(Key) -> ok
delete(Key) ->
    playii_db:delete({backup, Key}).
