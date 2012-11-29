%%%-------------------------------------------------------------------
%%% File    : playii_snap_mock.erl
%%% Author  : jacky zhao <>
%%% Description : snap the mock, easy to test
%%% Created : 14 Jul 2008 by jacky zhao <>
%%%-------------------------------------------------------------------

-module(playii_snap_mock).

%% API
-export([load/1, save/3, delete/1]).

-include("playii.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec load(Key) -> State | undefined
load(Key) ->
    undefined.

%% @spec save(Key, State, State0) -> ok
save(Key, State, State0) ->
    ok.

%% @spec delete(Key) -> ok
delete(Key) ->
    ok.
